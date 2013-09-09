
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


C $Header$


C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE PHOT_MOD

C-----------------------------------------------------------------------
C
C  FSB This version has NO internal write statements
C  FSB This version has the code for XR96 added.
C  FSB change indices from L to II in newOptics loop 08/17/2006
C  FSB This version has all write statements commented out.(08/03/2006)
C
C  FSB NOTE - this code assumes that the top of the modeling domain
C  is about 100 [mb] or 10 [kPa] ~ 16 [km] in altitude. If a
C  higher altitude top is used , the method of calculating the
C  ozone column and the ozone optical depth will be necessary.
C
C  FSB This version has the addition of Rayleigh optical depth for the
C  stratosphere as well as the calculation of single scattering
C  albedo for the AOD calculation. (01/17/2006)
C  FSB This version has deleted the JPROC values of Cs and Qy as well as
C  the default aerosol.  It also contains the fast optics
C  routines.
C  FSB This module supports the SAPRC99 Chemical mechanism within
C  CMAQ.
C  FSB This version calls a fast optical routine for aerosol
C  extinction and scattering
C  FSB This version uses a set of constant refractive indices
C  The new subroutine GETNEWPAR now sets up the refractive indices.
C  
C  Bill Hutzell(Mar 2011) moved determining refractive indices to a
C  separate file and new subroutine called AERO_PHOTDATA.
C  
C  Bill Hutzell(Jun 2011) modified TWOSTREAM_S subroutine to account for 
C  GAM2 equal to zero in the Toon et al. (1989) solution to the two stream
C  of the radiative transfer equation based on how the NCAR TUV model 
C  implements the approximation
C
C  Bill Hutzell(May 2013) modified optical depth agruments to give vetical
C  profile rather than surface values. Note that TAU_TOT now includes 
C  stratospheric values.
C-----------------------------------------------------------------------

      USE CSQY_DATA

      IMPLICIT NONE

C***include files

      INCLUDE SUBST_CONST      ! physical constants

C***parameters

      REAL, PARAMETER :: SMALL = 1.0E-36    ! a small number

C***Fundamental Constants: ( Source: CRC76, pp 1-1 to 1-6)

      REAL, PARAMETER :: PLANCK_C = 6.62606876E-34 ! Planck's Constant [Js]
      REAL, PARAMETER :: LIGHT_SPEED = 299792458.0 ! speed of light in a vacuum

C***LOGDEV for NEW_OPTICS and supporting routines
      
      INTEGER, SAVE :: NEW_OPTICS_LOG

      INTEGER, PARAMETER :: N_DIAG_WVL = 2          ! number of dianostic wavelengths
      INTEGER, SAVE      :: DIAG_WVL( N_DIAG_WVL )  ! pointers to diagnostic wavelengths
      
C***Information for photolysis

      INTEGER, SAVE :: NWL     ! number of wavelengths
!     INTEGER, PARAMETER  :: NWL_INLINE_METHOD = 7

      INTEGER IWL               ! index use for wavelength
      INTEGER ITT               ! index for temperature
      INTEGER IRRXN

      REAL, ALLOCATABLE, SAVE :: WAVELENGTH( : )  ! effective wavelengths [nm ]

      REAL, ALLOCATABLE, SAVE :: FEXT( : )   ! downward solar direct flux at the top of
                                ! of the Atmosphere.  [ photons / ( cm **2 s) ]
                                ! values from Fast-JX Code
                                ! Version 5.0 (XinZhu & MP Dec 04)
                                ! www.ess.uci.edu/~prather/fastj.html
                                ! only tropospheric values are used.

C***fsb note the earlier version of the following array had
C***  7.33e14 in position 2. The correct value 7.332e14
C***  has been entered 08/02/2007

C***surface albedo

      REAL, ALLOCATABLE, SAVE :: ALB( : )  ! set in subroutine PHOT

C***Cloud albedo values from JPROC

      REAL, ALLOCATABLE, SAVE :: CLOUD_BETA_LWC( : ) ! cloud extinction coef divided by LWC
      REAL, ALLOCATABLE, SAVE :: CLOUD_COALBEDO( : ) ! cloud coalbedo
      REAL, ALLOCATABLE, SAVE :: CLOUD_G( : )        ! cloud asymmetry factor

      INTEGER, SAVE :: NTEMP_STRAT
      REAL, ALLOCATABLE, SAVE :: XO3CS( :,: )       !
      REAL, ALLOCATABLE, SAVE :: TEMP_O3_STRAT( : ) ! temperature for XO3CS, K

C***arrays for reference data for needed photolysis rates

      REAL, ALLOCATABLE, SAVE :: XXCS( :,:,: )  ! absorption cross sections
      REAL, ALLOCATABLE, SAVE :: XXQY( :,:,: )  ! quantum yield

      REAL, ALLOCATABLE, SAVE :: RTEMP_S( :,: )

      CHARACTER(16), ALLOCATABLE, SAVE  :: PHOTOLYSIS_RATE( : ) ! subset of photolysis rates from CSQY DATA


C***Indices for special case photolysis cross sections

      INTEGER, SAVE :: LNO2
      INTEGER, SAVE :: LO3O1D
      INTEGER, SAVE :: LO3O3P
      INTEGER, SAVE :: LACETONE
      INTEGER, SAVE :: LKETONE
      INTEGER, SAVE :: LMGLY_ADJ
      INTEGER, SAVE :: LMGLY_ABS
      INTEGER, SAVE :: LHCHOR_06

      INTEGER, SAVE :: IREFTEMPS  ! number of ref. temperatures

      INTEGER, SAVE :: NUMB_LANDUSE_REF
      INTEGER, SAVE :: INDEX_GRASSLAND_REF
      INTEGER, SAVE :: INDEX_OCEAN_REF
      INTEGER, SAVE :: INDEX_SEA_ICE

      CHARACTER(30), ALLOCATABLE, SAVE :: LANDUSE_REF( : )
      REAL,          ALLOCATABLE, SAVE :: ZENITH_COEFF_REF( : )
      REAL,          ALLOCATABLE, SAVE :: SEASON_COEFF_REF( : )
      REAL,          ALLOCATABLE, SAVE :: SNOW_COEFF_REF( : )
      REAL,          ALLOCATABLE, SAVE :: SPECTRAL_ALBEDO_REF( :,: )

      INTEGER, PARAMETER :: NUMB_EXPECT_NLCD50  = 50
      INTEGER, SAVE      :: NUMB_LANDUSE_NLCD50
      CHARACTER(60), ALLOCATABLE, SAVE :: LANDUSE_NLCD50( : )
      INTEGER,       ALLOCATABLE, SAVE :: ALBMAP_REF2NLCD50( : )
      REAL,          ALLOCATABLE, SAVE :: ALBFAC_REF2NLCD50( : )

      INTEGER, PARAMETER :: NUMB_EXPECT_NLCD40  = 40
      INTEGER, SAVE      :: NUMB_LANDUSE_NLCD40
      CHARACTER(60), ALLOCATABLE, SAVE :: LANDUSE_NLCD40( : )
      INTEGER,       ALLOCATABLE, SAVE :: ALBMAP_REF2NLCD40( : )
      REAL,          ALLOCATABLE, SAVE :: ALBFAC_REF2NLCD40( : )

      INTEGER, PARAMETER :: NUMB_EXPECT_USGS  = 24
      INTEGER, SAVE      :: NUMB_LANDUSE_USGS
      CHARACTER(60), ALLOCATABLE, SAVE :: LANDUSE_USGS( : )
      INTEGER,       ALLOCATABLE, SAVE :: ALBMAP_REF2USGS( : )
      REAL,          ALLOCATABLE, SAVE :: ALBFAC_REF2USGS( : )

      INTEGER, PARAMETER :: NUMB_EXPECT_MODIS = 33
      INTEGER, SAVE      :: NUMB_LANDUSE_MODIS
      CHARACTER(60), ALLOCATABLE, SAVE :: LANDUSE_MODIS( : )
      INTEGER,       ALLOCATABLE, SAVE :: ALBMAP_REF2MODIS( : )
      REAL,          ALLOCATABLE, SAVE :: ALBFAC_REF2MODIS( : )

      LOGICAL, SAVE      :: NO_NLCD40
      LOGICAL, SAVE      :: WRITE_CELL

C***special information for acetone
C***  Reference:
C***     Cameron-Smith, P., Incorporation of non-linear
C***     effective cross section parameterization into a
C***     fast photolysis computation  code (Fast-J)
C***     Journal of Atmospheric Chemistry, Vol. 37,
C***     pp 283-297, 2000.

      INTEGER, PARAMETER :: NWL_ACETONE_FJX = 7

      REAL, SAVE :: OP0( 2, NWL_ACETONE_FJX ) ! variable needed for acetone

      DATA ( OP0( 1, IWL ), IWL = 1, NWL_ACETONE_FJX ) /
     &     2.982E-20, 1.301E-20, 4.321E-21, 1.038E-21,
     &     5.878E-23, 1.529E-25, 0.0/

      DATA ( OP0( 2, IWL ), IWL = 1, NWL_ACETONE_FJX ) /
     &     3.255E-20, 1.476E-20, 5.179E-21, 1.304E-21,
     &     9.619E-23, 2.671E-25, 0.0 /

      REAL, SAVE :: YY30( NWL_ACETONE_FJX )   ! variable needed for acetone

      DATA YY30 / 5.651E-20, 1.595E-19, 2.134E-19,
     &     1.262E-19, 1.306E-19, 1.548E-19, 0.0 /

      REAL :: OPTT                ! variable needed for acetone

      CONTAINS

C///////////////////////////////////////////////////////////////////////
      SUBROUTINE NEW_OPTICS ( NLAYS, NMODE,
     &                        BLKTA, BLKPRS, BLKDENS, BLKZH, BLKZF,
     &                        BLKO3, BLKNO2,
     &                        ZSFC, COSZEN, SINZEN, RSQD,
     &                        LATCR, LONGCR, JDATE, LWC, CLDFRC,
     &                        BLKRJ, ETOT_SFC,
     &                        TAUC_AERO, TAU_TOT, TAUO3_TOP,
     &                        TAU_RAY, SSA, TAU_CLOUD, STOZONE )
C-----------------------------------------------------------------------
C
C  FSB  NOTE new call vector <<<<<<<<<<<<< **********
C
C  FSB This version has clouds
C  FSB calculates the photolysis rates as a function of species and height
C
C  first coded 10/19/2004 by Dr. Francis S. Binkowski
C     Carolina Environmental Program
C     University of North Carolina at Chapel Hill
C     email: frank_binkowski@unc.edu
C  modified by FSB  July 29, 2005, 01/19/2006  by FSB
C
C  Mar 2011 Bill Hutzell
C      -revised arguement to account for aerosol redesign in 
C       CMAQ version 5.0
C      -change array declaration to allow flexible number of
C       wavelength bins 
C  Apr 2012 Bill Hutzell
C      -revised error checking to needed photolysis data 
C      -modified case statement for RACM2 photolysis rates
C      -moved aerosol optics to its own module
C   
C-----------------------------------------------------------------------


      USE UTILIO_DEFN
      USE AEROMET_DATA                   ! parameters for aerosol calculations

      USE AERO_PHOTDATA
      
      IMPLICIT NONE

      INCLUDE SUBST_RXCMMN      ! provides photolysis reaction list

C***arguments

      INTEGER, INTENT(IN) :: NLAYS   ! # of vertical layers
      INTEGER, INTENT(IN) :: NMODE   ! # of aerosol modes
      INTEGER, INTENT(IN) :: JDATE   ! julian date YYYYDDD

      REAL, INTENT(IN) :: BLKPRS ( NLAYS )    ! Air pressure in [ atm ]
      REAL, INTENT(IN) :: BLKTA  ( NLAYS )    ! Air temperature [ K ]
      REAL, INTENT(IN) :: BLKDENS( NLAYS )    ! Air density  [ molecules / cm**3 ]
      REAL, INTENT(IN) :: BLKZH  ( NLAYS )    ! layer half-height [ m ]
      REAL, INTENT(IN) :: BLKZF  ( NLAYS+1 )  ! layer full height[ m ]
      REAL, INTENT(IN) :: BLKO3  ( NLAYS )    ! O3 concentration [ molecules / cm**3 ]
      REAL, INTENT(IN) :: BLKNO2 ( NLAYS )    ! NO2 concentration [ molecules / cm**3 ]
      REAL, INTENT(IN) :: ZSFC                ! surface height (msl) [ m ]
      REAL, INTENT(IN) :: COSZEN, SINZEN      ! sine and cosine of the zenith angle
      REAL, INTENT(IN) :: RSQD                ! square of  solar distance [ au**2 ]
      REAL, INTENT(IN) :: LATCR, LONGCR       ! local latitude and longitude

      REAL, INTENT(IN) :: LWC   ( NLAYS )     ! liquid water content [g/m**3]
      REAL, INTENT(IN) :: CLDFRC( NLAYS )     ! fraction of gridcell covered by cloud

      REAL, INTENT(OUT) :: BLKRJ ( NLAYS, NPHOTAB ) ! photolysis rates [ 1 / sec ]
      REAL, INTENT(OUT) :: ETOT_SFC ( : )   ! total downward irradiance at sfc
                                              ! [ Watts / m**2  ]

      REAL, INTENT(OUT) :: TAUC_AERO( :, : )  ! aerosol optical depth, bottom of layer
      REAL, INTENT(OUT) :: TAU_TOT  ( :, : )  ! total optical depth, bottom of layer
      REAL, INTENT(OUT) :: TAU_CLOUD( :, : )  ! cloud optical depth, bottom of layer

      REAL, INTENT(OUT) :: TAUO3_TOP( : )   ! optical depth of ozone above model domain
      REAL, INTENT(OUT) :: TAU_RAY  ( : )   ! Rayleigh optical depth above model domain
      REAL, INTENT(OUT) :: SSA      ( : )   ! single scatering albedo for aerosol column
      REAL, INTENT(OUT) :: STOZONE          ! total ozone above the top of the model
                                            ! ignoring tropospheric ozone [ DU ]

C***internal

      REAL, PARAMETER :: STRAT_TEMP = 225.0  ! stratospheric temperature
      REAL, PARAMETER :: ZTOA = 50.0E3  ! top of the atmosphere [ m ]

      INTEGER L, I, IWL , II, ILEV, IPHOT, MODE ! loop indices
      INTEGER NPHOT

      INTEGER NLEVEL
      REAL BLKDZ( NLAYS )       ! layer thicknesses [ m ]
      REAL DSDH ( NLAYS )       ! slant path function
      REAL FLXSCALE             ! scaling factor for downward solar
                                ! flux at the top of the model.

      REAL ZTOM                 ! top of model  [ m ]
      REAL DSDH_TOP             ! slantpath function from ZTOM to ZTOA
      REAL TAU_O3               ! optical depth of stratospheric ozone [ m ]
      REAL DENSTOM              ! estimated air density at top of model
                                ! [ molecules / cm**3 ]
      REAL LAMDA                ! wavelength  [ nm ]
      REAL INV_LAMBDA           ! reciprocal of wavelength [ 1/nm ]
      REAL LAMDA_UM             ! wavelength  [ um ]

C***working absorption cross sections [ cm**2 ]. These have been corrected
C***  for ambient ( pressure and temperature ) conditions.

      REAL AO3
      REAL ANO2

      REAL DGN_CORE   ! geometric mean diameters of aerosol core
      REAL VOL        ! Total Modal aerosol volume
      REAL VOL_CORE   ! Modal aerosol volume of aerosol core
      REAL DGN_SHELL  ! geometric mean diameters of aerosol core
      REAL VOL_SHELL  ! Modal aerosol volume of aerosol core
      REAL XXLSG      ! geometric standard deviation

C***indices
C***  refractive indices for layer and each mode

      REAL NREAL( NMODE, NLAYS )
      REAL NIMAG( NMODE, NLAYS )

      COMPLEX NR_SHELL
      COMPLEX NI_SHELL
      COMPLEX NR_CORE
      COMPLEX NI_CORE

      REAL BETA_M    ! molecular scattering coefficient [ 1/m ]
      REAL BEXT_MODE  ! total aerosol extinction coefficient [ 1/m ]
      REAL BSCAT_MODE ! total aerosol scattering coefficient [ 1/m ]
      REAL GBAR_MODE  ! total aerosol asymmetry factor
      REAL BEXT       ! total aerosol extinction coefficient [ 1/m ]
      REAL VFAC, BSC  ! unit correction factors
      REAL BSCAT      ! total aerosol scattering coefficient [ 1/m ]
      REAL G_BAR      ! total aerosol asymmetry factor

C***FSB The following variable is aq switch that allows a fast version of
C***  aerosol optics to be used when set to .TRUE.

C***scattering and absorption for the layer

      REAL DTABS_A, DTABS_M, DTSCAT_A, DTSCAT_M, DTSCAT, DTABS

C***scratch variables

      REAL DJ, DF
      REAL DSDH_TD( NLAYS+1 )     ! slant path function from top down

C***Increment of optical depth

      REAL DTAU    ( NLAYS+1 )    ! total depeth at level
      REAL DT_AERO ( NLAYS+1 )    ! aerosol contribution at level
      REAL DT_CLOUD( NLAYS+1 )    ! cloud contribution  at level

C***single scattering albedo for layer

      REAL OM( NLAYS+1 )

C***asymmetry factor

      REAL G( NLAYS+1 )

C***arrays for fluxes and irradiances used in

C***delta-Eddington code

      REAL FDIR( NLAYS+1 )        ! direct actinic flux
      REAL FUP ( NLAYS+1 )        ! diffuse upward actinic flux
      REAL FDN ( NLAYS+1 )        ! diffuse downward flux
      REAL EDIR( NLAYS+1 )        ! direct irradiance
      REAL EUP ( NLAYS+1 )        ! diffuse upward irradiance
      REAL EDN ( NLAYS+1 )        ! diffuse downward irradiance

C***surface albedo

      REAL RSFC

      REAL FX
      REAL ESUM( NLAYS )          ! total downward irradiance
      REAL FSUM( NLAYS )          ! total actinic flux

C***needed for stratospheric Raleigh optical depth

!!!      REAL, PARAMETER :: R_G = 2.9271E3   ! dry air gas constant divided
      REAL, PARAMETER :: R_G = 100.0 * RDGAS / GRAV  ! dry air gas constant
                                                     ! divided by gravitational
                                                     ! acceleration [cm/K] NOTE: cgs units

      REAL HSCALE               ! Scale height [cm] ! NOTE: cgs units

      REAL NBAR                 ! total number of air molecules [ # /cm**2 ]
                                ! above top of model domain

      REAL, SAVE :: COS85

C***FSB Cloud properties.
C***  FSB These properties are taken fro HU & Stamnes,1993,
C***  An accurate parameterizationof the radiative properties of
C***  water clouds suitable for use in climate models, Journal of
C***  Climate, vol. 6, pp. 728-742. The values in the data statements
C***  were calculated with an equivalent radius of 10 micrometers.
C***  Note: Hu &Stamnes give beta in [ 1 / km/ for LWC in [ g / m**3 ]
C***  the values for beta/ LWC also give beta in [1/m] with LWC in [g/m **3]


      REAL DTSCAT_CLOUD         ! cloud scattering coefficient [1/m]
      REAL G_CLOUD              ! local cloud asymmetry factor
      REAL OM_CLOUD             ! local cloud single scattering albedo

      LOGICAL, SAVE :: FIRST      = .TRUE.  ! Flag for first call
      LOGICAL       :: SUCCESS


C***arrays for fluxes and irradiances used in
      REAL, ALLOCATABLE, SAVE :: SRAYL( : )     ! Molecular scattering cross sections [ cm ** 2]
      REAL, ALLOCATABLE, SAVE :: TAU_SCAT( : )  ! scattering optical depth
      REAL, ALLOCATABLE, SAVE :: CONV_WM2( : )  ! conversion factor for
                                                ! [photons/(cm**2 s )] to [Watts/m**2]

C***three-dimensional array for Cs and Qy
C***  (temperature, wavelength, species)

      REAL, ALLOCATABLE, SAVE  :: XCS( :,:,: )
      REAL, ALLOCATABLE, SAVE  :: XQY( :,:,: )

C***(layer, wavelength species)

      REAL, ALLOCATABLE, SAVE  :: CSZ( :,:,: )
      REAL, ALLOCATABLE, SAVE  :: QYZ( :,:,: )


      IF ( FIRST ) THEN

         NEW_OPTICS_LOG = INIT3()
         
         NPHOT = NPHOTAB
         ALLOCATE( CONV_WM2( NWL ) )
         ALLOCATE( SRAYL   ( NWL ) )
         ALLOCATE( TAU_SCAT( NWL ) )
         ALLOCATE( XCS( IREFTEMPS, NWL, NPHOTAB ) )
         ALLOCATE( XQY( IREFTEMPS, NWL, NPHOTAB ) )
         ALLOCATE( CSZ( NLAYS, NWL, NPHOTAB ) )
         ALLOCATE( QYZ( NLAYS, NWL, NPHOTAB ) )


C***FSB Set up conversion factor for
C***  [photons / ( cm**2 s) ] to [Watts / m**2 ]
C***  THE 1.0E13 FACTO IS 1.0E9 * 1.0 E4
C***  The 1.0e9 is for the wavelength [ nm ] -> [ m ]
C***  The 1.0e4 is for the area [ cm **2 ] -> [ m**2 ]

         DO IWL = 1, NWL
            LAMDA = WAVELENGTH( IWL )
            CONV_WM2( IWL ) = 1.0E13 * ( PLANCK_C * LIGHT_SPEED )
     &                      / LAMDA
         END DO

         COS85 = COS( 85.0 * PI180 )

C***get molecular scattering cross sections

         CALL GETSRAY ( NWL, WAVELENGTH, SRAYL )

         FIRST = .FALSE.

      END IF   ! FIRSTIME

C***initialize BLKRJ

      NPHOT = NPHOTAB
      BLKRJ = 0.0


C***Initialize ETOT_SFC, TAUC_AERO, TAU_TOT, TAUO3_TOP, TAU_RAY, SSA

      ETOT_SFC  = 0.0
      TAUC_AERO  = 0.0
      TAU_TOT   = 0.0
      TAUO3_TOP = 0.0
      TAU_RAY   = 0.0
      TAU_CLOUD = 0.0
      SSA       = 0.0
      STOZONE   = 0.0

C***Test zenith angle. If coszen is zero or negative, zenith angle is
C***  equal to or greater than 90 degrees, i.e. before sunrise or
C***  after sunset at the surface.
C***  Return  all photolysis rates set to zero. Ignore possible twilight
C***  processes in upper troposphere.

C***FSB NOTE: tests of the algorithm for slant path show that the
C***  critical zenith angle for the tropospheric slant path is 88 degrees,
C***  but the critical zenith angle for the stratospheric slant path is
C***  85 degrees.  Thus, the code returns zeros for angles greater then or
C***  equalt to 85 degrees. cos( 85 degrees ) equals 8.715574e-02.

      IF ( COSZEN .LE. COS85 ) RETURN

C***Adjust Absorption cross sections and quantum yields for
C***  ambient conditions

      CALL GETCSQY ( NLAYS, NWL, IREFTEMPS, BLKTA, BLKDENS, NPHOT, CSZ, QYZ )

C***calculate scale height from top of model domain

      HSCALE = R_G * BLKTA( NLAYS )

C***estimate air density at top of model domain

      DENSTOM = BLKDENS( NLAYS )
     &        * EXP( -100.0 * ( BLKZF( NLAYS + 1 ) - BLKZH( NLAYS ) )
     &               / HSCALE )

C***calculate the total number of air molecules [ # / cm**2 ]
C***  above top of model domain.

      NBAR = HSCALE * DENSTOM

C***get total ozone column

!     DAY = JDATE - ( JDATE / 1000 ) * 1000
!     CALL O3AMT ( LATCR, LONGCR, DAY, STOZONE )
      Call O3TOTCOL ( LATCR, LONGCR, JDATE, STOZONE )

C***set top of modeling domain

      ZTOM = BLKZF( NLAYS + 1 )

C***get layer thicknesses and slantpath starting at the TOP

      CALL SLANTPATH2 ( NLAYS, BLKZF, ZSFC, REARTH, SINZEN, BLKDZ,
     &                  DSDH )

C***get slantpath from ZTOM to ZTOA

      CALL SLANTPATHTOP ( ZTOM, ZTOA, ZSFC, REARTH, SINZEN, DSDH_TOP )

C***FSB zero out summation variables

      TAUC_AERO  = 0.0
      TAU_TOT   = 0.0
      TAU_CLOUD = 0.0
      TAU_SCAT  = 0.0
      SSA       = 1.0

C***loop over wavelength

      DO IWL = 1, NWL           ! outermost loop

         LAMDA      = WAVELENGTH( IWL ) ! working wavelength [ nm ]
         LAMDA_UM   = 1.0E-3 * LAMDA ! convert to micro-meters
         INV_LAMBDA = 1.0 / LAMDA
         RSFC       = ALB( IWL )        ! surface albedo

C***stratosphere is assumed to exist above the model domain
C***  top layer. THe assumption here is that this layer is at about 100
C***  [mb] or 10 [kPa]
C***Get optical depth for stratospheric ozone

         CALL GET_TAUO3 ( IWL, STOZONE, STRAT_TEMP, TAU_O3 )

         TAUO3_TOP( IWL ) = TAU_O3

C***get Rayleigh optical depth for stratosphere

         TAU_RAY( IWL ) = NBAR * SRAYL( IWL )

C***set scaling factor for reducing extraterrestrial flux
C***  add ozone and Rayleigh optical depths. Use the
C***  pseudospherical correction for the stratosphere.

         FLXSCALE = 1.0 / RSQD

C*** initialize delta tau's and loop over layers

         DTAU      = 0.0
         DT_AERO   = 0.0
         DT_CLOUD  = 0.0

         DO L = 2, NLAYS + 1
            II = NLAYS + 2 - L  ! from top to bottom

C***in the following statements the factor of 100.0 converts
C***  converts [ 1 / cm ] to [ 1 / m ]

            BETA_M = SRAYL( IWL )           * BLKDENS( II ) * 100.0
            AO3    = CSZ( II, IWL, LO3O3P ) * BLKO3  ( II ) * 100.0
            ANO2   = CSZ( II, IWL, LNO2   ) * BLKNO2 ( II ) * 100.0


C***set up aerosol optical properties

            G_BAR = AERO_ASYM_FAC ( II, IWL )
            BEXT  = AERO_EXTI_COEF( II, IWL )
            BSCAT = AERO_SCAT_COEF( II, IWL )


C***calculate total absorption and scattering contributions
C***to optical depth

C***The contributions to scattering and absorption from molecules and particles
C***  are calculated separately to facilitate the calculation
C***  of the total single scatering albedo of the column of aerosols
C***  as measured by satellites.

            DTSCAT_M = BETA_M * BLKDZ( II ) ! molecular scattering
            DTSCAT_A = BSCAT  * BLKDZ( II ) ! particle scattering

            DTSCAT_M = MAX( DTSCAT_M, SMALL )
            DTSCAT_A = MAX( DTSCAT_A, SMALL )


            DTABS_M  = ( AO3 + ANO2  )  * BLKDZ( II ) ! molecular absorption
            DTABS_A  = ( BEXT - BSCAT ) * BLKDZ( II ) ! particle absorption

            DTABS_M  = MAX( DTABS_M, SMALL )
            DTABS_A  = MAX( DTABS_A, SMALL )

            DT_CLOUD( L ) = CLOUD_BETA_LWC( IWL ) * LWC( II ) * BLKDZ( II )

C***FSB adjust DT_CLOUD for cloud fraction.

            DT_CLOUD( L ) = DT_CLOUD( L ) * CLDFRC( II ) * SQRT( CLDFRC( II ) )

C***FSB The 3/2 power of CLDFRC is an approximation from the NCAR CCM2
C***  model that parameterizes cloud overlap.

            OM_CLOUD = 1.0 - CLOUD_COALBEDO( IWL )
            DTSCAT_CLOUD = OM_CLOUD * DT_CLOUD( L )
            G_CLOUD      = CLOUD_G( IWL )

C***calculate total absorption and scattering contributions
C***to optical depth

            DTSCAT = DTSCAT_M + DTSCAT_A + DTSCAT_CLOUD
            DTABS  = DTABS_M  + DTABS_A + ( 1.0 - OM_CLOUD ) * DT_CLOUD( L )

C***set aerosol optical depth for later use

            DT_AERO ( L )  = BEXT * BLKDZ( II )

C***Now calculate the vertical profiles of optical depth,
C***  single scattering albedo, asymmetry factor
C***  and DSDH starting at the top.

            DTAU( L ) = DTSCAT + DTABS
            OM  ( L ) = DTSCAT / ( DTSCAT + DTABS )
            G   ( L ) = ( G_BAR * DTSCAT_A + G_CLOUD * DTSCAT_CLOUD )
     &                / DTSCAT

            DSDH_TD( L ) = DSDH( L - 1 )

C***initialize optical depth profiles to the layer increment

            TAUC_AERO( II, IWL ) = DT_AERO( L )  ! aerosol optical depth
            TAU_TOT  ( II, IWL ) = DTAU( L )     ! total optical depth
            TAU_CLOUD( II, IWL ) = DT_CLOUD( L ) ! cloud optical depth

C***FSB get sums of unscaled optical depths

            TAU_SCAT ( IWL )     = TAU_SCAT ( IWL ) + DTSCAT_A
            TAUO3_TOP( IWL )     = TAUO3_TOP( IWL ) - ( AO3 * BLKDZ( II ) )  ! remove tropospheric O3 from stratospheric value

C***initialize optical depth profiles to the layer increment

            TAUC_AERO( II, IWL ) = DT_AERO( L )  ! aerosol optical depth
            TAU_TOT  ( II, IWL ) = DTAU( L )     ! total optical depth
            TAU_CLOUD( II, IWL ) = DT_CLOUD( L ) ! cloud optical depth

         END DO                 ! loop over layers

C***set values for the stratosphere

         OM     ( 1 ) = TAU_RAY( IWL )
     &                / ( TAU_RAY( IWL ) + TAUO3_TOP( IWL ) )
         G      ( 1 ) = 0.05
         DTAU   ( 1 ) = TAUO3_TOP( IWL ) + TAU_RAY( IWL )
         DSDH_TD( 1 ) = DSDH_TOP

         NLEVEL = NLAYS + 1

C***calculate optical depth profiles
         TAU_TOT  ( NLAYS, IWL ) = TAU_TOT  ( NLAYS, IWL ) + DTAU( 1 )
         TAUC_AERO( NLAYS, IWL ) = TAUC_AERO( NLAYS, IWL ) + DT_AERO( 1 )  
         TAU_CLOUD( NLAYS, IWL ) = TAU_CLOUD( NLAYS, IWL ) + DT_CLOUD( 1 ) 

         DO L =  NLAYS-1, 1, -1
            TAU_TOT  ( L, IWL ) = TAU_TOT  ( L, IWL ) + TAU_TOT  ( L+1, IWL )
            TAUC_AERO( L, IWL ) = TAUC_AERO( L, IWL ) + TAUC_AERO( L+1, IWL ) 
            TAU_CLOUD( L, IWL ) = TAU_CLOUD( L, IWL ) + TAU_CLOUD( L+1, IWL )
         END DO
         
C***Set fluxes to zero

         FDIR = 0.0
         FUP  = 0.0
         FDN  = 0.0
         EDIR = 0.0
         EUP  = 0.0
         EDN  = 0.0


C***calculate fluxes and irradiances


         CALL TWOSTREAM_S ( NLEVEL, COSZEN, RSFC, DTAU, OM, G, DSDH_TD,
     &                      FDIR, FUP, FDN, EDIR, EUP, EDN )

         DO L = 1, NLAYS
            II = NLAYS + 2 - L
            FX = FDIR( II ) + FDN( II ) + FUP( II ) ! actinic flux
            FSUM( L ) = FX
            ESUM( L ) = EDIR( II ) + EDN( II ) ! downward irradiance
                        
         END DO                 ! loop over layers

C***FSB Calculate column averaged scattering albedo

         IF( TAUC_AERO( 1, IWL ) .GT. 1.0E-30 )THEN
             SSA( IWL ) = TAU_SCAT( IWL ) / TAUC_AERO( 1, IWL )
         END IF
         
C***FSB capture the total downward irradiance at the surface [ W / m**2]

         ETOT_SFC( IWL ) = CONV_WM2( IWL ) * FLXSCALE * FEXT( IWL )
     &                   * ESUM( 1 )

         DO L = 1, NLAYS

C***multiply by the spectral flux at the top of
C***  the model domain, and calculate the appropriate
C***  actinic flux

            DF = FLXSCALE * FEXT( IWL ) * FSUM( L )

C***compute rate of photolysis (j-values) for each reaction

            DO IPHOT = 1, NPHOT

               DJ = DF * CSZ( L, IWL, IPHOT )
     &                 * QYZ( L, IWL, IPHOT )
               BLKRJ(L, IPHOT) = ( BLKRJ(L, IPHOT) + DJ ) ! [ 1 / sec ]
!!!               BLKRJ( L, IPHOT ) = BLKRJ( L, IPHOT ) + ( DJ * 60.0 ) ! [ 1 / min ]

            END DO     ! loop on IPHOT
         END DO    ! loop on layers

      END DO    ! loop over wavelengths

9503   FORMAT('LAYER = ',I3,' MODE = ',I3,' LAMBDA(nm)  = ',ES12.4,' DGN_CORE(m) = ',ES12.4,
     &        ' DGN_SHELL(m) = ', ES12.4 / ' REFRACT_IDX_SHELL(NR,NI) = ', 2(ES12.4,1X),
     &        ' REFRACT_IDX_CORE(NR,NI) = ', 2(ES12.4,1X) / ' LN(GEO.STD.DEV.) = ',
     &        ES12.4)
9504   FORMAT('LAYER = ',I3,' MODE = ',I3,' LAMBDA(nm)  = ',ES12.4,' DGN(m) = ',ES12.4,
     &        ' REFRACT_IDX(NR,NI) = ', 2(ES12.4,1X) / ' VOL.DENS. = ', ES12.4,
     &        ' LN(GEO.STD.DEV.) = ', ES12.4)

      RETURN
      END SUBROUTINE NEW_OPTICS

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE LOAD_REF_DATA ( )
C-----------------------------------------------------------------------
C  Purpose: read input file for 
C           -wavelength bin and temperature structure.
C           -photolysis cross-sections and quantum
C           -landuse type data for surface alebdo
C
C  Revision History:
C   29 Mar 11 S.Roselle: Replaced I/O API include files with UTILIO_DEFN
C-----------------------------------------------------------------------

      USE UTILIO_DEFN
      USE AERO_DATA, ONLY: N_MODE

      IMPLICIT NONE

      INCLUDE SUBST_RXCMMN      ! chemical mechamism reactions COMMON

C***arguments

C     NONE

C***local

      LOGICAL :: WRITE_LOG = .TRUE.

      CHARACTER(  16 ) :: PNAME = 'LOAD_REF_DATA'
      CHARACTER(  16 ) :: CSQY_FILE = 'CSQY_DATA' ! CSQY_DATA i/o logical name
      CHARACTER(  16 ) :: PHOT_EXPECT
      CHARACTER(  30 ) :: LAND_EXPECT
      CHARACTER( 120 ) :: MSG                     ! buffer for messages to output
      CHARACTER( 240 ) :: FILE_LINE

      CHARACTER(  16 ),  ALLOCATABLE :: AE_RERACT_REF( : )

!     INTEGER, INTENT(OUT) :: NWL_PHOT    ! # of wavelengths used in PHOT_MOD.F
      INTEGER :: NWL_PHOT    ! # of wavelengths used in PHOT_MOD.F
      INTEGER :: IOST        ! IOST returned from OPEN function
      INTEGER :: JDATE = 0
      INTEGER :: LOG_UNIT
      INTEGER :: PHOTAB_UNIT
      INTEGER :: IPHOT, IPHOT_LOAD ! loop indices
      INTEGER :: ITT, ITT_LOAD     ! loop indices 
      INTEGER :: IP_MAP, IP_REF    ! photolysis reaction indicies
      INTEGER :: IWL_LOAD
      INTEGER :: STRT, FINI

      INTEGER :: NAE_REFRACT_REF 

      REAL,       ALLOCATABLE :: AE_IMAG_REFRACT( :, : )
      REAL,       ALLOCATABLE :: AE_REAL_REFRACT( :, : )

      LOGICAL                  :: ERROR_FLAG = .FALSE.

C***external functions: none

      LOG_UNIT = INIT3()

      PHOTAB_UNIT = GETEFILE( CSQY_FILE, .TRUE., .TRUE., PNAME )

      IF ( PHOTAB_UNIT .LT. 0 ) THEN
         MSG = 'Error opening the CSQY data file: ' // TRIM( CSQY_FILE )
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

C...begin read

      READ( PHOTAB_UNIT,'(22X,A32)' ) JTABLE_REF

      IF ( JTABLE_REF .NE. MECHNAME ) THEN
         MSG =  'WARNING: JTABLE mechanism is for ' // JTABLE_REF
     &       // ' but gas chemistry name is '       // MECHNAME
         CALL M3WARN( PNAME, 0, 0, MSG )
      END IF

      READ( PHOTAB_UNIT,'(10X,I4)' ) NPHOT_MAP

#ifdef verbose_phot
      write( log_unit,'(22x,a32)' ) jtable_ref
      write( log_unit,'(10x,i4)' ) nphot_map
#endif

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

      ALLOCATE( PNAME_MAP( NPHOT_MAP ) )
      ALLOCATE( PNAME_REF( NPHOT_MAP ) )
      ALLOCATE( PHOT_MAP ( NPHOT_MAP ) )

      DO IPHOT_LOAD = 1, NPHOT_MAP
         READ( PHOTAB_UNIT,'(A16)' ) PNAME_REF( IPHOT_LOAD )

#ifdef verbose_phot
         write( log_unit,'(i3,1x,a16)' ) iphot_load, pname_ref( iphot_load )
#endif

         PNAME_MAP( IPHOT_LOAD ) = PNAME_REF( IPHOT_LOAD )
         PHOT_MAP ( IPHOT_LOAD ) = IPHOT_LOAD
      END DO

      READ( PHOTAB_UNIT,'(10X,I3)' ) NTEMP_REF

#ifdef verbose_phot
      write( log_unit,'(10x,i3)' ) ntemp_ref
#endif

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

#ifdef verbose_phot
      write( log_unit,* ) trim( file_line )
#endif

      IREFTEMPS = NTEMP_REF

      ALLOCATE( TEMP_BASE( NTEMP_REF ) )

      DO ITT_LOAD = 1, NTEMP_REF
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

#ifdef verbose_phot
         write( log_unit,* ) trim( file_line )
#endif

         READ( FILE_LINE,* ) IPHOT_LOAD, TEMP_BASE( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,'(4x,f6.2)' ) temp_base( itt_load )
#endif

      END DO

      ALLOCATE( TEMP_REF( NTEMP_REF, NPHOT_MAP) )

      DO ITT_LOAD = 1, 14 ! skip next 13 lines
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( log_unit, '(I2,1X,A)' )ITT_LOAD,TRIM(FILE_LINE)
#endif
      END DO

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( log_unit, '(I2,1X,A)' )ITT_LOAD,TRIM(FILE_LINE)
#endif

!      READ( PHOTAB_UNIT,'(17X,I3)' ) NWL_REF
!     READ( FILE_LINE,'(17X,I3,2X,17X,I3)' ) NWL_REF, NAE_REFRACT_REF
      READ( FILE_LINE, 4999) NWL_REF

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      
#ifdef verbose_phot
      write( log_unit, * )TRIM(FILE_LINE)
#endif

      READ( FILE_LINE, 4999)NAE_REFRACT_REF

4999  FORMAT(17X,I3,2X,17X,I3)

      IF( NAE_REFRACT_REF .NE. NUM_REFRACTIVE )THEN
         WRITE( LOG_UNIT,* ) 'NAE_REFRACT_REF  = ', NAE_REFRACT_REF
         MSG = 'NAERO_REFRACT used in ' // CSQY_FILE
     &       // ' does equal NWL in PHOT_MOD.F file. '
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF
      
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, '(a3, 1x, a16)')'IDX', 'REFRACTIVE_IDX'
#endif

      ALLOCATE( AE_RERACT_REF   ( NAE_REFRACT_REF ) )
          
#ifdef verbose_phot
      write( log_unit, '(a3, 1x, a16)')'IDX', 'REFRACTIVE_IDX'
#endif

      STRT = SCAN(FILE_LINE, '=', BACK = .TRUE.) + 1
      FINI =  LEN(FILE_LINE)

      READ( FILE_LINE( STRT:FINI ), * )( AE_RERACT_REF( ITT_LOAD ), 
     &                                   ITT_LOAD = 1, NAE_REFRACT_REF )

      DO ITT_LOAD = 1, NAE_REFRACT_REF 
! set up refractive indices used by aero_photdata routine

          REFRACTIVE_INDEX( ITT_LOAD )%NAME = AE_RERACT_REF( ITT_LOAD )
          ALLOCATE( REFRACTIVE_INDEX( ITT_LOAD )%REAL_PART( N_MODE, NWL_REF ) )         
          ALLOCATE( REFRACTIVE_INDEX( ITT_LOAD )%IMAG_PART( N_MODE, NWL_REF )  ) 
           
#ifdef verbose_phot
          write( log_unit, '(i3, 1x, a16)')itt_load, refractive_index( itt_load )%name
#endif

      END DO

      
      
      NWL       = NWL_REF
      NWL_PHOT  = NWL

#ifdef verbose_phot
      write( log_unit,'(17x,i3)' ) nwl_ref
#endif

      IF ( NWL_REF .NE. NWL_PHOT ) THEN
         WRITE( LOG_UNIT,* ) 'NWL_PHOT = ', NWL_PHOT
         WRITE( LOG_UNIT,* ) 'NWL_REF  = ', NWL_REF
         MSG = 'NWL_REF used in ' // CSQY_FILE
     &       // ' does equal NWL in PHOT_MOD.F file. '
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, * )FILE_LINE
#endif

      ALLOCATE( STWL_REF        ( NWL_REF ) )
      ALLOCATE( EFFWL_REF       ( NWL_REF ) )
      ALLOCATE( ENDWL_REF       ( NWL_REF ) )
      ALLOCATE( FSOLAR_REF      ( NWL_REF ) )
      ALLOCATE( CLD_BETA_REF    ( NWL_REF ) )
      ALLOCATE( CLD_COALBEDO_REF( NWL_REF ) )
      ALLOCATE( CLD_G_REF       ( NWL_REF ) )
      ALLOCATE( AE_REAL_REFRACT ( NAE_REFRACT_REF, NWL_REF ) )
      ALLOCATE( AE_IMAG_REFRACT ( NAE_REFRACT_REF, NWL_REF ) )

      DO IWL_LOAD = 1, NWL_REF
!         READ( PHOTAB_UNIT,'(4X,3(F8.3,2X),2X,ES12.4,2X,2(F8.3,2X),ES12.4,2X)' )
         READ( PHOTAB_UNIT, * )iphot_load,
     &         STWL_REF( IWL_LOAD ), EFFWL_REF( IWL_LOAD ),
     &         ENDWL_REF( IWL_LOAD ), FSOLAR_REF( IWL_LOAD ),
     &         CLD_BETA_REF( IWL_LOAD ), CLD_G_REF( IWL_LOAD ),
     &         CLD_COALBEDO_REF( IWL_LOAD ),
     &         ( AE_REAL_REFRACT( ITT_LOAD, IWL_LOAD ), 
     &           AE_IMAG_REFRACT( ITT_LOAD, IWL_LOAD ),
     &           ITT_LOAD = 1, NAE_REFRACT_REF )

               DO ITT_LOAD = 1, NAE_REFRACT_REF
                  REFRACTIVE_INDEX( ITT_LOAD )%REAL_PART( 1:N_MODE, IWL_LOAD ) 
     &                                      = AE_REAL_REFRACT( ITT_LOAD, IWL_LOAD )
                  REFRACTIVE_INDEX( ITT_LOAD )%IMAG_PART( 1:N_MODE, IWL_LOAD ) 
     &                                      = AE_IMAG_REFRACT( ITT_LOAD, IWL_LOAD )
               END DO
#ifdef verbose_phot
         write( log_unit,'(4x,3(f8.3,2x),2x,2(es12.4,2x),f8.3,2x,12(es12.4,2x))' )
     &          stwl_ref( iwl_load ), effwl_ref( iwl_load ),
     &          endwl_ref( iwl_load ),fsolar_ref( iwl_load ),
     &          cld_beta_ref( iwl_load ), cld_g_ref( iwl_load ),
     &          cld_coalbedo_ref( iwl_load ),
     &          ( ae_real_refract( itt_load, iwl_load ),
     &            ae_imag_refract( itt_load, iwl_load ), itt_load = 1, nae_refract_ref )
#endif

      END DO


      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

      ALLOCATE( CS_REF ( NPHOT_MAP, NTEMP_REF, NWL_REF ) )
      ALLOCATE( QY_REF ( NPHOT_MAP, NTEMP_REF, NWL_REF ) )
      ALLOCATE( EQY_REF( NPHOT_MAP, NTEMP_REF, NWL_REF ) )
      ALLOCATE( ECS_REF( NPHOT_MAP, NTEMP_REF, NWL_REF ) )

      CS_REF = 0.0
      QY_REF  = 0.0
      EQY_REF = 0.0
      ECS_REF = 0.0

      DO IPHOT_LOAD = 1, NPHOT_MAP
         DO ITT_LOAD = 1, NTEMP_REF
            READ( PHOTAB_UNIT,'(A16,7X,F8.3,1X,40(1PE12.6,2X))' )
     &            PHOT_EXPECT, TEMP_REF( ITT_LOAD, IPHOT_LOAD),
     &            ( CS_REF( IPHOT_LOAD, ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_REF )

#ifdef verbose_phot
            write( log_unit,'(a16,7x,f8.3,1x,40(1pe12.6,2x))' )
     &             phot_expect, temp_ref( itt_load, iphot_load),
     &             ( cs_ref( iphot_load, itt_load, iwl_load), iwl_load = 1, nwl_ref )
#endif

            IF ( PHOT_EXPECT .NE. PNAME_REF( IPHOT_LOAD ) ) THEN
                MSG =  'CS for ' // TRIM( PHOT_EXPECT )
     &              // ' does match the order the PHOT_MAP array.'
                CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
            END IF

            READ( PHOTAB_UNIT,'(A16,7X,F8.3,1X,40(1PE12.6,2X))' )
     &            PHOT_EXPECT, TEMP_REF( ITT_LOAD, IPHOT_LOAD),
     &            ( EQY_REF( IPHOT_LOAD, ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_REF )

            QY_REF( IPHOT_LOAD, ITT_LOAD, 1:NWL_REF) = EQY_REF( IPHOT_LOAD, ITT_LOAD, 1:NWL_REF)

#ifdef verbose_phot
            write( log_unit,'(a16,7x,f8.3,1x,40(1pe12.6,2x))' )
     &             phot_expect, temp_ref( itt_load, iphot_load),
     &             ( qy_ref( iphot_load, itt_load, iwl_load), iwl_load = 1, nwl_ref )
#endif

            IF ( PHOT_EXPECT .NE. PNAME_REF(IPHOT_LOAD) ) THEN
               MSG =  'EQY for ' // TRIM( PHOT_EXPECT )
     &             // ' does match the order the PHOT_MAP array.'
               CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
            END IF
         END DO
      END DO

      DO ITT_LOAD = 1, 3 ! skip next 3 lines
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      END DO

      READ( PHOTAB_UNIT,'(15X,I3)' ) NTEMP_STRAT_REF

#ifdef verbose_phot
      write( log_unit,'(16x,i3)' ) ntemp_strat_ref
#endif

      ALLOCATE( TEMP_STRAT_REF ( NTEMP_STRAT_REF ) )
      ALLOCATE( O3_CS_STRAT_REF( NTEMP_STRAT_REF, NWL_REF ) )

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

      DO ITT_LOAD = 1, NTEMP_STRAT_REF
         READ( PHOTAB_UNIT,'(A16,7X,F8.3,1X,40(1PE12.6,2X))' )
     &         PHOT_EXPECT, TEMP_STRAT_REF( ITT_LOAD ),
     &         ( O3_CS_STRAT_REF( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_REF )

#ifdef verbose_phot
         write( log_unit,'(a16,7x,f8.3,1x,40(1pe12.6,2x))' )
     &          phot_expect, temp_strat_ref( itt_load ),
     &          ( o3_cs_strat_ref( itt_load, iwl_load), iwl_load = 1, nwl_ref )
#endif

         IF ( PHOT_EXPECT .NE. 'O3_STRAT' ) THEN
            MSG = 'O3_STRAT not found at expected location in CSQY_FILE. ' //
     &            TRIM( PHOT_EXPECT ) // ' found.'
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
         END IF
      END DO

      DO ITT_LOAD = 1, 5 ! skip next 5 lines
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      END DO

      READ( PHOTAB_UNIT,5013 ) NUMB_LANDUSE_REF

      DO ITT_LOAD = 1, 3 ! skip next 3 lines
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      END DO

      READ( PHOTAB_UNIT,5013 ) INDEX_GRASSLAND_REF
      READ( PHOTAB_UNIT,5013 ) INDEX_OCEAN_REF
      READ( PHOTAB_UNIT,5013 ) INDEX_SEA_ICE

#ifdef verbose_phot
      write( log_unit,6013 )'NUMB_LANDUSE_REF    = ', numb_landuse_ref
      write( log_unit,6013 )'INDEX_GRASSLAND_REF = ', index_grassland_ref
      write( log_unit,6013 )'INDEX_OCEAN_REF     = ', index_ocean_ref
      write( log_unit,6013 )'INDEX_SEA_ICE       = ', index_sea_ice
#endif

      ALLOCATE( LANDUSE_REF     ( NUMB_LANDUSE_REF ) )
      ALLOCATE( ZENITH_COEFF_REF( NUMB_LANDUSE_REF ) )
      ALLOCATE( SEASON_COEFF_REF( NUMB_LANDUSE_REF ) )
      ALLOCATE( SNOW_COEFF_REF  ( NUMB_LANDUSE_REF ) )
      ALLOCATE( SPECTRAL_ALBEDO_REF( NWL_PHOT, NUMB_LANDUSE_REF ) )

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE ! skip line

      DO ITT_LOAD = 1, NUMB_LANDUSE_REF
         READ( PHOTAB_UNIT,5012 ) LANDUSE_REF( ITT_LOAD ),
     &                            ZENITH_COEFF_REF( ITT_LOAD ),
     &                            SEASON_COEFF_REF( ITT_LOAD ),
     &                            SNOW_COEFF_REF( ITT_LOAD )
#ifdef verbose_phot
         write( log_unit,5012 ) landuse_ref( itt_load ),
     &                          zenith_coeff_ref( itt_load ),
     &                          season_coeff_ref( itt_load ),
     &                          snow_coeff_ref( itt_load )
#endif
      END DO

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE ! skip line

      DO ITT_LOAD = 1, NUMB_LANDUSE_REF
         READ( PHOTAB_UNIT,'(A30,1X,40(1PE12.6,2X))' ) LAND_EXPECT,
     &        ( SPECTRAL_ALBEDO_REF(IWL_LOAD, ITT_LOAD), IWL_LOAD = 1, NWL_REF )

#ifdef verbose_phot
         write( log_unit,'(a30,1x,40(1pe12.6,2x))' ) trim( land_expect ),
     &        ( spectral_albedo_ref(iwl_load, itt_load), iwl_load = 1, nwl_ref )
#endif

      END DO

      DO ITT_LOAD = 1, 3 ! skip next 3 lines
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      END DO

      READ( PHOTAB_UNIT,5013 ) NUMB_LANDUSE_NLCD50
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE ! skip line

#ifdef verbose_phot
      write( log_unit,6013 ) 'NUMB_NLCD50_MODIS = ', numb_landuse_NLCD50
      write( log_unit,6009 ) '! I', 'LANDUSE_NLCD50-MODIS', 'INDEX_ALBREF',
     &                       'FAC_ALBREF'
#endif

      ALLOCATE( LANDUSE_NLCD50( NUMB_LANDUSE_NLCD50 )  )
      ALLOCATE( ALBMAP_REF2NLCD50( NUMB_LANDUSE_NLCD50 )  )
      ALLOCATE( ALBFAC_REF2NLCD50( NUMB_LANDUSE_NLCD50 )  )

      DO ITT_LOAD = 1, NUMB_LANDUSE_NLCD50
         READ( PHOTAB_UNIT,5016 ) LANDUSE_NLCD50( ITT_LOAD ),
     &                            ALBMAP_REF2NLCD50( ITT_LOAD ),
     &                            ALBFAC_REF2NLCD50( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,6016 ) itt_load, landuse_NLCD50( itt_load ),
     &                          albmap_ref2NLCD50( itt_load ),
     &                          albfac_ref2NLCD50( itt_load )
#endif

      END DO

      READ( PHOTAB_UNIT,5013 ) NUMB_LANDUSE_USGS
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE ! skip line

#ifdef verbose_phot
      write( log_unit,6013 ) 'NUMB_USGS = ', numb_landuse_usgs
      write( log_unit,6009 ) '! I','LANDUSE_USGS', 'INDEX_ALBREF', 'FAC_ALBREF'
#endif

      ALLOCATE( LANDUSE_USGS   ( NUMB_LANDUSE_USGS ) )
      ALLOCATE( ALBMAP_REF2USGS( NUMB_LANDUSE_USGS ) )
      ALLOCATE( ALBFAC_REF2USGS( NUMB_LANDUSE_USGS ) )

      DO ITT_LOAD = 1, NUMB_LANDUSE_USGS
         READ( PHOTAB_UNIT,5016 ) LANDUSE_USGS( ITT_LOAD ),
     &                            ALBMAP_REF2USGS( ITT_LOAD ),
     &                            ALBFAC_REF2USGS( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,6016 ) itt_load, landuse_usgs( itt_load ),
     &                          albmap_ref2usgs( itt_load ),
     &                          albfac_ref2usgs( itt_load )
#endif

      END DO

      READ( PHOTAB_UNIT,5013 ) NUMB_LANDUSE_MODIS
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE ! skip line

#ifdef verbose_phot
      write( log_unit,6013 ) 'NUMB_MODIS = ', numb_landuse_modis
      write( log_unit,6009 ) '! I','LANDUSE_MODIS', 'INDEX_ALBREF', 'FAC_ALBREF'
#endif

      ALLOCATE( LANDUSE_MODIS   ( NUMB_LANDUSE_MODIS ) )
      ALLOCATE( ALBMAP_REF2MODIS( NUMB_LANDUSE_MODIS ) )
      ALLOCATE( ALBFAC_REF2MODIS( NUMB_LANDUSE_MODIS ) )

      DO ITT_LOAD = 1, NUMB_LANDUSE_MODIS
         READ( PHOTAB_UNIT,5016 ) LANDUSE_MODIS( ITT_LOAD ),
     &                            ALBMAP_REF2MODIS( ITT_LOAD ),
     &                            ALBFAC_REF2MODIS( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,6016 ) itt_load, landuse_modis( itt_load ),
     &                          albmap_ref2modis( itt_load ),
     &                          albfac_ref2modis( itt_load )
#endif

      END DO

      NO_NLCD40 = .TRUE.  ! default condition that file does not contain NLCD40 Landuse data
      
      READ( PHOTAB_UNIT,5013, END = 101 ) NUMB_LANDUSE_NLCD40
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE ! skip line

#ifdef verbose_phot
      write( log_unit,6013 ) 'NUMB_NLCD40_MODIS = ', numb_landuse_NLCD40
      write( log_unit,6009 ) '! I', 'LANDUSE_NLCD40-MODIS', 'INDEX_ALBREF',
     &                       'FAC_ALBREF'
#endif

      ALLOCATE( LANDUSE_NLCD40( NUMB_LANDUSE_NLCD40 )  )
      ALLOCATE( ALBMAP_REF2NLCD40( NUMB_LANDUSE_NLCD40 )  )
      ALLOCATE( ALBFAC_REF2NLCD40( NUMB_LANDUSE_NLCD40 )  )

      DO ITT_LOAD = 1, NUMB_LANDUSE_NLCD40
         READ( PHOTAB_UNIT,5016 ) LANDUSE_NLCD40( ITT_LOAD ),
     &                            ALBMAP_REF2NLCD40( ITT_LOAD ),
     &                            ALBFAC_REF2NLCD40( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,6016 ) itt_load, landuse_NLCD40( itt_load ),
     &                          albmap_ref2NLCD40( itt_load ),
     &                          albfac_ref2NLCD40( itt_load )
#endif

      END DO
      
      NO_NLCD40 = .FALSE.

101   IF( NO_NLCD40 )THEN
          MSG = TRIM( PNAME ) // ':'
     &       // TRIM( CSQY_FILE )
     &       // ' does not contain data for NLCD40 land use and'
     &       // ' corresponds to CMAQ version 5.01.'
          CALL M3MESG( MSG )
      END IF

      ALLOCATE( WAVELENGTH( NWL_PHOT ) )
      ALLOCATE( FEXT      ( NWL_PHOT ) )
      ALLOCATE( ALB       ( NWL_PHOT ) )


      ALLOCATE( CLOUD_BETA_LWC( NWL_PHOT ) )  ! cloud extinction coef divided by LWC
      ALLOCATE( CLOUD_COALBEDO( NWL_PHOT ) )  ! cloud coalbedo
      ALLOCATE( CLOUD_G       ( NWL_PHOT ) )  ! cloud asymmetry factor

      DO IWL_LOAD = 1, NWL_PHOT
         WAVELENGTH    ( IWL_LOAD ) = EFFWL_REF       ( IWL_LOAD )
         FEXT          ( IWL_LOAD ) = FSOLAR_REF      ( IWL_LOAD )
         CLOUD_G       ( IWL_LOAD ) = CLD_G_REF       ( IWL_LOAD )
         CLOUD_COALBEDO( IWL_LOAD ) = CLD_COALBEDO_REF( IWL_LOAD )
         CLOUD_BETA_LWC( IWL_LOAD ) = CLD_BETA_REF    ( IWL_LOAD )
      END DO

      DO IWL_LOAD = 1, NWL_PHOT
         IF ( WAVELENGTH( IWL_LOAD ) .LE. 380.1 ) THEN
            ALB( IWL_LOAD ) = 0.05
         ELSE
            ALB( IWL_LOAD ) = 0.10
         END IF
      END DO

      NTEMP_STRAT = NTEMP_STRAT_REF
      ALLOCATE( TEMP_O3_STRAT( NTEMP_STRAT_REF ) )
      ALLOCATE( XO3CS        ( NTEMP_STRAT_REF, NWL_PHOT ) )

      DO ITT_LOAD = 1, NTEMP_STRAT_REF
         TEMP_O3_STRAT( ITT_LOAD ) = TEMP_STRAT_REF( ITT_LOAD )
         DO IWL_LOAD = 1, NWL_PHOT
            XO3CS( ITT_LOAD, IWL_LOAD ) = O3_CS_STRAT_REF( ITT_LOAD, IWL_LOAD )
         END DO
      END DO

C***initialize pointers for mandatory photolysis rates

      LNO2      = 0
      LO3O1D    = 0
      LO3O3P    = 0
      LACETONE  = 0
      LKETONE   = 0
      LMGLY_ADJ = 0
      LMGLY_ABS = 0

C***get needed photolysis data for the model chemistry from the
C***CSQY_DATA

       ALLOCATE( PHOTOLYSIS_RATE ( NPHOTAB ) )
       ALLOCATE( XXCS( NPHOTAB, IREFTEMPS, NWL ) )
       ALLOCATE( XXQY( NPHOTAB, IREFTEMPS, NWL ) )
       ALLOCATE( RTEMP_S( IREFTEMPS, NPHOTAB ) )

       DO IPHOT = 1, NPHOTAB
          IP_MAP = INDEXR( PHOTAB( IPHOT ), NPHOT_MAP, PNAME_MAP )
          IF ( IP_MAP .LE. 0 ) THEN
             MSG = 'FATAL ERROR: photolysis reaction ' // TRIM( PHOTAB( IPHOT ) )
     &          // ' not found in ' //
     &             'the reference data! '
             ERROR_FLAG = .TRUE.
             CALL M3WARN ( PNAME, 0, 0, MSG )
          END IF
          IP_REF = PHOT_MAP( IP_MAP )
          PHOTOLYSIS_RATE( IPHOT ) = PNAME_MAP( IP_MAP )

C***check to see if this photolysis reaction is a special case that
C***  is referenced in other sections of the code.  if so, then set
C***  the appropriate pointers for later processing

           SELECT CASE ( TRIM( PHOTOLYSIS_RATE( IPHOT ) ) )
              CASE( 'O3O3P', 'O3O3P_SAPRC99', 'O3O3P_06', 'O3_O3P_IUPAC04', 'O3O3P_NASA06' )
                    LO3O3P = IPHOT
              CASE( 'NO2', 'NO2_SAPRC99', 'NO2_06', 'NO2_RACM2' )
                    LNO2 = IPHOT
              CASE( 'O3O1D',  'O3O1D_SAPRC99' , 'O3O1D_06', 'O3_O1D_IUPAC04', 'O3O1D_NASA06' )
                    LO3O1D = IPHOT
              CASE( 'KETONE', 'KET_RACM2' )
                    LKETONE   = IPHOT
              CASE( 'MGLY_ADJ' )
                    LMGLY_ADJ = IPHOT
              CASE(  'MGLY_ABS' )
                    LMGLY_ABS = IPHOT
              CASE( 'ACETONE', 'CH3COCH3_RACM2' )
                    LACETONE  = IPHOT
              CASE( 'HCHOR_06', 'HCHO_RAD_RACM2')
                    LHCHOR_06 = IPHOT
           END SELECT


C***load the local cross section & quantum yield data from the reference
C***  dataset for this photolysis reaction

            DO ITT = 1, IREFTEMPS
               DO IWL = 1, NWL
                  XXCS( IPHOT, ITT, IWL ) = CS_REF( IP_REF, ITT, IWL )
                  XXQY( IPHOT, ITT, IWL ) = QY_REF( IP_REF, ITT, IWL )
                  RTEMP_S( ITT, IPHOT ) = TEMP_REF( ITT, IP_REF )
               END DO   ! iwl
            END DO   ! itt

       END DO   ! iphot

       IF ( LNO2   .EQ. 0 ) THEN
          MSG = 'NO2 cross-section not found in the CSQY data! '
          ERROR_FLAG = .TRUE.
          CALL M3WARN ( PNAME, 0, 0, MSG )
       END IF
       IF ( LO3O1D .EQ. 0 ) THEN
          MSG = 'O3(1D) production not found in the CSQY data! '
          CALL M3WARN ( 'NEW_OPTICS', 0, 0, MSG )
       END IF
       IF ( LO3O3P .EQ. 0 ) THEN
          MSG = 'O3 cross-section not found in the CSQY data! '
          ERROR_FLAG = .TRUE.
          CALL M3WARN ( PNAME, 0, 0, MSG )
       END IF

       IF( ERROR_FLAG )THEN
         MSG = 'The above fatal error(s) found in CSQY data! '
         CALL M3EXIT( PNAME, 0, 0, MSG, 1 )
       END IF

      WRITE( LOG_UNIT,* ) 'Sucessfully Loaded JTABLE'

5012  FORMAT( 4X,A30,1X,3(F8.3,2X) )
5013  FORMAT( 22X,I3 )
5016  FORMAT( 4X,A60,1X,I3,2X,3(F8.3,2X) )

#ifdef verbose_phot
6009  format( a3,', ',8(a,', ') )
6013  format( a22,1x,i3 )
6016  format( i3,1x,a60,1x,i3,2x,3(f8.3,2x) )
#endif

      RETURN
      END SUBROUTINE LOAD_REF_DATA

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE GETSRAY ( NWL, LAMDA, SRAYL )
C-----------------------------------------------------------------------
C  calculate molecular (Rayleigh) scattering cross section, srayl
C
C  coded 09/08/2004 by Dr. Francis S. Binkowski
C     Carolina Environmental Program
C     University of North Carolina at Chapel Hill
C     email: frank_binkowski@unc.edu
C
C  Reference:
C     Nicolet, M., On the molecular scattering in the terrestrial
C     atmosphere: An empirical formula for its calculation in the
C     homoshpere, Planetary and Space Science. Vol. 32,No. 11,
C     Pages 1467-1468, November 1984.
C-----------------------------------------------------------------------

      IMPLICIT NONE

C***arguments

      INTEGER, INTENT(IN) :: NWL         ! number of wavelength bins

      REAL, INTENT(IN)  :: LAMDA( NWL )  ! wavelengths  [nm]
      REAL, INTENT(OUT) :: SRAYL( NWL )  ! molecular scattering cross sections [cm**2]

C***Internal variables

      INTEGER I
      REAL WMICRN               ! wavelenght in micrometers
      REAL WMICRN1              ! 1 / wmicrn
      REAL XX                   ! variable in Nicolet method

C***get molecular scattering cross section. This is a fixed
C***  function of wavelength.

      DO I = 1, NWL
         WMICRN = 1.0E-3 * LAMDA( I ) ! wavelength in micrometers
         WMICRN1 = 1.0 / WMICRN

         IF ( WMICRN .LE. 0.55 ) THEN
            XX = 3.6772 + 0.389 * WMICRN + 0.09426 * WMICRN1
         ELSE
            XX = 4.04
         END IF

         SRAYL( I ) = 4.02E-28 * WMICRN1**XX    ! in [cm**2]

      END DO

      RETURN
      END SUBROUTINE GETSRAY

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE GETCSQY ( NLAYS, NWL, IREFTEMPS, TEMP, DENS, NPHOT, CSZ, QYZ )
C-----------------------------------------------------------------------
C  Purpose: Calculate values of absorption cross
C     section and quantum yield, given, temperature and air
C     pressure and density.
C
C     FSB This version uses different reference temperatures for
C     different species.
C
C  coded 10/14/2004 by Dr. Francis S. Binkowski
C     Carolina Environmental Program
C     University of North Carolina at Chapel Hill
C     email: frank_binkowski@unc.edu
C
C  reference for acetone:
C     Cameron-Smith, Philip J., Incorporating non_linear computation
C     code (Fast-J), Journal of Atmospheric Chemistry, Vol. 37,
C     pp 283-297, 2000)
C
C  Mar 2011: Bill Hutzell
C     - revised interpolation method for a general number of
C     interpolation points
C     - revised density corrections for specific photolysis reactions
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

C***Arguments

      INTEGER, INTENT(IN) :: NLAYS         ! number of layers
      INTEGER, INTENT(IN) :: NWL           ! number of wave length intervals
      INTEGER, INTENT(IN) :: NPHOT         ! number of photolysis reactions
      INTEGER, INTENT(IN) :: IREFTEMPS     ! # of temperatures for XXCS and XXQY values

      REAL, INTENT(IN)  :: TEMP( NLAYS )    ! air temperature [K]
      REAL, INTENT(IN)  :: DENS( NLAYS )    ! air density [molecules/cm**2]
      REAL, INTENT(OUT) :: CSZ( NLAYS, NWL, NPHOT ) ! abs cross sections
      REAL, INTENT(OUT) :: QYZ( NLAYS, NWL, NPHOT ) ! quantum yields

C***Internal:

      INTEGER  IT, IWL, LAYS, IPHOT, ITEMP

      REAL  XTEMP            ! local temperature
      REAL  YTEMP            ! temperature difference ratio
      REAL, ALLOCATABLE, SAVE :: REFTEMP   ( : )
      REAL, ALLOCATABLE, SAVE :: DELTA_REFT( : )

      REAL, PARAMETER :: TTX1 = 235.0
      REAL, PARAMETER :: TTX2 = 298.0
      REAL, PARAMETER :: DTTX = TTX2 - TTX1

      REAL            :: PRESSURE       ! units vary
      REAL            :: QDUM           ! scratch variable for yield
      REAL            :: ALPHA, BETA    ! scratch variables

      LOGICAL, SAVE   :: FIRSTCALL = .TRUE.

      IF ( FIRSTCALL ) THEN
          ALLOCATE( REFTEMP ( IREFTEMPS ) )
          ALLOCATE( DELTA_REFT( IREFTEMPS - 1 ) )
          FIRSTCALL = .FALSE.
      END IF

C***Loop over layer:

      DO LAYS = 1, NLAYS

C***Use new values

C***fetch ambient temperature

         XTEMP = TEMP( LAYS )

C***loop over species

         DO IPHOT = 1, NPHOT

C***set reference temperatures for the species

            REFTEMP( 1:IREFTEMPS ) = RTEMP_S( 1:IREFTEMPS, IPHOT )

C***set reference temperature differences AND find temperature range:

            DO IT = 1, IREFTEMPS - 1
               DELTA_REFT( IT ) = REFTEMP( IT + 1 ) - REFTEMP( IT )
               IF ( XTEMP .GT. REFTEMP( IT ) .AND. XTEMP .LE. REFTEMP( IT + 1 ) ) THEN
                  ITEMP = IT
               END IF
            END DO

            IF ( XTEMP .LE. REFTEMP( 1 ) )  THEN
               ITEMP = 0
            ELSE IF ( XTEMP .GE. REFTEMP( IREFTEMPS ) ) THEN
               ITEMP = IREFTEMPS
            END IF

C***loop over wavelengths

            DO IWL = 1, NWL

C***for this case hold value constant with temperature
C***  for the case where the temperature variation is zero or
C***  unknown this section uses the constant values, because
C***  the reference temperature has been set to 350 [K] a value
C***  that is warmer than any atmospheric temperature

               IF ( ITEMP .EQ. 0 ) THEN

                  CSZ( LAYS, IWL, IPHOT ) = XXCS( IPHOT, ITEMP+1, IWL )
                  QYZ( LAYS, IWL, IPHOT ) = XXQY( IPHOT, ITEMP+1, IWL )

C***for the next case use linear interpolation

               ELSE IF ( ITEMP .GE. 1 .AND. ITEMP .LT. IREFTEMPS ) THEN

                  YTEMP = ( XTEMP - REFTEMP( ITEMP ) )
     &                  / DELTA_REFT( ITEMP )

                  CSZ( LAYS, IWL, IPHOT ) =   XXCS( IPHOT, ITEMP, IWL )
     &                                    + ( XXCS( IPHOT, ITEMP+1, IWL )
     &                                      - XXCS( IPHOT, ITEMP, IWL ) )
     &                                    * YTEMP
                  QYZ( LAYS, IWL, IPHOT ) =   XXQY( IPHOT, ITEMP, IWL )
     &                                    + ( XXQY( IPHOT, ITEMP+1, IWL )
     &                                      - XXQY( IPHOT, ITEMP, IWL ) )
     &                                    * YTEMP

C***if the ambient temperature is warmer than the maximum
C***  reference temperature, then use the value at the maximum reference temperature

               ELSE IF ( ITEMP .EQ. IREFTEMPS ) THEN

                  CSZ( LAYS, IWL, IPHOT ) = XXCS( IPHOT, ITEMP, IWL )
                  QYZ( LAYS, IWL, IPHOT ) = XXQY( IPHOT, ITEMP, IWL )

               END IF

C***Make density corrections using values from Fast-JX V. 5.0

               SELECT CASE ( TRIM( PHOTOLYSIS_RATE( IPHOT ) ) )

                  CASE ( 'CCHO_R', 'ALD2', 'CH3CHO_RACM2' )      ! 'CH3CHO -> CH3 + HCO'

                     IF ( QYZ( LAYS, IWL, IPHOT ) .GT. 1.0E-5 ) THEN
                              QDUM = QYZ( LAYS, IWL, IPHOT )
                              QYZ( LAYS, IWL, IPHOT ) = 1.0
     &                           / ( 1.0 + ( 1.0 / QDUM - 1.0 ) * DENS( LAYS ) / 2.465E19 )
                        QYZ( LAYS, IWL, IPHOT ) = MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) )
                        QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, QYZ( LAYS, IWL, IPHOT ) )
                     END IF

                  CASE ( 'C2CHO', 'ALD_RACM2', 'BALD_RACM2',  'UALD_RACM2' )  ! C3 and higher aldehydes

C***density correction to quantum yield

                     IF ( QYZ( LAYS, IWL, IPHOT ) .LT. 1.0E-5 ) THEN
                        QDUM = 0.0
                     ELSE
                        QDUM = 1.0
     &                       / ( 1.0 + ( 1.0 / QYZ( LAYS, IWL, IPHOT ) - 1.0 )
     &                         * DENS( LAYS ) / 2.465E19 )
                     END IF
                     QDUM = MIN( QDUM, 1.0 )

                     QYZ( LAYS, IWL, IPHOT ) = QDUM
                     QYZ( LAYS, IWL, IPHOT ) = MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) )
                     QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, QYZ( LAYS, IWL, IPHOT ) )

                  CASE ( 'MVK-06', 'MVK_06' )

C***quantum yield from
C***  Gierczak, T., J. B. Burkholder, R. K. Talukdar, A. Mellouki, S. B. Barone,
C***  and A. R. Ravishankara, Atmospheric fate of methyl vinyl ketone and methacrolein,
C***  J. Photochem. Photobiol A: Chemistry, 110 1-10, 1997.
C***  depends on pressure and wavelength, set upper limit to 1.0
C***  However, chamber evaluations for SAPRC07T require a pressure correction where
C***  number density coefficient is five times higher.

C***density correction to quantum yield

C***remove wavelength dependence

                     QDUM = QYZ( LAYS, IWL, IPHOT )
     &                    / ( 5.5 + 5.0 * 9.2E-19 * 2.465E+19 )
     &                    *  118.4 ! or   (5.5 + 5.0*9.2E-19*2.465E+19)
                     QDUM = MIN( QDUM, 1.0 )

                     QYZ( LAYS, IWL, IPHOT ) = QDUM
                     QYZ( LAYS, IWL, IPHOT ) = MIN(1.0, QYZ( LAYS, IWL, IPHOT ))
                     QYZ( LAYS, IWL, IPHOT ) = MAX(0.0, QYZ( LAYS, IWL, IPHOT ))

                  CASE ( 'MACR-06', 'MACR_06', 'MACR_RACM2' )

C***quantum yield based on 2.76 times MVK from
C***  Gierczak, T., J. B. Burkholder, R. K. Talukdar, A. Mellouki, S. B. Barone,
C***  and A. R. Ravishankara, Atmospheric fate of methyl vinyl ketone and methacrolein,
C***  J. Photochem. Photobiol A: Chemistry, 110 1-10, 1997.
C***  depends on pressure and wavelength, set upper limit to 1.0
C***  However, chamber evaluations for SAPRC07T require a pressure correction where
C***  number density coefficient is five times higher.

C***density correction to quantum yield

C***remove wavelength dependence

                     QDUM = QYZ( LAYS, IWL, IPHOT )
     &                    / ( 5.5 + 5.0 * 9.2E-19 * DENS( LAYS ) )
     &                    *  118.4 ! or   (5.5 + 5.0*9.2E-19*2.465E+19)
                     QDUM = MIN( QDUM, 1.0 )

                     QYZ( LAYS, IWL, IPHOT )  = QDUM
                     QYZ( LAYS, IWL, IPHOT ) = MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) )
                     QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, QYZ( LAYS, IWL, IPHOT ) )

                  CASE ( 'MEK-06', 'MEK_06' )

C***Quantum Yields from
C***  Raber, W.H. (1992) PhD Thesis, Johannes Gutenberg-Universitaet, Mainz, Germany.
C***  other channels assumed negligible (less than 10%).
C***  Total quantum yield  = 0.38 at 760 Torr. but Carter
C***  adjusts to 0.175 based on chamber tests and sets the values in
C***  mechanism definition file.
C***  NOTE: SAPRC07T includes this quantum yield in the mechanism definition
C***        file as factor times the photolysis rates where the quantum yield is
C***        set one.

C***temperature/density correction to quantum yield

C***Stern-Volmer form given:  1/phi = 0.96 + 2.22e-3*P(torr)
C***  compute local pressure in torr
                     PRESSURE = ( 1.03547E-19 * DENS( LAYS ) * XTEMP ) ! TORR

                     IF ( PRESSURE  .LT. 181.0 ) THEN
C***the factor 2.645 normalizes at surface and allows mech.def adjustment
                        QDUM =  2.645 / ( 0.96 + 2.22E-3 * ( 181.0 ) )
                     ELSE
                        QDUM =  2.645 / ( 0.96 + 2.22E-3 * PRESSURE )
                     END IF

                     QYZ( LAYS, IWL, IPHOT ) = QDUM
                     QYZ( LAYS, IWL, IPHOT ) = MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) )
                     QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, QYZ( LAYS, IWL, IPHOT ) )

                  CASE ( 'MGLY-06' , 'BACL-07', 'MGLY_06' , 'BACL_07', 'MGLY_IUPAC04' )

C***temperature/density correction to quantum yield

                     PRESSURE = ( 1.03547E-19 * DENS( LAYS ) * XTEMP ) ! in TORRs
                     PRESSURE = MIN( 472.0, PRESSURE )
                     QYZ( LAYS, IWL, IPHOT ) = MIN( QYZ( LAYS, IWL, IPHOT ), 1.0 )
                     QYZ( LAYS, IWL, IPHOT ) = MAX( QYZ( LAYS, IWL, IPHOT ), 0.0 )

C***Pressure dependence based on Koch and Moortgat (1998),
C***  J. Phys. Chem. A, vol 102, pages 9142. The application contradicts
C***  NASA (2006) & IUPAC (2005) and is used based recommendations for
C***  SAPRC07T photolysis rates by William Carter (2009)

                     IF ( ( WAVELENGTH( IWL ) .LT. 500.0 ) .AND.
     &                    ( WAVELENGTH( IWL ) .GT. 240.0 ) ) THEN

                        IF ( ( QYZ( LAYS, IWL, IPHOT ) .GT. 0.0 ) .AND.
     &                       ( QYZ( LAYS, IWL, IPHOT ) .LT. 1.0 ) ) THEN
C***remove wavelength dependence
                           QDUM = 1.36E8 * ( 472.0 )
     &                          / ( 1.0 / QYZ( LAYS, IWL, IPHOT ) - 1.0 )
                           QDUM = QDUM
     &                          / ( QDUM + 1.36E8 * PRESSURE )
                        ELSE
                           QDUM = QYZ( LAYS, IWL, IPHOT )
                        END IF

                     ELSE IF ( WAVELENGTH( IWL ) .LE. 240.0 ) THEN
                        QDUM = QYZ( LAYS, IWL, IPHOT )
                     ELSE IF ( WAVELENGTH( IWL ) .GE. 500.0 ) THEN
                        QDUM = 0.0
                     END IF

                     QYZ( LAYS, IWL, IPHOT ) = QDUM
                     QYZ( LAYS, IWL, IPHOT ) = MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) )
                     QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, QYZ( LAYS, IWL, IPHOT ) )

                  CASE (  'ACRO-09', 'ACRO_09' )

C***density correction quantum yield

                     QYZ( LAYS, IWL, IPHOT ) = MIN( QYZ( LAYS, IWL, IPHOT ), 1.0 )
                     QYZ( LAYS, IWL, IPHOT ) = MAX( QYZ( LAYS, IWL, IPHOT ), 0.0 )

C***Number density dependence based on Gardner et. al (1997),
C***  J. Phys. Chem., vol 91, pages 1922. The application uses
C***  the quantum yields set in in cross-section file. For
C***  SAPRC07T, yields set approximation four times NASA (2006)
C***  because the mechanism developer sums over all possible channels and
C***  Gardner et. al may support this conclusion.

                     IF ( DENS( LAYS ) .GE. 8.0E+17 ) THEN
                        QDUM = ( 4.0E-3 + 1.0 / ( 8.6E-2 + 1.613E-17 * DENS( LAYS ) ) )
     &                       /  0.006384
                     ELSE IF ( DENS( LAYS ) .LT. 8.0E+17 ) THEN
                        QDUM = 12.00713
                     END IF

                     QYZ( LAYS, IWL, IPHOT ) = QYZ( LAYS, IWL, IPHOT ) * QDUM
                     QYZ( LAYS, IWL, IPHOT ) = MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) )
                     QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, QYZ( LAYS, IWL, IPHOT ) )

                  CASE ( 'HCHOM-06', 'HCHOM_06', 'HCHO_MOL_RACM2' )  ! 'CH2O -> H2 + CO'

C***temperature/density correction to quantum yield

                     IF ( LHCHOR_06 .GT. 0 ) THEN

                        IF ( ( WAVELENGTH( IWL ) .GE. 330.0 ) .AND.
     &                       ( QYZ( LAYS, IWL, IPHOT ) .GT. 0.0 ) ) THEN

                           QDUM = 1.0 / QYZ( LAYS, IWL, IPHOT )        ! need to subst actual value in QY
                           BETA = 1.0 / ( 1.0 - QYZ( LAYS, IWL, LHCHOR_06 ) )  ! need to subst actual value in QY

                           IF ( XTEMP .LT. 300.0 .AND. XTEMP .GT. 220.0 ) THEN
                               PRESSURE = 82.06 * ( DENS( LAYS ) / 6.02E+23 ) * XTEMP   ! pressure units, atm
                               ALPHA = ( QDUM - BETA )
     &                               * ( 1.0 + 0.05 * ( WAVELENGTH( IWL ) - 329.0 )
     &                                 * ( ( XTEMP - 80.0 ) / 80.0 ) )
                           ELSE IF ( XTEMP .LE. 220.0 ) THEN
                               PRESSURE = 3.0E-20 * DENS( LAYS )
                               ALPHA = ( QDUM - BETA )
     &                               * ( 1.0 + 0.0875 * ( WAVELENGTH( IWL ) - 329.0 ) )
                           ELSE IF ( XTEMP .GE. 300.0 ) THEN
                               PRESSURE = 4.09E-20 * DENS( LAYS )
                               ALPHA = ( QDUM - BETA )
     &                               * ( 1.0 + 0.1375 * ( WAVELENGTH( IWL ) - 329.0 ) )
                           END IF

                           QYZ( LAYS, IWL, IPHOT )  = 1.0 / ( BETA + PRESSURE * ALPHA )

C***reduce wavelength dependence

                           QDUM = ( BETA + 1.001 * ALPHA )
     &                          / ( BETA + PRESSURE * ALPHA )
                           QYZ( LAYS, IWL, IPHOT )  = QDUM * QYZ( LAYS, IWL, IPHOT )

                        ELSE

                           QYZ( LAYS, IWL, IPHOT )  = QYZ( LAYS, IWL, IPHOT )

                        END IF

                        QYZ( LAYS, IWL, IPHOT ) = MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) )
                        QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, QYZ( LAYS, IWL, IPHOT ) )

                     END IF

                  CASE (  'ACET-06' , 'ACET_06', 'ACETONE', 'CH3COCH3_RACM2' ) ! 'CH3COCH3 -> products'

C***temperature/density correction to quantum yield

                     IF ( IPHOT .EQ. LACETONE .AND. NWL .EQ. NWL_ACETONE_FJX ) THEN

C***OPTT is the effective cross section ( Cs * QY )
C***This is an implementation of eq(21) of Cameron-Smith (2000)
C***special information for acetone
C***  Reference:
C***              Cameron-Smith, P., Incorporation of non-linear
C***              effective cross section parameterization into a
C***              fast photolysis computation  code (Fast-J)
C***              Journal of Atmospheric Chemistry, Vol. 37,
C***              pp 283-297, 2000.

                        OPTT = ( ( TTX2 - XTEMP ) * OP0( 1, IWL )
     &                         + ( XTEMP - TTX1 ) * OP0( 2, IWL ) )
     &                       / ( DTTX * ( 1.0 + YY30( IWL ) * DENS( LAYS ) ) )
                        CSZ( LAYS, IWL, LACETONE ) = OPTT
                        QYZ( LAYS, IWL, LACETONE ) = 1.0

                     ELSE

                        QYZ( LAYS, IWL, IPHOT ) = QY_ACETONE( XTEMP, DENS( LAYS ), WAVELENGTH( IWL ) )
                        QYZ( LAYS, IWL, IPHOT ) = MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) )
                        QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, QYZ( LAYS, IWL, IPHOT ) )

                     END IF

                  CASE (  'KETONE'  )

                     CSZ( LAYS, IWL, IPHOT ) = CSZ( LAYS, IWL, IPHOT )
     &                                       * 1.0 / ( 1.0 + 0.80E-19 * DENS( LAYS ) )

                  CASE ( 'KET_RACM2', 'HKET_RACM2', 'MEK_RACM2', 'MVK_RACM2' ) ! Ketone treatement from W. Stockwell sbox

                     IF( QYZ( LAYS, IWL, IPHOT ) .GT. 1.0E-5  )THEN
                         QYZ( LAYS, IWL, IPHOT ) =  1.0 
     &                                           / (1.0 + 4.057E-20*DENS(LAYS)
     &                                           * (1.0/QYZ(LAYS, IWL, IPHOT )-1.0))
                         QYZ( LAYS, IWL, IPHOT ) = MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) )
                         QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, QYZ( LAYS, IWL, IPHOT ) )
                     ELSE
                         QYZ( LAYS, IWL, IPHOT ) = 0.0
                     END IF

                  CASE ( 'GLYH2_RACM2', 'GLYF_RACM2', 'GLYHX_RACM2', 'MGLY_RACM2'  ) ! glyoxal treatement from W. Stockwell sbox

                     IF( QYZ( LAYS, IWL, IPHOT ) .GT. 1.0E-5  )THEN
                         QYZ( LAYS, IWL, IPHOT ) =  1.0 
     &                                           / (1.0 + 4.057E-20*DENS(LAYS)
     &                                           * (1.0/QYZ(LAYS, IWL, IPHOT )-1.0))
                         QYZ( LAYS, IWL, IPHOT ) = MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) )
                         QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, QYZ( LAYS, IWL, IPHOT ) )
                     ELSE
                         QYZ( LAYS, IWL, IPHOT ) = 0.0
                     END IF

                  CASE (  'MGLY_ADJ',  'MGLY_ABS' )

                     CSZ( LAYS, IWL, IPHOT ) = CSZ( LAYS, IWL, IPHOT )
     &                                       * 1.0 / ( 1.0 + 1.67E-19 * DENS( LAYS ) )

                  END SELECT

            END DO     ! loop on IWL
         END DO     ! loop on IPHOT

      END DO     ! loop on LAYS

      RETURN
      END SUBROUTINE GETCSQY

C///////////////////////////////////////////////////////////////////////

      REAL FUNCTION QY_ACETONE( TEMP, DENS_NUMB, LAMBDA )

!-----------------------------------------------------------------------
! Computes acetone quantum yields according to:
! IUPAC (2005) recommendation based on
! Blitz, M. A., D. E. Heard, M. J. Pilling, S. R. Arnold, and M. P. Chipperfield
!       (2004), Pressure and temperature-dependent quantum yields for the
!       photodissociation of acetone between 279 and 327.5 nm, Geophys.
!       Res. Lett., 31, L06111, doi:10.1029/2003GL018793.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!***arguments

      REAL, INTENT(IN) :: TEMP        ! air temperature, K
      REAL, INTENT(IN) :: DENS_NUMB   ! air number density, 1/cm^3
      REAL, INTENT(IN) :: LAMBDA      ! wavelength, nm

!***local

      REAL :: A0           ! 1st coef for qy
      REAL :: A1           ! 2nd coef for qy
      REAL :: A2           ! 3rd coef for qy
      REAL :: A3           ! 4th coef for qy
      REAL :: A4           ! 5th coef for qy
      REAL :: A5           ! 6th coef for qy
      REAL :: A6           ! 7th coef for qy

      REAL :: PHI_CO       ! CO branch of IUPAC (2005) acetone QYZ
      REAL :: PHI_CH3CO    ! CH3CO branch of IUPAC (2005) acetone QYZ
      REAL :: AA           ! scratch variable for IUPAC (2005) acetone QYZ
      REAL :: BB           ! scratch variable for IUPAC (2005) acetone QYZ
      REAL :: CC           ! scratch variable for IUPAC (2005) acetone QYZ

      REAL :: TEMP_ARGUE   ! temperature over 295 K
      REAL :: INV_LAMBDA   ! reciprocal of wavelength, 1.E+7/nm

      REAL, PARAMETER :: T295K = 295

      TEMP_ARGUE = T295K / TEMP
      INV_LAMBDA = 1.0E+7 / LAMBDA

      IF ( LAMBDA .GE. 248.0 .AND. LAMBDA .LE. 349.0 ) THEN

         AA = 0.350 * TEMP_ARGUE**(1.28)
         BB = 0.068 * TEMP_ARGUE**(2.65)
         A0 = ( AA / ( 1.0 - AA ) ) * EXP( BB * ( LAMBDA - 248.0 ) )
         PHI_CO = 1.0 / ( 1.0 + A0 )

         IF ( LAMBDA .LE. 302.0 ) THEN

!***wavelengths 248-302 nm

            AA = 1.600 * 1.0E-19 * TEMP_ARGUE**(2.38)
            BB =  0.55 * 1.0E-03 * TEMP_ARGUE**(3.19)
            A1 = AA * EXP( -BB * ( INV_LAMBDA - 33113.0 ) )
            PHI_CH3CO = ( 1.0 - PHI_CO ) / ( 1.0 + A1*DENS_NUMB )

!***wavelengths 302-349 nm

         ELSE

            AA = 1.62 * 1.0E-17 * TEMP_ARGUE**(10.03)
            BB = 1.79 * 1.0E-3  * TEMP_ARGUE**(1.364)
            A2 = AA * EXP( -BB * ( INV_LAMBDA  - 30488.0 ) )

            AA = 26.29 * TEMP_ARGUE**(6.59)
            BB = 5.72 * 1.0E-7 * TEMP_ARGUE**(2.93)
            CC =   30006.0     * TEMP_ARGUE**(0.064)
            A3 = AA * EXP( -BB * (INV_LAMBDA - CC )**2.0 )

            AA = 1.67 * 1.0E-15 * TEMP_ARGUE**(7.25)
            BB = 2.08 * 1.0E-3  * TEMP_ARGUE**(1.16)
            A4 = AA * EXP( -BB * ( INV_LAMBDA - 30488.0 ) )

            PHI_CH3CO = ( 1.0 - PHI_CO )
     &                * ( 1.0 + A4 * DENS_NUMB + A3 )
     &                / ( ( 1.0 + A2 * DENS_NUMB + A3 )
     &                * ( 1.0 + A4 * DENS_NUMB ) )
         END IF

         QY_ACETONE = PHI_CO + PHI_CH3CO

      ELSE IF ( LAMBDA .LT. 248.0 .AND. LAMBDA .GT. 0.0 ) THEN ! set QY to 1.0

!***based on IUPAC (2005) data sheet

         PHI_CO    = 0.05
         PHI_CH3CO = 0.95
         QY_ACETONE = PHI_CO + PHI_CH3CO

      ELSE IF ( LAMBDA .GT. 349.0 ) THEN

         QY_ACETONE = 0.0

      END IF

      QY_ACETONE = MAX( 0.0, MIN( 1.0, QY_ACETONE ) )

      RETURN
      END FUNCTION QY_ACETONE

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE GET_TAUO3 ( IWL, STOZONE, STRAT_TEMP, TAU_O3 )
C-----------------------------------------------------------------------
C  subroutine to calculate the optical depth of ozone in the
C     stratosphere
C
C  special cross sections for calculating stratospheric ozone
C     optical depth
C
C  the following temperatures and cross sections are from
C     Fast-J
C     REFERENCE:
C     Wild, O., X. Zhu, and M.J. Prather, Fast-J: Accurate simulation
C     of in- and below-clolud photolysis in tropospheric chemical
C     models,
C     Journal of Atmospheric Chemistry, Vol. 37, pp 245-282, 2000
C
C  coded 10/20/2004 by Dr. Francis S. Binkowski
C     Carolina Environmental Program
C     University of North Carolina at Chapel Hill
C     email: frank_binkowski@unc.edu
C     Updated to Fast-JX version 5.0
C  Mar 2011 Bill Hutzell
C     revised interpolation method for a general number of
C     interpolation points
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

C***arguments

      INTEGER, INTENT(IN) :: IWL       ! wavelenth index

      REAL, INTENT(IN)  :: STOZONE     ! ozone column amount [ DU ]
      REAL, INTENT(IN)  :: STRAT_TEMP  ! average temperature for stratosphere [ K ]
      REAL, INTENT(OUT) :: TAU_O3      ! optical depth for statosphere

C***Local

      REAL, PARAMETER :: DU_TO_CONC = 2.6879E16  ! conversion factor from [ DU ]
                                                 ! to [ molecules / cm ** 2 ]

      INTEGER IXT, IXTEMP

      REAL OZONE_CS        ! interpolated ozone absorption cross section
      REAL YTT             ! interpolation variable

C***Find temperature range:

      IF ( STRAT_TEMP .LE. TEMP_O3_STRAT( 1 ) ) IXTEMP = 0

      DO IXT = 1, NTEMP_STRAT - 1
         IF ( STRAT_TEMP .GT. TEMP_O3_STRAT( IXT ) .AND.
     &        STRAT_TEMP .LT. TEMP_O3_STRAT( IXT + 1 ) ) THEN
              IXTEMP = IXT
              YTT = ( STRAT_TEMP - TEMP_O3_STRAT( IXT ) )
     &            / ( TEMP_O3_STRAT( IXT + 1 ) - TEMP_O3_STRAT( IXT ) )
         END IF
      END DO

      IF ( STRAT_TEMP .GE. TEMP_O3_STRAT( NTEMP_STRAT ) ) THEN
         IXTEMP = NTEMP_STRAT
         YTT = 0.0
      END IF

C***do linear interpolation

      IF ( IXTEMP .EQ. 0 ) THEN
         OZONE_CS = XO3CS( 1, IWL )
      ELSE IF ( IXTEMP .GE. 1 .AND. IXTEMP .LT. NTEMP_STRAT ) THEN
         OZONE_CS = XO3CS( IXTEMP, IWL ) +
     &            ( XO3CS( IXTEMP+1, IWL ) - XO3CS( IXTEMP, IWL ) ) * YTT
      ELSE IF ( IXTEMP .EQ. NTEMP_STRAT ) THEN
         OZONE_CS = XO3CS( IXTEMP, IWL )
      END IF

      TAU_O3 = DU_TO_CONC * STOZONE * OZONE_CS

      RETURN
      END SUBROUTINE GET_TAUO3

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE O3AMT ( XLAT, XLONG, MDAY, OZONE )
C-----------------------------------------------------------------------
C  This subroutine implements an algorithm for the annual behavior
C     of total ozone ( taken here to be stratospheric) from
C     climatology
C  Reference:
C     Van Heuklon, Thomas K., Estimating atmospheric ozone for solar
C     radiation models, Solar Energy, Vol. 22, pp 63-68, 1979.
C  updated from an earlier version by
C     Dr. Francis S. Binkowski, The Carolina Environmental Program,
C     The University of North Carolina at Chapel Hill.
C     Email: frank_binkowski@unc.edu
C     November 03. 2004.
C  Only Northern Hemisphere is implemented.
C-----------------------------------------------------------------------

      IMPLICIT NONE

C***arguments

      INTEGER, INTENT(IN) :: MDAY  ! Day number during the year
                                   ! Jan 1st = 1.0, Feb 1st = 32, etc.

      REAL, INTENT(IN)  :: XLAT  ! latitude of point on earth's surface
      REAL, INTENT(IN)  :: XLONG ! longitude of point on earth's surface
      REAL, INTENT(OUT) :: OZONE ! Total column amount of ozone [ DU ]

C***Internal:

C***The following parameters are from Table 1 of Van Heuklon (1979).

      REAL, SAVE ::  A, B, C, D, F, G, H, FJ
      DATA A/150.0/, B/1.28/, C/40.0/, D/0.9865/, F/-30.0/, G/20.0/,
     &     H/3.0/, FJ/235.0/

C***FSB FJ is the equatorial annual average of atmospheric ozone
C***  content, as noted on page 65 of Nav Heulklon (1979). This value
C***  sets the basic background for ozone.

      REAL, PARAMETER :: RD = 0.017453   ! degrees to radians

C***Variables of convenience

      REAL E, FI, BPHI, DEF, HLI, SINB, SINB2

C***set the day

      E = FLOAT( MDAY )
      FI = 20.0
      IF ( XLONG .LT. 0.0 ) FI = 0.0
      BPHI  = B * XLAT * RD
      DEF   = D * ( E + F ) * RD
      HLI   = H * ( XLONG + FI ) * RD
      SINB  = SIN( BPHI )
      SINB2 = SINB * SINB

C***the following equation implements equation (4) of VanHeuklon (1979)

      OZONE  = FJ + ( A + C * SIN( DEF ) + G * SIN( HLI ) ) * SINB2

      RETURN
      END SUBROUTINE O3AMT

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE SOLEFM3 ( YEAR, DAY, EQT, SINDEC, COSDEC, SOLDIST )
C-----------------------------------------------------------------------
C  FSB Coded September 9, 2004 by Dr. Francis S. Binkowski
C     Environmental Modeling for Policy Development group,
C     The Carolina Environmental Program
C     The University of North Carolina-Chapel Hill
C     Email: frank_binkowski@unc.edu
C
C  REFERENCE:The code is a modification of that  from
C     J.J. Michalsky, The Astronomical Almanac's algorithm for
C     approximate solar position (1950-2050)
C     Solar Energy vol. 40, No. 3, pp 227-235, 1988.
C     who based it upon
C
C     The Astronomical Almanac, U.S. Gov't. Printing Office,
C     Washington DC. Page c24 has the algorithm
C
C  The stated accuracy is 0.01 degree for the epoch ( 1950 - 2050).
C
C  This subroutine calculates the sine and cosine of the solar
C     declination and the equation of time, and solar distance
C     using an approximation to equations used to generate the
C     tables in  The Astronomical Almanac.
C
C  input:
C     year = year (e.g. 1998)
C     day  = day of the year (e.g. Feb. 2 is 33)
C     lat  = latitude in degrees (north is positive)
C     long = longitude in degrees (east is positive)
C
C  output:
C     EQT    = Equation of Time
C     sindec = sine of the solar declination
C     cosdec = cosine of the solar declination
C     soldist = distance of sun in astronomical units
C-----------------------------------------------------------------------

      IMPLICIT NONE

C***arugments

      REAL, INTENT(IN)  :: YEAR, DAY
      REAL, INTENT(OUT) :: EQT, SINDEC, COSDEC, SOLDIST

C***parameters

!!!from above      REAL, PARAMETER :: PI = 3.1415927
      REAL, PARAMETER :: TWOPI = 2.0 * PI
!!!from above      REAL, PARAMETER :: RAD   = PI / 180.0
      REAL, PARAMETER :: RAD   = PI180
      REAL, PARAMETER :: RADM1 = 1.0 / RAD

C***FSB inline documentation defines the variables.

      REAL DELTA, LEAP, JD, TIME, L, G, LAMDA, EPSILON
      REAL ALPHA
      REAL SING, SIN2G, COSG, COS2G
      REAL(8) NUM, DEN

C***start code
C***get current julian date, jd
C***  the offset year of 1949 is for convenience of having all
C***  positive numbers for the epoch (1950 - 2050)

      DELTA = YEAR - 1949.0
      LEAP  = AINT( DELTA / 4.0 )

C***jd = 32916.5 + 365.0 * delta + leap + hour / 24.0
C***set hour = 12.0 and 12.0 / 24.0 -> 0.5

      JD = 2432916.5 + 365.0 * DELTA + LEAP + DAY + 0.5

C***first number is jd for midnight 0 Jan 1949
C***  leap is leap days since 1949.

C***  calculate ecliptic conditions:

      TIME = JD - 2451545.0     ! 51545.0 is noon Jan 1, 2000

C***calculate mean solar longitude (L)

      L = 280.460 + 0.985674 * TIME ! mean solar longitude

C***now force L to be between 0.0 and 360. degrees

      L = MODULO( L, 360.0 )
      IF ( L .lt. 0.0 ) L = L + 360.0

C***calculate mean anomaly (g) in radians between 0 and twopi

      G = 357.528 + 0.9856003 * TIME
      G = MODULO( G, 360.0 )
      IF ( G .LT. 0.0 ) G = G + 360.0

      G = RAD * G    ! in radians now

C***calculate trig functions of g using identities
C***  this speeds up the calculations

      SING = SIN( G )
      COSG = COS( G )
      SIN2G = 2.0 * SING * COSG
      COS2G = COSG * COSG - SING * SING

C***calculate ecliptic longitude ( Lamda) and
C***  obliquity (epsilon) of ecliptic in radians

      LAMDA = L +  1.915 * SING  + 0.020 * SIN2G
      LAMDA = MODULO( LAMDA, 360.0 )
      IF ( LAMDA .LT. 0.0 ) LAMDA = LAMDA + 360.0

C***calculate obliquity (epsilon)

      EPSILON = 23.429 - 4.0E-7 * TIME
      LAMDA   = RAD * LAMDA    ! in radians
      EPSILON = RAD * EPSILON  ! in radians

C***now calculate right ascension (Alpha) Use this method to get the
C***  correct quadrant.

      NUM = COS( EPSILON ) * SIN( LAMDA ) ! numerator
      DEN = COS( LAMDA )          ! denominator
      ALPHA = ATAN( NUM / DEN )

C***force right ascension to be between 0 and twopi

      IF ( DEN .LT. 0.0D0 ) THEN
         ALPHA = ALPHA + PI
      ELSE IF ( NUM .LT. 0.0D0 ) THEN
         ALPHA = ALPHA + TWOPI
      END IF

C***Alpha = atan2(num,den)
C***  Alpha = MOD(Alpha, 360.0)
C***  IF ( Alpha .lt. 0.0)  Alpha = Alpha + 360.0

      ALPHA = RADM1 * ALPHA     ! Alpha now in degrees

C***calculate the sine of the declination
C***  (sindec) cosine of the declination (cosdec)

      SINDEC = SIN( EPSILON ) * SIN( LAMDA )
      COSDEC = SQRT( 1.0 - SINDEC * SINDEC )

C***calculate the Equation of Time( EQT) in degrees.

      EQT = L - ALPHA
      IF ( EQT .GT. 350.0 ) EQT = EQT - 360.0 ! catch a problem

C***calculate solar distance

      SOLDIST = 1.00014 - 0.01671 * COSG - 0.00014 * COS2G

      RETURN
      END SUBROUTINE SOLEFM3

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE GETZEN2 ( TIME, SINLAT, COSLAT, SINDEC, COSDEC, EQT,
     &                     COSZEN )
C-----------------------------------------------------------------------
C  calculate the zenith angle information at the present time step
C
C  input:
C     time    = LST in decimal hours <<NOTE: LST
C     sinlat  = sine of the latitude
C     coslat  = cosine of the latitude
C     sindec  = sine of the solar declination
C     cosdec  = cosine of the solar declination
C     EQT     = equation of time in degrees
C
C  Output:
C     coszen  = cosine of the zenith angle
C
C  Revision history:
C     FSB Coded September 10, 2004 by Dr. Francis S. Binkowski
C     FSB modified February 15, 2005 to increase efficiency
C     Environmental Modeling for Policy Development group,
C     The Carolina Environmental Program
C     The University of North Carolina-Chapel Hill
C     Email: frank_binkowski@unc.edu
C
C  Note: because this routine will be called frequently
C     during a simulation, the sines and cosines of the
C     latitude should be calculated once at the beginning of
C     simulation.
C-----------------------------------------------------------------------

      IMPLICIT NONE

C***arguments

      REAL, INTENT(IN)  :: TIME, SINLAT, COSLAT
      REAL, INTENT(IN)  :: SINDEC, COSDEC, EQT
      REAL, INTENT(OUT) :: COSZEN

C***internal variables

      REAL HANGLE          ! local hour angle in degrees

!!!from above      REAL, PARAMETER :: PI = 3.1415927
!!!from above      REAL, PARAMETER :: RAD = PI / 180.0
      REAL, PARAMETER :: RAD = PI180

C***start code.

      HANGLE = 15.0 * ( TIME - 12.0 )  + EQT
      COSZEN = SINDEC * SINLAT + COSLAT * COSDEC * COS( RAD * HANGLE )

C***do not allow zenith angle to be lower than 1 degree (COSZEN = 0.999998)
C***when COSZEN is 1.0, gas-phase chemistry solver does not work properly 
      IF (COSZEN .GT. 0.999998) COSZEN = 0.999998

      RETURN
      END SUBROUTINE GETZEN2

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE SLANTPATH2 ( NLAYS, Z, ZSFC, REARTH, SINZEN, DZ, DSDH )
C-----------------------------------------------------------------------
C  PURPOSE:
C     Calculate slant path, ds/dh, over vertical depth in spherical
C     geometry also calculates the layer thicknesses.
C     NOTE!!!
C     This version is restricted to zenith angle less than 90 degrees
C-----------------------------------------------------------------------
C  ARGUMENTS:
C     INPUT:
C       NLAYS   - INTEGER, number of specified altitude levels
C       z       - REAL, altitude (agl) [m] <<<    meters
C       This is from file ZF ( full layers ) from METCRO3D
C       Z(1) is zero.
C       zsfc    - REAL, ground elevation (msl) [m]
C       rearth  - REAL, radius of the earth [m]
C       sinzen  - REAL, sine of solar zenith angle
C
C     OUTPUT:
C       dz      - REAL, layer thicknesses [ m ]
C       dsdh    - REAL, slant path of direct beam through each layer
C       when travelling from the top of the atmosphere downward
C-----------------------------------------------------------------------
C  EDIT HISTORY:
C     Inspired by sphers from TUV
C     09/08/2004 modified to specialize for CMAQ application
C     by Dr. Francis S. Binkowski
C     Environmental Modeling for Policy Development group,
C     The Carolina Environmental Program
C     The University of North Carolina-Chapel Hill
C     Email: frank_binkowski@unc.edu
C
C-----------------------------------------------------------------------
C  REFERENCE:
C     Dahlback, A. and K. Stamnes, A new spherical model for computing
C     the radiation field available for photolysis and heating at
C     twilight, Planetary and Space Sciences, Vol. 39, No. 5,
C     pp 671-683, 1991.
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

C***arguments

      INTEGER, INTENT(IN) :: NLAYS

      REAL, INTENT(IN)  :: Z( NLAYS + 1 ), ZSFC, REARTH, SINZEN
      REAL, INTENT(OUT) :: DZ  ( NLAYS )  ! layer thicknesses counting from surface upward
      REAL, INTENT(OUT) :: DSDH( NLAYS )

C***Internal

      INTEGER I, J, K           ! loop indices
      REAL RE
      REAL DSJ                  ! slant path length [m]
      REAL DHJ                  ! layer thickness [m]
      REAL(8) RJ, RJP1
      REAL(8) RPSINZ            ! rpsinz = (re + zd(i)) * sinzen
      REAL(8) RPSINZ2           ! rpsinz * rpsinz
      REAL(8) GA, GB            ! see usage
      REAL ZE( NLAYS + 1 )      ! altitudes MSL
      REAL ZD( NLAYS + 1 )      ! array of altitudes indexed from top
      REAL DZI( NLAYS )         ! layer thicknesses counting downward from the top

C-----------------------------------------------------------------------

C***re include the altitude above sea level to the radius of the earth

      RE = REARTH + ZSFC

C***ze is the altitude above msl

      DO K = 1, NLAYS + 1
         ZE( K ) = Z( K )
!!sjr          ZE(K) = Z(K) - ZSFC
      END DO

C***   DZ(1) = ZE(2) - ZE(1)
C***   DZI(1) = ZE(NLAYS + 1) - ZE(NLAYS)

C***calculate dz

      DO K = 1, NLAYS
         DZ( K ) = ZE( K + 1 ) - ZE( K )
      END DO

C***zd, dzi are inverse coordinates of ze & dz

      DO K = 1, NLAYS + 1
         J = NLAYS + 1 - K + 1
         ZD( J ) = ZE( K )
      END DO

      DO K = 1, NLAYS
         J = NLAYS + 1 - K
         DZI( J ) = DZ( K )
      END DO

C***initialize dsdh

      DO I = 1, NLAYS
         DSDH( I ) = 0.0
      END DO

C***FSB The following code is a direct implementation of appendix B
C***  of Dahlbeck and Stamnes (1991) for the case of solar zenith
C***  angle less than 90 degree.

C***calculate ds/dh of every layer starting at the top

      DO J = 1, NLAYS
C***  K = NLAYS - J +1
         RPSINZ  = ( RE + ZD( J ) ) * SINZEN
         RPSINZ2 = RPSINZ * RPSINZ

         IF ( J .LT. NLAYS ) THEN
            RJ   = RE + ZD( J )
            RJP1 = RE + ZD( J + 1 )
            DHJ  = DZI( J )
         ELSE
            RJ   = RE + ZD( J )
            RJP1 = RE
            DHJ  = DZI( J )
         END IF

C***define GA and GB

         GB = SQRT( MAX( 0.0D0, RJ * RJ     - RPSINZ2 ) )
         GA = SQRT( MAX( 0.0D0, RJP1 * RJP1 - RPSINZ2 ) )

C***This is equation B1 from Dahlbeck and Stamnes (1991)

         DSJ = ABS( GB - GA )

C***this is the slant path (Chapman) function.

         DSDH( J ) = DSJ / DHJ    ! Note dsdh is on a top to bottom grid.

      END DO   ! loop over altitude

      RETURN
      END SUBROUTINE SLANTPATH2

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE SLANTPATHTOP ( ZTOM, ZTOA, ZSFC, REARTH, SINZEN,
     &                          DSDHTOP )
C-----------------------------------------------------------------------
C  FSB This is a SPECIAL version to get the slant path from the top of
C    the modeling domain (ztom) to the top of the atmosphere (ztoa).
C-----------------------------------------------------------------------
C  PURPOSE:
C     Calculate slant path, ds/dh, over vertical depth in spherical
C     geometry also calculates the layer thicknesses.
C     NOTE!!!
C     This version is restricted to zenith angle less than 90 degrees
C-----------------------------------------------------------------------
C  ARGUMENTS:
C     INPUT:
C       ztom    - REAL, altitude (agl) of top of modeling domain [m] <<<meters
C       This is from file ZF ( full layers ) from METCRO3D
C       Z(1) is zero.
C       ztoa    - REAL altitude (msl) of top of atmosphere [ m ]
C       zsfc    - REAL, ground elevation (msl) [m]
C       rearth  - REAL, radius of the earth [m]
C       sinzen  - REAL, sine of solar zenith angle
C
C     OUTPUT:
C       dsdhtop   - REAL, slant path of direct beam through each layer
C       when travelling from the top of the atmosphere downward
C       to the top of top model
C-----------------------------------------------------------------------
C  EDIT HISTORY:
C     Inspired by sphers from TUV
C     09/08/2004 modified  to specialize for CMAQ application
C     11/11/2004 modified to do just the one layer from ztom to ztoa.
C     by Dr. Francis S. Binkowski
C     Environmental Modeling for Policy Development group,
C     The Carolina Environmental Program
C     The University of North Carolina-Chapel Hill
C     Email: frank_binkowski@unc.edu
C
C-----------------------------------------------------------------------
C  REFERENCE:
C     Dahlback, A. and K. Stamnes, A new spherical model for computing
C     the radiation field available for photolysis and heating at
C     twilight, Planetary and Space Sciences, Vol. 39, No. 5,
C     pp 671-683, 1991.
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

C***arguments

      REAL, INTENT(IN)  :: ZTOM, ZTOA, ZSFC, REARTH,  SINZEN
      REAL, INTENT(OUT) :: DSDHTOP

C***Internal

      INTEGER I, J, K           ! loop indices
      REAL RE
      REAL DSJ                  ! slant path length [m]
      REAL DHJ                  ! layer thickness [m]
      REAL(8) RJ, RJP1
      REAL(8) RPSINZ            ! rpsinz = (re + zd(i)) * sinzen
      REAL(8) RPSINZ2           ! rpsinz * rpsinz
      REAL(8) GA, GB            ! see usage

C-----------------------------------------------------------------------

C***re include the altitude above sea level to the radius of the earth

      RE = REARTH + ZSFC

!!sjr  DHJ = ZTOA - ZTOM
      DHJ = ZTOA - ( ZTOM + ZSFC )

C***FSB The following code is a direct implementation of appendix B
C***  of Dahlbeck and Stamnes (1991) for the case of solar zenith
C***  angle less than 90 degree.

C***  calculate ds/dh of every layer starting at the top

      RPSINZ  = ( REARTH + ZTOA ) * SINZEN
      RPSINZ2 = RPSINZ * RPSINZ

!!sjr  RJ = RE + ZTOA
      RJ = REARTH + ZTOA
      RJP1 = RE + ZTOM

C***define GA and GB

      GB = SQRT( MAX( 0.0D0, RJ * RJ     - RPSINZ2 ) )
      GA = SQRT( MAX( 0.0D0, RJP1 * RJP1 - RPSINZ2 ) )

C***This is equation B1 from Dahlbeck and Stamnes (1991)

      DSJ = GB - GA

C***this is the slant path (Chapman) function.

      DSDHTOP = DSJ / DHJ

      RETURN
      END SUBROUTINE SLANTPATHTOP

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE TWOSTREAM_S ( NLEVEL, MU, RSFC, TAUU, OMU, GU, DSDH,
     &                         FDR, FUP, FDN, EDR, EUP, EDN )
C-----------------------------------------------------------------------
C  PURPOSE:
C     Solve two-stream equations for multiple layers.  The subroutine is
C     based on equations from:  Toon et al., 1989.
C     It contains only the Delta Eddington method.
C     A pseudo-spherical correction has also been added.
C     FSB This version is restricted to solar zenith angle LESS THAN 90
C     degrees
C-----------------------------------------------------------------------
C  ARGUMENTS:
C     INPUT:
C       nlevel - INTEGER, number of specified altitude levels in the
C                working grid
C       mu     - REAL, cosine of solar senith angle
C       rsfc   - REAL, surface albedo at current wavelength
C       tauu   - REAL, unscaled optical depth of each layer
C       omu    - REAL, unscaled single scattering albedo of each layer
C       gu     - REAL, unscaled asymmetry parameter of each layer
C       dsdh   - REAL, slant path of direct beam through each layer
C                crossed when travelling from the top of the atmosphere
C                to layer
C     OUTPUT:
C       fdr - REAL, contribution of the direct component to the total
C             actinic flux at each altitude level
C       fup - REAL, contribution of the diffuse upwelling component to
C             the total actinic flux at each altitude level
C       fdn - REAL, contribution of the diffuse downwelling component to
C             the total actinic flux at each altitude level
C       edr - REAL, contribution of the direct component to the total
C             spectral irradiance at each altitude level
C       eup - REAL, contribution of the diffuse upwelling component to
C             the total spectral irradiance at each altitude level
C       edn - REAL, contribution of the diffuse downwelling component to
C             the total spectral irradiance at each altitude level
C-----------------------------------------------------------------------
C  EDIT HISTORY:
C     This is a modification of ps2str.f from TUV
C     this routine has been modified from the original TUV code by
C     Dr. Francis S. Binkowski, Carolina Environmental Program
C     09/2004 removed various two-stream methods (FSB)
C     09/2004 made mu an input (FSB)
C     09/2004 simplified for case of solar zenith angle less than
C             90 degrees
C-----------------------------------------------------------------------
C  References:
C
C     Joseph, J.H., W.J. Wiscombe, and J.A. Weinman, The delta-Eddington
C     Approximation for radiative flux transfer, Jour. Atmos. Res.,
C     Vol.33, No. 12, pages 2452 - 2459, December , 1976.
C     (the method implemented here)
C
C     Toon, O.B., C.P. McKay, T.P. Ackerman, and K. Santhanam, Rapid
C     calculation of radiative heating rates and photodissociation rates
C     in inhomogeneous multiple scattering atmospheres, J. Geophys. Res.
C     Vol. 94, No. D13, Pages 16,287 - 16,301, November 20, 1989.
C     (all citations for equation numbers and page numbers are to this
C     reference)
C
C     Zeng, J., S. Madronich, and K. Stamnes, A note on the use of the
C     two-stream delta-scaling approximation for calculating atmospheric
C     photolysis rate coefficients, Journal of Geophysical Research,
C     vol 101, D9, pp 14,525 - 14530, June 20, 1996.
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, PARAMETER :: KZ = 100
      INTEGER, PARAMETER :: NROWS = 2 * KZ

C***arguments

      INTEGER, INTENT(IN) :: NLEVEL

      REAL, INTENT(IN)  :: MU, RSFC
      REAL, INTENT(IN)  :: TAUU( NLEVEL ), OMU( NLEVEL ), GU ( NLEVEL )
      REAL, INTENT(IN)  :: DSDH( NLEVEL )
      REAL, INTENT(OUT) :: FUP ( NLEVEL ), FDN( NLEVEL ), FDR( NLEVEL )
      REAL, INTENT(OUT) :: EUP ( NLEVEL ), EDN( NLEVEL ), EDR( NLEVEL )

C***local:

      REAL TAUC  (0:KZ)        ! optical depth variable
      REAL TAUSLA(0:KZ)        ! slant path optical depth
      REAL MU2   (0:KZ)        ! replaces mu1 for slant path

C***internal coefficients and matrix

      INTEGER ROW
      REAL LAM(KZ), TAUN(KZ), BGAM(KZ)
      REAL E1(KZ), E2(KZ), E3(KZ), E4(KZ)
      REAL CUP(KZ), CDN(KZ), CUPTN(KZ), CDNTN(KZ)
      REAL MU1                  ! constant for delta_Eddington method
      REAL A(NROWS), B(NROWS), D(NROWS), E(NROWS), Y(NROWS)

C***other:

      REAL PIFS, FDN0
      REAL GI(KZ), OMI(KZ), TEMPG
      REAL F, G, OM
      REAL GAM1, GAM2, GAM3, GAM4

      REAL EXPON, EXPON0, EXPON1, DIVISR, TEMP, UP, DN
      REAL SSFC
      INTEGER NLAYER, MROWS, LEV

      INTEGER I, J

C***Some additional program constants:

!!!!from above      REAL, PARAMETER :: PI          = 3.1415926535898  ! pi
      REAL, PARAMETER :: LARGEST     = 1.0E+36 ! largest machine number
      REAL, PARAMETER :: SMALLEST    = 1.0E-36 ! largest machine number
      REAL, PARAMETER :: SQRTLRGST   = 1.0E+18 ! sqrt(largest)
      REAL, PARAMETER :: SQRTLRGSTM1 = 1.0E-18 ! 1/sqrt(largest)
      REAL, PARAMETER :: EPS         = 1.0E-3
      REAL, PARAMETER :: PRECIS      = 1.0E-7

C-----------------------------------------------------------------------

C***boundary conditions:

      PIFS = 1.0                ! solar flux is set to unity here.
      FDN0 = 0.0                ! no downward diffuse flux

      NLAYER = NLEVEL - 1

      DO J = 0, KZ
         TAUC  (J) = 0.0
         TAUSLA(J) = 0.0
         MU2   (J) = SQRTLRGSTM1
      END DO

C***scaling for delta-Eddington approximation

C***start diagnostic prinout

      DO I = 1, NLAYER
         F = GU(I) * GU(I)
         GI  (I) = ( GU(I) - F ) / ( 1.0 - F )
         OMI (I) = ( 1.0 - F ) * OMU(I) / ( 1.0 - OMU(I) * F )
         TAUN(I) = ( 1.0 - OMU( I ) * F ) * TAUU(I)
      END DO

C***set  tausla to be slant optical path contribution for each layer.

      DO I = 1, NLAYER
         TAUSLA(I) = TAUN(I) * DSDH(I)
      END DO

      TAUC(1)   = TAUN(1)

      DO I = 1, NLAYER
         TAUN(I)   = TAUN(I)
         TAUSLA(I) = TAUSLA(I-1) + TAUSLA(I) ! NOTE redefinition of tausla
                                             ! to be a sum over altitude.
         TAUC(I)   = TAUC(I-1) + TAUN(I)

C***FSB calculate MU2(i). This is the substitute for mu ( = 1/ coszen) for the
C***  pseudo spherical approximation. It is ther ratio of vertical optical
C***  depth to the slant optical depth.
C***  This has been simplified from TUV ps2str because only
C***  zenith angles < or = 90 degrees are considered.

         IF ( TAUSLA(I) .EQ. TAUSLA(I-1) ) THEN
            MU2(I) = SQRTLRGST
         ELSE
            MU2(I) = (TAUC(I) - TAUC(I-1)) / (TAUSLA(I) - TAUSLA(I-1))
            MU2(I) = SIGN( MAX( ABS( MU2(I)), SQRTLRGSTM1 ), MU2(I) )
         END IF
      END DO

C***compute coefficients for each layer:
C***  gam1 - gam4 = 2-stream coefficients
C***  expon0 = calculation of e when tau is zero
C***  expon1 = calculation of e when tau is taun
C***  cup and cdn = calculation when tau is zero
C***  cuptn and cdntn = calc. when tau is taun
C***  divisr = prevents division by zero

      DO 11, I = 1, NLAYER

         G = GI(I)
         OM = OMI(I)

C***stay away from 1 by precision.  For g, also stay away from -1

         TEMPG = MIN( ABS( G ), 1.0 - PRECIS )
         G = SIGN( TEMPG, G )
         OM = MIN( OM, 1.0 - PRECIS )

C***calculate the gamma values from line 1 Table 1, page 16,289

         GAM1 =  ( 7.0 - OM * ( 4.0 + 3.0 * G ) ) * 0.25
         GAM2 = -( 1.0 - OM * ( 4.0 - 3.0 * G ) ) * 0.25
         GAM3 =  ( 2.0 - 3.0 * G * MU ) * 0.25
         GAM4 =  1.0 - GAM3
         MU1  =  0.5

C***lambda = pg 16,290 equation 21
C***big gamma = pg 16,290 equation 22

         IF( ABS( GAM2 ) .LE. SMALLEST )THEN
            WRITE( NEW_OPTICS_LOG, 2609)I, GI(I), OMI(I), G, OM, GAM1, GAM2
            WRITE( NEW_OPTICS_LOG, 2610)
         ENDIF

         IF( ( GAM1*GAM1 - GAM2*GAM2 ) .LT. 0.0 )THEN
            WRITE( NEW_OPTICS_LOG, 2609)I, GI(I), OMI(I), G, OM, GAM1, GAM2
            WRITE( NEW_OPTICS_LOG, 2611)
         ENDIF
 
     
         LAM(I) = SQRT( GAM1*GAM1 - GAM2*GAM2 )

         IF( ABS( GAM2 ) .LE. SMALLEST )THEN 
C***adjustment based on NCAR TUV model
            BGAM(I) = 0.0
         ELSE
            BGAM(I) = ( GAM1 - LAM(I) ) / GAM2
         ENDIF

         EXPON = EXP( -LAM(I) * TAUN(I) )

C***e1 - e4 = pg 16,292 equation 44

         E1(I) = 1.0 + BGAM(I) * EXPON
         E2(I) = 1.0 - BGAM(I) * EXPON
         E3(I) = BGAM(I) + EXPON
         E4(I) = BGAM(I) - EXPON

C***the following sets up for the C equations 23, and 24
C***  found on page 16,290
C***  prevent division by zero (if LAMBDA = 1 / MU,
C***  shift 1/MU^2 by  EPS = 1.E-3
C***  which is approx equiv to shifting MU by 0.5*EPS* (MU)**3

         EXPON0 = EXP( -TAUSLA(I-1) )
         EXPON1 = EXP( -TAUSLA(I) )

         DIVISR = LAM(I) * LAM(I) - 1.0 / ( MU2(I) * MU2(I) )
         TEMP = MAX( EPS, ABS( DIVISR ) )
         DIVISR = SIGN( TEMP , DIVISR )

         UP = OM * PIFS *
     &        ( ( GAM1 - 1.0 / MU2(I) ) * GAM3 + GAM4 * GAM2 ) / DIVISR

         DN = OM * PIFS *
     &        ( ( GAM1 + 1.0 / MU2(I) ) * GAM4 + GAM2 * GAM3 ) / DIVISR

C***cup and cdn are when tau is equal to zero
C***cuptn and cdntn are when tau is equal to taun

         CUP(I)   = UP * EXPON0
         CDN(I)   = DN * EXPON0
         CUPTN(I) = UP * EXPON1
         CDNTN(I) = DN * EXPON1

 11   CONTINUE                  ! loop on layer

C***set up matrix
C*** ssfc = pg 16,292 equation 37  where pi Fs is one (unity).

      SSFC = RSFC * MU * EXP( -TAUSLA( NLAYER ) ) * PIFS

C***MROWS = the number of rows in the matrix

      MROWS = 2 * ( NLAYER )

C*** the following are from pg 16,292  equations 39 - 43.
C*** set up first row of matrix:

      I = 1
      A(1) = 0.0
      B(1) = E1(I)
      D(1) = -E2(I)
      E(1) = FDN0 - CDN(I)

      ROW = 1

C***set up odd rows 3 thru (MROWS - 1):

      I = 0
      DO 20, ROW = 3, MROWS - 1, 2
         I = I + 1
         A(ROW) = E2(I) * E3(I) - E4(I) * E1(I)
         B(ROW) = E1(I) * E1(I + 1) - E3(I) * E3(I + 1)
         D(ROW) = E3(I) * E4(I + 1) - E1(I) * E2(I + 1)
         E(ROW) = E3(I) * ( CUP(I + 1) - CUPTN(I) ) +
     &            E1(I) * ( CDNTN(I) - CDN(I + 1) )
 20   CONTINUE

C***set up even rows 2 thru (MROWS - 2):

      I = 0
      DO 30, ROW = 2, MROWS - 2, 2
         I = I + 1
         A(ROW) = E2(I + 1) * E1(I) - E3(I) * E4(I + 1)
         B(ROW) = E2(I) * E2(I + 1) - E4(I) * E4(I + 1)
         D(ROW) = E1(I + 1) * E4(I + 1) - E2(I + 1) * E3(I + 1)
         E(ROW) = ( CUP(I + 1) - CUPTN(I) ) * E2(I + 1) -
     &            ( CDN(I + 1) - CDNTN(I) ) * E4(I + 1)
 30   CONTINUE

C***set up last row of matrix at MROWS:

      ROW = MROWS
      I = NLAYER

      A(ROW) = E1(I) - RSFC * E3(I)
      B(ROW) = E2(I) - RSFC * E4(I)
      D(ROW) = 0.0
      E(ROW) = SSFC - CUPTN(I) + RSFC * CDNTN(I)

C***solve tri-diagonal matrix:

      CALL TRIDIAGONAL ( A, B, D, E, Y, MROWS )

C*** unfold solution of matrix, compute output fluxes:

      ROW = 1
      LEV = 1
      J = 1

C***the following equations are from pg 16,291  equations 31 & 32

      FDR(LEV) = 1.0    ! this the downward flux at the top of the model
      EDR(LEV) = MU * FDR(LEV)
      EDN(LEV) = FDN0
      EUP(LEV) =  Y(ROW) * E3(J) - Y(ROW + 1) * E4(J) + CUP(J)
      FDN(LEV) = EDN(LEV) / MU1
      FUP(LEV) = EUP(LEV) / MU1

      DO 60, LEV = 2, NLAYER + 1
         FDR(LEV) = EXP( -TAUSLA(LEV-1) )
         EDR(LEV) =  MU * FDR(LEV)
         EDN(LEV) =  Y(ROW) * E3(J) + Y(ROW + 1) * E4(J) + CDNTN(J)
         EUP(LEV) =  Y(ROW) * E1(J) + Y(ROW + 1) * E2(J) + CUPTN(J)
         FDN(LEV) = EDN(LEV) / MU1
         FUP(LEV) = EUP(LEV) / MU1

         ROW = ROW + 2
         J = J + 1
 60   CONTINUE

2609  FORMAT(/ 'PHOT_MOD: Instability in Two Stream RadTran Subroutine'
     &       / 'Layer: I = ',I3
     &       / 'Asymmetry Factor: GI = ',ES12.4
     &       / 'Single Scattering Albedo: OMI = ', ES12.4
     &       / 'SIGN( MIN( ABS( GI ), 1.0 - 1.0E-7), GI ): G = ', ES12.4
     &       / 'MIN( OMI, 1.0 - 1.0E-7 ): OM = ', ES12.4
     &       / '0.25*( 7.0 - OM * ( 4.0 + 3.0 * G ) ): GAM1 = ', ES12.4
     &       / '0.25*( OM * ( 4.0 + 3.0 * G ) - 1.0 ): GAM2 = ', ES12.4  /)

2610  FORMAT(/ 'SETTING (GAM1 - SQRT( GAM1**2 - GAM2**2 ))/GAM2 to zero' /)

2611  FORMAT(/ ' ( GAM1**2 - GAM2**2 ) < 0: NaNs introduced into solution ' /)

      RETURN
      END SUBROUTINE TWOSTREAM_S

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE TRIDIAGONAL ( A, B, D, E, Y, N )
C-----------------------------------------------------------------------
C  This version has the same variable names as in twostream_s, that
C     is D is the superdiagonal and E is the right hand side, and Y is
C     the  solution. The size of A,B,D,E and Y is now N, the
C     number of rows in the matrix.
C-----------------------------------------------------------------------
C
C  FUNCTION:
C     Solves tridiagonal system by Thomas algorithm.  Algorithm fails
C     if first pivot is zero.  In that case, rewrite the
C     equation as a set of order N-1, with U(2) trivially eliminated.
C     The associated tri-diagonal system is stored in 3 arrays
C     B: diagonal
C     A: sub-diagonal
C     D: super-diagonal
C     E: right hand side function
C     U : return solution from tridiagonal solver
C
C     [ B(1) D(1) 0    0    0 ...       0     ]
C     [ A(2) B(2) D(2) 0    0 ...       .     ]
C     [ 0    A(3) B(3) D(3) 0 ...       .     ]
C     [ .       .     .     .           .     ] Y(i) = E(i)
C     [ .             .     .     .     0     ]
C     [ .                   .     .     .     ]
C     [ 0                           A(N) B(N) ]
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION HISTORY:
C     NO.   DATE     WHO      WHAT
C     __    ____     ___      ____
C     5     11/09/04  FSB  Changed symbols to match twostream_s
C                          made arrays variable.
C     4     4/3/96    SJR  copied code and modified for use in JPROC
C     3     8/16/94   XKX  configuration management include statements
C     2     3/15/92   CJC  For use in Models-3 LCM.
C     1     10/19/89  JKV  converted for use on IBM
C     0      3/89     BDX  Initial version
C     yoj
C-----------------------------------------------------------------------

      IMPLICIT NONE

C***ARGUMENTS and their descriptions:

      INTEGER, INTENT(IN) :: N        ! number of rows in matrix
      REAL,    INTENT(IN) :: A( N )   ! subdiagonal
      REAL,    INTENT(IN) :: B( N )   ! diagonal
      REAL,    INTENT(IN) :: D( N )   ! superdiagonal
      REAL,    INTENT(IN) :: E( N )   ! R.H. side
      REAL,   INTENT(OUT) :: Y( N )   ! solution

C***SCRATCH LOCAL VARIABLES and their descriptions:

      INTEGER     J             ! loop index

      REAL        BET           !
      REAL        GAM( N )      !

C***begin body of subroutine  TRIDIAGONAL
C***  Decomposition and forward substitution:

      BET = 1.0 / B( 1 )
      Y( 1 ) = BET * E( 1 )

      DO J = 2, N
         GAM( J ) = BET * D( J - 1 )
         BET = 1.0 / ( B( J ) - A( J ) * GAM( J ) )
         Y( J ) = BET * ( E( J ) - A( J ) * Y( J - 1) )
      END DO

C***Back-substitution:

      DO J = N - 1, 1, -1
         Y( J ) = Y( J ) - GAM( J + 1 ) * Y( J + 1 )
      END DO

      RETURN

      END SUBROUTINE TRIDIAGONAL

C///////////////////////////////////////////////////////////////////////

      INTEGER FUNCTION INDEXR ( NAME1, N, NAME2 )
C-----------------------------------------------------------------------
C
C  FUNCTION:
C     This routine searches for NAME1 in list NAME2
C
C  REVISION HISTORY:
C     5/88   Modified for ROMNET
C     July 29, 2005 by FSB
C     Changed name to avoid conflict FSB
C     copied from CMAQ routine INDEX2 to allow internal use
C
C  ARGUMENT LIST DESCRIPTION:
C
C  Input arguments:
C     NAME1       Character string being searched for
C     N           Length of array to be searched
C     NAME2       Character array to be searched
C
C  Output arguments:
C     INDEX1      The position within the NAME2 array that NAME1
C                 found.  If string was not found, INDEX1 = 0
C
C  LOCAL VARIABLE DESCRIPTION:
C     None
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: N

      CHARACTER*(*), INTENT(IN) :: NAME1
      CHARACTER*(*), INTENT(IN) :: NAME2(*)

      INTEGER I

C***Assume NAME1 is not in list NAME2

      INDEXR = 0

      DO I = 1, N
         IF ( INDEX( NAME2( I ), NAME1 ) .EQ. 1 ) THEN
            INDEXR = I
            RETURN
         END IF
      END DO

      RETURN
      END FUNCTION INDEXR


      END MODULE PHOT_MOD
