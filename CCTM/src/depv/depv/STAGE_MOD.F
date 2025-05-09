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

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE STAGE_MOD 

C-------------------------------------------------------------------------------
C Name:     Surface Tiled Aerosol and Gaseous Exchange (STAGE)
C Purpose:  Computes aerosol and gaseous air surface exchange for sub-
C           grid land use categories. All gaseous species are treated 
C           as having bidirectional exchange following the two layer 
C           model from Nemitz et al. 2001 and aerosol species deposition 
C           follows Binkowski and Shankar 1995. Note that the Nemitz et al.            
C           parameterization reduces to a standard deposition velocity
C           if the pollutant concentration on the leaf, in the leaf stomata, 
C           and in the soil are zero.
C 
C References:
C          Nemitz et al. 2001 Q. J. R. Meteorol. Soc DOI: 10.1002/qj.49712757306
C          Binkowski and Shankar 1995 JGR  DOI: 10.1029/95JD02093
C          Galmarini et al. 2012 Atmos. Chem. Phys. https://doi.org/10.5194/acp-21-15663-2021
C          Appel et al. 2021 Geosci. Model Dev. https://doi.org/10.5194/gmd-14-2867-2021
C          Fuller et al. 1966 Ind. Eng. Chem. https://doi.org/10.1021/ie50677a007
C          Massman 1999 Atmos. Environ. https://doi.org/10.1016/S1352-2310(98)00204-0 
C Default variables output area weighted Vd
C Optional variables output land use specific LAI, RA, U*, Z0, and Vd
C
C Revised:  1 Dec 2017  Original version.  (J. Bash)
C-------------------------------------------------------------------------------

      Use GRID_CONF           ! horizontal & vertical domain specifications
      Use STAGE_LU_MOD             ! Land surface data
      Use ASX_DATA_MOD, only: grid_data, met_data, dwat, hplus_ap, hplus_def, hplus_h2o, kvis
      Use CONST
      USE UTILIO_DEFN      
      USE CGRID_SPCS          ! CGRID mechanism species
      USE STAGE_DATA
      USE AERO_DATA, Only: N_MODE
      USE CENTRALIZED_IO_MODULE, Only: WR_AVAIL
      USE RUNTIME_VARS, Only: MOSAIC

      IMPLICIT NONE

C shared variables 
      INTEGER, ALLOCATABLE, SAVE :: DEPV_SUR( : )   ! pointer to surrogate
      REAL,    ALLOCATABLE, SAVE :: VDEP( : )    ! deposition  velocity [ m/s ]
      REAL,    ALLOCATABLE, SAVE :: VDEPJ( :,: ) ! deposition  velocity [ m/s ]
      INTEGER, PARAMETER         :: N_AE_DEP_SPC = 9 
C land use indexes
      Logical, Allocatable, Save :: Water( : )
      Logical, Allocatable, Save :: Ag( : )
      Integer, Save              :: l_ag
C gas phase species indices
      Integer, Save              :: n_HONO         ! index in depv
      Integer, Save              :: s_HONO         ! index in dep_gas_all
      Integer, Save              :: n_NO2          ! index in depv
      Integer, Save              :: s_NO2          ! index in dep_gas_all
      Integer, Save              :: n_O3           ! index in depv
      Integer, Save              :: s_O3           ! index in dep_gas_all
      Integer, Save              :: n_NH3          ! index in depv
      Integer, Save              :: s_NH3          ! index in dep_gas_all
      Integer, Save              :: n_HG           ! index in depv
      Integer, Save              :: s_HG           ! index in dep_gas_all
      Integer, Save              :: n_HGII         ! index in depv
      Integer, Save              :: s_HGII         ! index in dep_gas_all
      Integer, Save              :: n_HCHO         ! index in depv
      Integer, Save              :: s_HCHO         ! index in dep_gas_all
      Integer, PRIVATE           :: ALLOCSTAT
      CHARACTER( 96 )            :: xmsg = ' '
C Aerosol deposition arrays
      REAL, ALLOCATABLE, SAVE  :: XXLSG( : ) ! log of standard deviation
      REAL, ALLOCATABLE, SAVE  :: DG( : )    ! geometric mean diameter
      REAL, ALLOCATABLE, SAVE  :: PDENS( : ) ! particle density         


      Contains
         SUBROUTINE INIT_STAGE ( JDATE, JTIME )

C-----------------------------------------------------------------------
C  This subroutine sets up the mapping and options for the STAGE gaseous 
C  and aerosol exchange subroutines. 
C-----------------------------------------------------------------------
         USE NH3_BIDI_MOD
         USE MOSAIC_MOD, Only: Tile_Data, init_mosaic
         USE CONST

         Implicit None        
 
         Include SUBST_FILES_ID  ! file name parameters

C Arguments:
         Integer, Intent( IN ) :: JDATE, JTIME      ! internal simulation date&time
         integer               :: c, r, l, n, s, n_diag_dep
         CHARACTER( 16 ), PARAMETER :: pname      = 'INIT_STAGE'        
         CHARACTER( 16 )       :: gc_depv_name( dep_gas_all )
         CHARACTER( 16 )       :: gc_scav_name( dep_gas_all )
         Logical               :: unique_gc_depv( n_gc_depv )
C Local variables:

         CHARACTER( 16 ) :: VDAE_NAME( N_AE_DEP_SPC )! dep vel surrogate name table
C                                       1234567890123456
         DATA         VDAE_NAME( 1 ) / 'VNUMATKN        ' /
         DATA         VDAE_NAME( 2 ) / 'VNUMACC         ' /
         DATA         VDAE_NAME( 3 ) / 'VNUMCOR         ' /
         DATA         VDAE_NAME( 4 ) / 'VMASSI          ' /
         DATA         VDAE_NAME( 5 ) / 'VMASSJ          ' /
         DATA         VDAE_NAME( 6 ) / 'VMASSC          ' /
         DATA         VDAE_NAME( 7 ) / 'VSRFATKN        ' /
         DATA         VDAE_NAME( 8 ) / 'VSRFACC         ' /
         DATA         VDAE_NAME( 9 ) / 'VSRFCOR         ' /

C Check aerosol deposition options 
         If( ( STAGE_E20 .And. STAGE_P22 ) ) Then
            XMSG = 'Both CTM_STAGE_E20 and CTM_STAGE_P22 aerosol dry ' //
     &             ' deposition options were set to Y. Please select one. '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         Else If ( STAGE_E20 .And. STAGE_S22 ) Then
            XMSG = 'Both CTM_STAGE_E20 and CTM_STAGE_S22 aerosol dry ' //
     &             ' deposition options were set to Y. Please select one. '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         Else If ( STAGE_P22 .And. STAGE_S22 ) Then
            XMSG = 'Both CTM_STAGE_S22 and CTM_STAGE_P22 aerosol dry ' //
     &             ' deposition options were set to Y. Please select one. '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

C if abflux this specifies soil, vegetation and water compensation points for NH3
         IF ( abflux ) THEN
            CALL Init_NH3_Bidi( jdate, jtime )
         END IF

!         CALL INIT_MOSAIC( JDATE, JTIME, Grid_Data%LUFRAC )
C  Allocate arrays
         Allocate ( Water  ( Tile_Data%n_lufrac ),
     &              Ag     ( Tile_Data%n_lufrac ),STAT = ALLOCSTAT )

         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating Water, or Ag'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
                         
         ALLOCATE ( VDEP( N_AE_DEP_SPC ), 
     &              VDEPJ( Tile_Data%n_lufrac,N_AE_DEP_SPC ), 
     &              DEPV_SUR( N_AE_DEPV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating VDEP, VDEPJ, DEPV_SUR'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( DG( N_MODE ), XXLSG( N_MODE ), PDENS( N_MODE), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DG, XXLSG, or PDENS'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Set the dep vel surrogate pointers
         DO S = 1, N_AE_DEPV
            N = INDEX1( AE_DEPV( S ), N_AE_DEP_SPC, VDAE_NAME )
            IF ( N .NE. 0 ) THEN
               DEPV_SUR( S ) = N
            ELSE
               XMSG = 'Could not find ' // AE_DEPV( S ) // ' in aerosol' //
     &                ' surrogate table. >>> Dep vel set to zero <<< '
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               DEPV_SUR( S ) = 0
            END IF
         END DO                               

C-----------------------------------------------------------------
C        Species maps
C-----------------------------------------------------------------
         s_HONO = 0
         n_HONO = 0
         s_NO2  = 0
         n_NO2  = 0
         s_O3   = 0
         n_O3   = 0
         s_NH3  = 0
         n_NH3  = 0
         s_HG   = 0
         n_HG   = 0
         s_HGII = 0
         n_HGII = 0
         s_HCHO = 0
         n_HCHO = 0

         n = 0
         maploop: DO s = 1, dep_gas_all
            IF ( .NOT. asx_run_map( s ) ) CYCLE maploop
            n = n + 1

            If ( vd_name( s ) .EQ. 'NO2' ) THEN
               s_NO2 = s
               n_NO2 = n
            End If
            If ( vd_name( s ) .EQ. 'HONO' ) THEN
               s_HONO = s
               n_HONO = n
            End If
            If ( vd_name( s ) .EQ. 'O3' ) THEN
               s_O3 = s
               n_O3 = n
            End If
            If ( vd_name( s ) .EQ. 'NH3' ) THEN
               s_NH3 = s
               n_NH3 = n
            End If
            If ( vd_name( s ) .EQ. 'HG' ) THEN
               s_HG = s
               n_HG = n
            End If
            If ( vd_name( s ) .EQ. 'HGIIGAS' ) THEN
               s_HGII = s
               n_HGII = n
            End If
            If ( vd_name( s ) .EQ. 'HCHO' ) THEN
               s_HCHO = s
               n_HCHO = n
            End If
         END DO maploop

         Water  = .FALSE.
         Ag     = .FALSE.
         l_ag   = 0
         DO l = 1, Tile_Data%n_lufrac
            Select Case( trim( Tile_Data%cat_lu( l ) ) )
               Case( 'WATER' )
                  Water( l ) = .TRUE.
               Case( 'AG'    )
                  l_ag       = l
                  ag( l )    = .TRUE.
               Case( 'AGMOS' )
                  ag( l )    = .TRUE.
               Case( 'HAY'   )
                  ag( l )    = .TRUE.
            End Select
         END DO

         END SUBROUTINE INIT_STAGE
!------------------------------------------------------------------------------
! ******** Calculate deposition velocity for trace gas species ********
! 
! Applies Ohms law and the first Kirchhoff current law to estimate trace gas 
! dry deposition velocities and canopy/vegetation/soil concentrations following
! the framework of Nemitz et al., 2000 and Massad et al 2010
! 
! References: 
!  
! Nemitz et al. Resistance modelling of ammonia exchange over oilseed rape, Ag.
!    Forest Met. 105, 405-425 https://doi.org/10.1016/S0168-1923(00)00206-9, 2000
! Massad et al. Review and parameterisation of bi-directional ammonia exchange 
!    between vegetation and the atmosphere, Atmos. Chem. Phys., 10, 10359-10386
!    https://doi.org/10.5194/acp-10-10359-2010, 2010
!------------------------------------------------------------------------------


         SUBROUTINE GAS_X( JDate, JTime, TStep, c, r, cgridl1 )        
        
         Use NH3_BIDI_MOD
         Use MOSAIC_MOD, Only: Tile_Data, is_ag, is_agmos, is_water, zsoil1, zsoil2 
         Use HG_BIDI
         Use Resist_Funcs
         Use HLCONST_MOD
#ifdef sens
         USE DDM3D_DEFN, ONLY : NP, NPMAX, S_PVD, S_CGRIDL1
#endif


         Implicit None

         Integer, Intent( IN )  :: JDate, JTime, c, r      ! internal simulation date&time
         REAL,    Intent( IN )  :: cgridl1( : )    ! layer 1 concentrations
         REAL,    Intent( IN )  :: TStep           ! Time step in seconds
C Fuller-Schettler-Giddings (https://doi.org/10.1021/ie50677a007; Table I) Diffusion Volume for air 
         Real, Parameter :: Diff_Vol   = 20.1       ! cm3/mol

! Heterogeneous HONO
         Real            :: surf_bldg, surf_leaf    ! HONO building and leaf surface area/voluem (1/
         Real            :: kno2, conc_no2          ! pseudo-first order reaction NO2 + H2O -> HONO  (1/s), NO2 (ppm)
#ifdef sens
      REAL               :: s_conc_no2( NPMAX )
#endif

! Physical Chemistry variables
         Real            :: heff_ap, heff_wat, heff ! Henry's constant for leaf apoplast, surface water, and wet surfaces
         Real            :: hplus                   ! H+ in aqueous media
         LOGICAL         :: effective               ! true=compute effective Henry's Law const
         Real            :: scc_pr_23               ! (SCC/PR)**2/3, fn of DIF0
         Real            :: dif_T, k_vis, dwat_T    ! Trace gas diffusivity, kinematic viscosity of air, and diffusivity of water vapor in air (m2/s)
! Soil properties
         Real            :: sm_v1m, sm_v5cm, sm_v1cm ! Soil moisture at 1 m, 5 cm, and 1 cm (m3/m3)
         Real            :: sm_vsat                  ! volumetric soil saturation (m3/m3)
         Real            :: sm_vfc                   ! volumetric field capacity (m3/m3)
         Real            :: sm_vwlt                  ! volumetric wilting point (m3/m3)
         Real            :: sm_vres                  ! volumetric residual water (m3/m3)
         Real            :: sm_bslp                  ! slope of the saturation water retention curve (ratio)
! meteorological variables
         Real            :: temp_g, temp_2m  ! Soil and 2m temperature (K)
         Real            :: q_2m             ! water vapor mixing ratio at 2m ()
         Real            :: qss_grnd         ! saturation water vapor mixing ratio at the surface ()
         Real            :: rh               ! relative humidity (%) 
         Real            :: ustar            ! surface friction velocity (m/s)
         Real            :: z0               ! zero plane displacement (m)
! Canopy variables
         Real            :: l_leaf          ! leaf aerodynamic width (m)
         Real            :: lai             ! leaf area index (m2/m2)
! Land use variables
         Real            :: swet, sdry, lwet, ldry                 ! leaf and soil wetness (ratio)
         Real            :: snow, no_snow, veg, no_veg, frac_lu    ! fractional land coverage (ratio)
         Real            :: ir_frac                                ! Irrigated fraction (bidi NH3 only : ratio)
         Logical         :: sea_ice
! Deposition velocity, conductances and resistances
         Real            :: vd      ! deposition velocity (m/s)
         Real            :: Ra      ! aerodynamic resistance (s/m)
         Real            :: Rb      ! quasi-laminar soil/water resistance (s/m)
         Real            :: Rb_leaf ! quasi-laminary leaf resistance (s/m)
         Real            :: Rstw    ! Stomatal resistance for water vapor (s/m)
         Real            :: Rst     ! Stomatal resistance (s/m)
         Real            :: Rcut    ! cuticlular resistance (s/m)
         Real            :: Rgc     ! under canopy soil resistance (s/m)
         Real            :: Rg      ! soil resistance (s/m)
         Real            :: Rwat    ! surface water resistance (s/m)
! Flux estiamtes
         Real            :: soil_flux        ! soil emission and deposition for soil biogeochem (ppm m/s)
         Real            :: LU_Flux, LU_Emis ! land use flux and land use emissions (ppm m/s)     
         Real            :: f_wat            ! Air-surface water flux (c_atm-c_wat)/Rwat (ppm m/s)
         Real            :: f_stom           ! Air-stomatal flux      (c_leaf - c_stom)/Rst (ppm m/s)
         Real            :: f_cut            ! Air-cuticular flux     (c_leaf - c_cut)/Rcut (ppm m/s)
         Real            :: f_soil           ! Air-soil flux          (c_z0 - cgrnd)/Rgc    (ppm m/s)
         Real            :: flux_ag          ! Flux for agricultural land use, BIDI NH3 only (ppm m/s)             
! Concentrations and compensation points
         Real            :: c_atm            ! ambient atmospheric concentration (ppm) 
         Real            :: c_z0             ! Canopy concentration at z0 (ppm) 
         Real            :: c_leaf           ! Leaf concentration at z0 (ppm)  
         Real            :: c_stom           ! Stomatal concentration at z0 (ppm)  
         Real            :: c_cut            ! Concentration at the cuticular surface (ppm)  
         Real            :: c_grnd           ! Concentration in the soil air space, no veg canopy (ppm)  
         Real            :: c_ll             ! Concentration in the soil air space, veg canopy (ppm)   
         Real            :: c_wat            ! Concentration in surface waters (ppm)    
! Air-sea exchange
         Real            :: ctemp2, lv, cp_air, tw              ! 2 meter temp (C), latent heat of vaporization [J/kg/K], specific heat capacity of air [J/kg/K], water temperature (K),
! indexes
         integer         :: i, l, n, s, n_diag
         Logical         :: NH3_hit, O3_hit, Hg_hit, HCHO_hit ! logical vars for special cases

! Hg bidi variables 
         Real            :: Hg_st, Hg_cut, Hg_grnd, Hg_wat, flux_hgII
   
         effective = .TRUE.

#ifdef sens
         s_pvd     ( :,:,:,: ) = 0.0
#endif
! grid cell met variables 
         temp_2m  = Met_Data%TEMP2(c,r)
         q_2m     = MET_DATA%Q2( c,r )
         qss_grnd = Met_Data%QSS_GRND( c,r )
         temp_g   = MET_DATA%TEMPG( c,r )
         rh       = q_2m/qss_grnd * 100.0
         rh       = max(min(rh,100.0),0.0)
         sm_vsat  = TILE_DATA%WSAT( c,r )
         sm_vfc   = TILE_DATA%WFC( c,r )
         sm_vwlt  = TILE_DATA%WWLT( c,r )
         sm_vres  = TILE_DATA%WRES( c,r )
         sm_bslp  = max(TILE_DATA%BSLP( c,r ),1.0)
         sm_v1cm  = min(MET_DATA%SOIM1( c,r ),sm_vsat)
         sm_v1cm  = max(sm_v1cm,sm_vres)
         If( ABFLUX ) Then
            ir_frac = frac_ir( c,r )
         Else
            ir_frac = 0.0 ! no data available
         End If
         If( PX_LSM ) Then
! simplified from Darcy's law assuming stationarity and only gravitational draining with the Campbell hydrological functions applied
            sm_v5cm = sm_v1cm * exp( (0.05-zsoil1) * GRAV )**(1.0/sm_bslp)
            sm_v5cm = Min( sm_v5cm, sm_vsat )
            sm_v5cm = Max( sm_v5cm, sm_vres )
            sm_v1m  = min(Tile_Data%SOIM2( c,r ),sm_vsat)
            sm_v1m  = max(sm_v1m,sm_vres)
         Else If( CLM_LSM .OR. NOAH_LSM ) Then
            sm_v5cm = Tile_Data%SOIM2( c,r )
            sm_v5cm = Min( sm_v5cm, sm_vsat )
            sm_v5cm = Max( sm_v5cm, sm_vres )
! simplified from Darcy's law assuming stationarity and only gravitational draining with the Campbell hydrological functions applied
            sm_v1m = sm_v5cm * exp( (1.0-zsoil2) * GRAV )**(1.0/sm_bslp)
            sm_v1m  = min(sm_v1m,sm_vsat)
            sm_v1m  = max(sm_v1m,sm_vres)
         End If
         If(sm_v1cm/sm_vfc .gt. 1.0 ) Then
            swet = 1.0 
            sdry = 0.0
         Else 
            swet = 0.0
            sdry = 1.0
         End If 
         snow    = max( 0.0, MET_DATA%SNOCOV( c,r ) )
! total snow fraction
         no_snow = 1.0 - snow

         sea_ice = .TRUE.
         IF (((GRID_DATA%OCEAN(c,r) + GRID_DATA%SZONE(c,r)) .GT. 0.0) .AND. (MET_DATA%SEAICE(c,r) .LE. 0.0)) THEN
            sea_ice = .FALSE.
         End If

! moved water temperature here to reduce redundancy and facilitate moving the hlconst 
! subroutine out of the land use loop for better vectorization.
         ctemp2 = temp_2m - stdtemp
         lv     = lv0 - dlvdt * ctemp2
         cp_air = CPD * ( 1.0 + 0.84 * q_2m )               ! [J/kg/K]
         tw     = ( ( 4.71e4 * cp_air / lv ) - 0.870 ) + stdtemp  ! [K]
         k_vis  = kvis*1.0e-4 * ( temp_2m/STDTEMP )**1.81 ! Following Massman 1999
         dwat_T = dwat*1.0e-4 * ( temp_2m/STDTEMP )**1.81 ! Following Massman 1999
         n = 0
         n_diag = 0
         flux_hgII = 0.0
         spc_loop: Do s = 1, dep_gas_all
            IF ( asx_run_map( s ) ) Then
            NH3_hit   = .FALSE.
            O3_hit    = .FALSE.
            Hg_hit    = .FALSE.
            HCHO_hit  = .FALSE.    
            n = n + 1
            Tile_Data%Grd_Vd( C, R, n )    = 0.0
            Tile_Data%Bidi_Emis( C, R, n ) = 0.0
! Special cases
            if( s .eq. s_NH3 )  NH3_hit  = .TRUE.
            if( s .eq. s_O3 )   O3_hit   = .TRUE.
            if( s .eq. s_Hg )   Hg_hit   = .TRUE.
            if( s .eq. s_HCHO ) HCHO_hit = .TRUE.
! Following Fuller et al 1966 (https://doi.org/10.1021/ie50677a007) and using the FSG-LeBas method from the
! EPA OnSite toolbox (https://www3.epa.gov/ceampubl/learn2model/part-two/onsite/ed-background.html). 
            dif_T     = 1.0e-7*temp_2m**1.75 * sqrt( 1.0/MWAIR + 1.0/molwt_all( s ) ) / 
     &                ( Diff_Vol**(1.0/3.0) + LeBasM( s )**(1.0/3.0) )**2 
            scc_pr_23 = ( ( k_vis / dif_T ) / pr ) ** twothirds

            c_atm = max( cgridl1( n ), 1.0e-30 )
            f_stom    = 0.0
            f_cut     = 0.0
            f_soil    = 0.0
            flux_ag   = 0.0
            f_wat     = 0.0
            soil_flux = 0.0
            heff_wat  = hlconst( hlind( s ), tw, effective, hplus_h2o )* 0.08205 * tw
            heff_ap   = hlconst( hlind( s ), temp_2m, effective, hplus_ap )
            heff      = hlconst( hlind( s ), temp_2m, effective, hplus_def )* 0.08205 * temp_2m
            If ( HGBIDI .And. Hg_Hit ) Then
               Call  Get_Hg_Comp( Hg_st, Hg_cut, Hg_grnd, Hg_wat, c_atm, heff_wat, heff, r, c )
            End If
            lu_loop: Do l = 1, Tile_Data%n_lufrac
               c_z0    = 0.0
               c_leaf  = 0.0
               LU_Emis = 0.0
               If( HGBIDI .And. Hg_Hit) Then
                  c_stom = Hg_st
                  c_cut  = Hg_cut
                  c_grnd = Hg_grnd
                  c_ll   = Hg_grnd
                  c_wat  = Hg_wat
               Else
                  c_stom = 0.0
                  c_cut  = 0.0
                  c_grnd = 0.0
                  c_ll   = 0.0
                  c_wat  = 0.0
               End If
               Rb     = 0.0
               Rb_leaf= 0.0
               Rst    = 0.0
               Rcut   = 0.0
               Rg     = 0.0
               Rgc    = 0.0                  
               Rstw   = 1.0e6
C land use specific area fraction 
               frac_lu = Tile_Data%LUFRAC( c,r,l )  
               If( frac_lu .Gt. 0.0 ) Then
C land use specific met data
               z0      = Calc_z0( is_ag(l), is_agmos(l), is_water(l), frac_lu, Tile_Data%Z00( l ), Met_Data%Z0( c,r ), 
     &                            Tile_Data%LAIMN0( l ), Tile_Data%LAI0( l ), Tile_Data%SOIT2( c,r ) )
               ustar   = Calc_USTAR( MET_DATA%USTAR( c,r ), Met_Data%ZH( c,r,1 ), Met_Data%Z0( c,r ), z0, frac_lu )
C Get Ra
               Ra = Calc_Ra( Met_Data%RA( c,r ), Met_Data%ZH( c,r,1 ), Met_Data%Z0( c,r ), z0, frac_lu )
!-------------------------------------------------------------------------------------------------
! Quasi-laminar boundary layer resistance
!-------------------------------------------------------------------------------------------------
               Rb  = Calc_Rbw( ustar, scc_pr_23 )
!-------------------------------------------------------------------------------------------------
! Air-water exchange
!-------------------------------------------------------------------------------------------------
               If( Water( l ) ) Then
!-------------------------------------------------------------------------------------------------
! Resistance to surface water
!-------------------------------------------------------------------------------------------------
                  Rwat = Calc_Rwater( ustar, q_2m, temp_2m, temp_g, tw,  LeBasM( s ), heff_wat, O3_hit, 
     &                                Hg_hit, sea_ice  )

                  If(c_wat .Gt. 0.0 ) Then
                     LU_Emis = c_wat / ( Ra + Rb + Rwat )
                  End If

                  Vd     = 1.0 / ( Ra + Rb + Rwat )  
                  f_wat  = f_wat  + frac_lu * ( c_wat - c_atm ) / ( Ra + Rb + Rwat )   
                  if( s .EQ. s_HGII ) Then
                     flux_hgII = flux_hgII + f_wat * frac_lu
                  End If
               Else 
!-------------------------------------------------------------------------------------------------
! Air-land exchange
!-------------------------------------------------------------------------------------------------
! Resistance to air-canopy exchange
!-------------------------------------------------------------------------------------------------
                  lai     = Calc_LAI( l, Tile_Data%n_lufrac, is_ag, is_agmos, is_water, MET_DATA%LAI( c,r ), 
     &                             Tile_Data%LUFRAC( c,r,: ), Tile_Data%LAIMN0, Tile_Data%LAI0, Tile_Data%SOIT2( c,r ) )
                  veg     = Calc_Veg( l, Tile_Data%n_lufrac, is_ag, is_agmos, is_water, MET_DATA%VEG( c,r ), 
     &                             Tile_Data%LUFRAC( c,r,: ), Tile_Data%VEGMN0, Tile_Data%VEG0, Tile_Data%SOIT2( c,r ) )
C land use specific land cover data
                  lwet    = Calc_CanWat( veg, lai, Met_Data%WR( c,r ), WR_AVAIL, q_2m, Met_Data%QSS_GRND( c,r ),
     &                                Met_Data%RN( c,r ), Met_Data%RC( c,r ) )
! Leaf micro wetness as a functiono of RH (Burkhardt et al. 2009 www.biogeosciences.net/6/67/2009/)
                  lwet    = max( 3.68e-4*exp(0.079*rh),lwet )
                  ldry    = 1.0 - lwet
                  no_veg  = 1.0 - veg
                  l_leaf  = Tile_Data%l_width( l )
C Calculate Rst
                  If( lai .Gt. 0.0 ) Then
!-------------------------------------------------------------------------------------------------
! Quasi Laminar Resistance to leaf 
!-------------------------------------------------------------------------------------------------
                     Rb_leaf = Calc_Rb_leaf( k_vis, dif_T, ustar, l_leaf, lai )
C Get Rstw
                     Rstw = Calc_Rstw( l, Tile_Data%n_lufrac, Met_Data%RS( c,r ), Tile_Data%LUFRAC( c,r,: ),
     &                           Tile_Data%rsmin, lai, ustar, Ra, Met_Data%RGRND( c,r ), sm_v1m, sm_vwlt, 
     &                           sm_vfc, temp_2m, qss_grnd, q_2m, Grid_Data%LWMASK( c,r ),
     &                           k_vis, dwat_T, l_leaf )

!-------------------------------------------------------------------------------------------------
! Resistance to air-stomatal exchange
!-------------------------------------------------------------------------------------------------
                     Rst = Calc_Rst( Rstw, dwat_T, dif_T, heff_ap, f0( s ), lai )

!-------------------------------------------------------------------------------------------------
! Resistance to air-cuticle exchange
!-------------------------------------------------------------------------------------------------
                     Rcut = Calc_Rcut( temp_g, rel_rx(s), molwt_all( s ), M_ac( s ), heff, dif_T, 
     &                            snow, no_snow, ldry, lwet, rh, lai, O3_hit, NH3_hit, ABFLUX )

!-------------------------------------------------------------------------------------------------
! Resistance to air-canopy covered soil exchange
!-------------------------------------------------------------------------------------------------
                  Else ! LAI = 0.0
                     Rstw    = 1.0e6
                     Rst     = 1.0e6
                     Rcut    = 1.0e6
                     Rb_leaf = 1.0e6
                  End If ! LAI    
!-------------------------------------------------------------------------------------------------
! Resistance to air-base soil exchange
!-------------------------------------------------------------------------------------------------
                  If( ABFLUX .And. NH3_Hit ) Then
                     Call Get_NH3_Comp( c_stom, c_ll, c_grnd, dif_T, r, c, l, l_ag )  ! Quasi Laminar Boundary Layer resistance
                  End If ! ABFLUX and NH3
                  If( HCHO_Hit ) Then
! HCHO stomatal compensation point from Shutter et al. 2024 Equation 8 adjusted from ppb to ppm
                     c_stom = 3.32e-4 + 1.89e-4* exp(0.194*(temp_g - stdtemp - 30.0))
                  End If 

                  Rgc = Calc_Rg( dif_T, k_vis, rel_rx( s ), Ra, ustar, lai, temp_g, molwt_all( s ), M_ac( s ), 
     &                           heff, sm_v1cm,sm_v5cm, sm_vsat, sm_vfc, sm_vwlt, sm_vres, sm_bslp, zsoil1,
     &                           ir_frac, sdry, swet, snow, no_snow, O3_Hit, NH3_Hit, ABFLUX )

                  Rg  = Calc_Rg( dif_T, k_vis, rel_rx( s ), Ra, ustar, 0.0, temp_g, molwt_all( s ), M_ac( s ), 
     &                           heff, sm_v1cm,sm_v5cm, sm_vsat, sm_vfc, sm_vwlt, sm_vres, sm_bslp, zsoil1,
     &                           ir_frac, sdry, swet, snow, no_snow, O3_Hit, NH3_Hit, ABFLUX )

!-------------------------------------------------------------------------------------------------
! Calculate the compensation points following Nimitz et al 2001
!-------------------------------------------------------------------------------------------------
! Leaf compensation point
!-------------------------------------------------------------------------------------------------
                  c_leaf = (c_atm/(Ra*Rb_leaf)+                                                        ! Atmospheric Component
     &                      c_stom*(1.0/(Ra*Rst)+1.0/(Rb_leaf*Rst)+1.0/(Rgc*Rst))+                     ! Stomatal Component
     &                      c_cut*(1.0/(Ra*Rcut)+1.0/(Rb_leaf*Rcut)+1.0/(Rgc*Rcut))+                   ! Cuticular Component
     &                      c_ll/(Rb_leaf*Rgc))/                                                       ! Under Canopy Surface Component
     &            (1.0/(Ra*Rb_leaf) +1.0/(Ra*Rst) +1.0/(Ra*Rcut)+1.0/(Rb_leaf*Rgc)+1.0/(Rb_leaf*Rcut) + 
     &             1.0/(Rb_leaf*Rst)+1.0/(Rgc*Rst)+1.0/(Rgc*Rcut))                                     ! Least common denominator
!-------------------------------------------------------------------------------------------------
! Compensation point at z0
!-------------------------------------------------------------------------------------------------
                  c_z0     = (c_atm/Ra+c_leaf/Rb_leaf+c_ll/Rgc)/(1.0/Ra+1.0/Rb_leaf+1.0/Rgc)           
!-------------------------------------------------------------------------------------------------
! Estimate air-surface flux
!-------------------------------------------------------------------------------------------------         
! positive values for deposition                 
                  LU_Flux     = veg * (c_atm-c_z0)/Ra +                 ! air-vegetation flux
     &                       no_veg * (c_atm-c_grnd)/( Ra + Rg )        ! air-soil flux
!-------------------------------------------------------------------------------------------------
! Deposition velocity
!-------------------------------------------------------------------------------------------------                                                               
                  vd = veg / (Ra + 1.0/(1.0/(Rb_leaf+1.0/(1.0/Rcut+1.0/Rst))+1.0/Rgc)) +
     &              no_veg / (Ra + Rg)
!-------------------------------------------------------------------------------------------------
! NH3 bidirectional exchange diagnostic fluxes
!-------------------------------------------------------------------------------------------------                                                               
                  If(ABFLUX .And. NH3_Hit ) Then     
                    If( Ag( l ) ) Then
                       soil_flux  = soil_flux + veg * ( c_ll - c_z0 ) / Rgc
     &                            + no_veg * ( c_grnd  - c_atm ) / ( Ra + Rg ) 
                    End If
                  End If ! ABFLUX and NH3
                  If( c_stom .Gt. 0.0 .Or. c_cut .Gt. 0.0 .Or. c_grnd .Gt. 0.0 .Or. c_ll .Gt. 0.0 ) Then
!-------------------------------------------------------------------------------------------------
! Bidirectional exchange
!-------------------------------------------------------------------------------------------------                                                               
                     LU_Emis = max( vd * c_atm - LU_Flux, 0.0 )                         ! should always be greater than 0                     
                     f_soil = f_soil + frac_lu * ( veg * ( ( c_ll - c_z0 ) / Rgc ) +    ! LU_Flux = f_soil + f_stom + f_cut
     &                                         no_veg * (c_grnd - c_atm)/( Ra + Rg ) )  ! air-soil exchange
                     f_stom = f_stom + frac_lu * veg * ( c_stom - c_leaf ) / Rst        ! air-stomatal exchange
                  End If ! compensation points greater than 0
               End If ! Water    
               vd =  vd * Tile_Data%Vd_Fac( n )
               If( MOSAIC ) Then
                  Tile_Data%Lu_Vd( c,r,n,l ) =  vd
                  Tile_Data%RA( c,r,l )      =  Ra
                  Tile_Data%RSTW( c,r,l )    =  Rstw
                  Tile_Data%LAI( c,r,l )     =  lai
                  Tile_Data%USTAR( c,r,l )   =  ustar
                  Tile_Data%Z0( c,r,l )      =  z0
                  Tile_Data%VEG( c,r,l )     =  Veg
               End If
! Generalization of the production and deposition velocity terms
               Tile_Data%Bidi_Emis( C, R, n ) = Tile_Data%Bidi_Emis( c, r, n )  + 
     &                                          frac_lu * LU_Emis
               Tile_Data%Grd_Vd( C, R, n ) = Tile_Data%Grd_Vd( c, r, n ) + 
     &                                       frac_lu * vd
            End If ! frac_lu > 0

            End Do lu_loop
            If(ABFLUX .And. NH3_Hit ) Then
               If( sum(Tile_Data%LUFRAC(c,r,:),mask=Ag) .Gt. 0.0 ) Then 
                  Call Calc_Nitrif ( TStep, C, R, l_ag, soil_flux )
               End If
            End If
            If(HGBIDI .And. Hg_Hit ) Then
! negative values are deposition fluxes
               Call Hg_Surf_Update ( f_stom, f_cut, f_soil, f_wat, flux_hgII, 
     &                               Heff_wat, Heff, TStep, c, r, Jdate, Jtime )
               flux_hgII = 0.0 
            End If
C--------------------------------------------------------------------------
            IF ( sfc_hono ) THEN

C HONO production via heterogeneous reaction on ground surfaces,
C 2NO2 = HONO + HNO3
C Rate constant for the reaction = (3.0E-3/60)* (A/V),
C where A/V is surface area/volume ratio
C HONO is produced and released into the atmosphere
C NO2 is lost via chemical reaction
C HNO3 is sticky and stays on the surfaces

C Calculate A/V for leaves.
C LAI was multiplied by 2 to account for the fact that surface area
C is provided by both sides of the leaves.
C Matthew Jones, Ammonia deposition to semi-natural vegetation,
C PhD dissertation, University of Dundee, Scotland, 2006

               surf_leaf = 2.0 * MET_DATA%LAI( c,r ) / MET_DATA%ZF( c,r,1 )

C Calculate A/V for buildings and other structures.
C Buildings and other structures can provide additional surfaces in
C urban areas for the heterogeneous reaction to occur. However, such
C information is not readily available; in the absence of such information,
C it is scaled to purb(c,r). Svensson et al., (1987) suggests a typical value
C of 0.2 for A/V for buildings in urban environments. A maximum value of 0.2
C for A/V for buildings is assigned to the grid cell containing the highest
C purb(c,r) i.e., 100.0. A/V for buildings for other grid-cell is calculated
C as purb(c,r)*(0.2/100.0); Cai et al. (2006) used a value of 1.0 for their
C study at New York (total A/V)

               surf_bldg = GRID_DATA%PURB( c,r ) * 0.002

C Calculate rate constant for the reaction (pseudo-first order reaction,
C unit per second). Calculate pseudo-first order rate constant using Eq 1
C of Vogel et al. (2003).  Unit of KNO2 is in 1/min in the paper; divide it
C by 60 to convert it into 1/sec.

               kno2 = MAX( 0.0, 5.0E-5 * (surf_leaf + surf_bldg) )

C Determine NO2 concentration needed for HONO production term.

               IF ( s .EQ. s_NO2 ) THEN
                  conc_no2 = cgridl1( n )
#ifdef sens
                     DO NP = 1, NPMAX
                        s_conc_no2( NP ) = s_cgridl1( NP, n,c,r )
                     END DO
#endif
               END IF
C Calculate production (bidi_emis) for HONO; unit = ppm * m/s
               IF ( s .EQ. s_HONO ) Then
                  Tile_Data%Bidi_Emis( C, R, n ) = kno2 * conc_no2 * MET_DATA%ZF( c,r,1 )
#ifdef sens
                  DO NP = 1, NPMAX
                     s_pvd( NP,n,c,r ) = kno2 * s_conc_no2( NP ) * MET_DATA%ZF( c,r,1 )
                  END DO
#endif
               END IF 
            END IF

            ! Check for negative values or NaN's                  
            if(isnan(Tile_Data%Bidi_Emis( C, R, n ))) write(logdev,*) 'NaN in ',vd_name( s ),' production term'
            if(isnan(Tile_Data%Grd_Vd( C, R, n ))) write(logdev,*) 'NaN in ',vd_name( s ),' Vd term'
            End If 
         End Do spc_loop

         Return         
         END SUBROUTINE GAS_X

         SUBROUTINE AERO_X(CGRID, C, R )

C *** Calculate deposition velocity for Aitken, accumulation, and
C     coarse modes.
C     Reference:
C     Binkowski F. S., and U. Shankar, The regional particulate
C     model 1. Model description and preliminary results.
C     J. Geophys. Res., 100, D12, 26191-26209, 1995.
 
C    May 05 D.Schwede: added impaction term to coarse mode dry deposition
C 25 May 05 J.Pleim:  Updated dry dep velocity calculation for aerosols
C                     to Venkatram and Pleim (1999)
C 20 Jul 05 J.Pleim:  Changed impaction term using modal integration of
C                     Stokes**2 / 400 (Giorgi, 1986, JGR)
C 14 Apr 08 J.Kelly:  Added code to calculate deposition velocity of
C                     coarse surface area and to account for variable
C                     standard deviation of the coarse mode.
C 08 Sep 08 P.Bhave:  Backward compatibility with AE4 mechanisms
C                     standardized names of all coarse-mode variables
C-----------------------------------------------------------------------

         USE AERO_DATA           ! aero variable data   
         USE AEROMET_DATA        ! Includes CONST module
         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE RXNS_DATA           ! chemical mechanism data
         Use MOSAIC_MOD, Only: Tile_Data, is_ag, is_agmos, is_water 
         Use Resist_Funcs 

         IMPLICIT NONE

C Includes:

         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments
         REAL,    POINTER       :: CGRID( :,:,:,: )
         INTEGER, INTENT( IN )  :: C,R                 ! Column and Row

C *** array indices hardcoded to match SUBROUTINE AERO_DEPV
      INTEGER, PARAMETER, DIMENSION( 3 ) :: 
     &                      VDN = (/ 1,2,3 /) , 
     &                      VDM = (/ 4,5,6 /) , 
     &                      VDS = (/ 7,8,9 /)  

C Meteorological variables

         CHARACTER( 16 ), SAVE :: AE_VRSN ! Aerosol version name

         INTEGER, SAVE :: NCELLS              ! number of cells per layer

         REAL, SAVE  :: XLM        ! mean free path [ m ]
         REAL, SAVE  :: AMU        ! dynamic viscosity [ kg m**-1 s**-1 ]

         REAL M3_WET, M3SUBT, M3_DRY
         REAL M2_WET, M2_DRY

         CHARACTER( 16 ), SAVE :: PNAME = 'AERO_X'
         CHARACTER( 16 ) :: VNAME            ! variable name
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER  V, N, L, NDX          ! loop counters
         INTEGER  n_diag
         INTEGER  SPC, S                ! species loop counter
         INTEGER  ALLOCSTAT

C modal Knudsen numbers
         REAL KN

C modal particle diffusivities for number, 2nd, and 3rd moment, or mass:
         REAL DCHAT0
         REAL DCHAT2
         REAL DCHAT3

C modal sedimentation velocities for number, 2nd, and 3rd moment, or mass:
         REAL VGHAT0
         REAL VGHAT2
         REAL VGHAT3

         INTEGER NCELL, J, IM

         REAL DCONST,  DCONST1
         REAL DCONST2, DCONST3
         REAL EINHAT0, EINHAT2, EINHAT3 ! Interception integration terms
         REAL SC0                      ! Schmidt numbers for number
         REAL SC2                      ! Schmidt numbers for 2ND MOMENT
         REAL SC3                      ! Schmidt numbers for 3rd moment
         REAL NU                       ! kinematic viscosity [ m**2 s**-1 ]
         REAL TWOXLM                   ! 2 X atmospheric mean free path
         REAL Ra, Ustar, lai, z0       ! Land use specific environmental variables
         REAL Ubar, SeaIce, SST, TempS ! Grid scale meteorological variables
         REAL Lu_frac, Veg             ! Land use and vegetation coverage fraction
         REAL lwet,swet,sm_vfc,sm_v1cm ! wet leaf and surfaces
         REAL l_width, Alpha           ! Emerson et al. 2020 land use variables
         REAL BAI, Ahair, Fhair, Aleaf ! Pleim et al. 2022 land use variables

C Parameters
         REAL, PARAMETER :: BHAT    = 1.246    ! Slope of the linearized Cunningham slip correction 
         REAL, PARAMETER :: IHAT    = 0.8473   ! Intercept of the linearized Cunningham slip correction 
         REAL, PARAMETER :: T0      = 288.15   ! [ K ] ! starting standard surface temp.
         REAL, PARAMETER :: THREEPI = 3.0 * PI

C Scalar variables for VARIABLE standard deviations.

         REAL    L2SG

         REAL    E1                  ! mode exp( log^2( sigmag )/8 )
         REAL    ES04                !        " **4
         REAL    ES08                !        " **8
         REAL    ES12                !        " **12
         REAL    ES16                !        " **16
         REAL    ES20                !        " **20
         REAL    ES28                !        " **28
         REAL    ES32                !        " **32
         REAL    ES36                !        " **36
         REAL    ES48                !        " **48
         REAL    ES64                !        " **64
         REAL    ESM12               !        " **(-12)
         REAL    ESM16               !        " **(-16)
         REAL    ESM20               !        " **(-20)
         REAL    ESM32               !        " **(-32)

C-----------------------------------------------------------------------

         VDEP  = 0.0   ! array assignment
         VDEPJ = 0.0   ! array assignment
! initialize the leaf wetness at the beginning of the loop
         lwet = 0.0

C ***    Set meteorological data for the grid cell.
         AIRDENS = Met_Data%DENS1( C,R )
         AIRTEMP = Met_Data%TEMP2( C,R )
         AIRPRES = Met_Data%PRSFC( C,R )
         TempS   = Met_Data%TEMPG( C,R )
         Ubar    = Met_Data%WSPD10( C,R )
         SeaIce  = Met_Data%SEAICE( C,R )
         SST     = Met_Data%TSEASFC( C,R ) - 273.15
         sm_vfc  = TILE_DATA%WFC( c,r )
         sm_v1cm = MET_DATA%SOIM1( c,r )
! following Lee and Pielke 1992 https://doi.org/10.1175/1520-0450(1992)031%3C0480:ETSSSH%3E2.0.CO;2
         IF( sm_v1cm/sm_vfc .gt. 1.0 ) Then
            swet = 1.0 
         Else 
            swet = 0.0 
         End If

C ***    extract grid cell concentrations of aero species from CGRID
C        into aerospc_conc in aero_data module
C        Also determines second moment from surface area and adds wet
C        species
#ifdef sens
         CALL EXTRACT_AERO( CGRID( C,R,1,: ), .TRUE., CGRID( C,R,:,: ), .FALSE. ) 
#else
         CALL EXTRACT_AERO( CGRID( C,R,1,: ), .TRUE. )
#endif

C ***    Calculate geometric mean diameters and standard deviations of the
C        "wet" size distribution
         CALL GETPAR( .FALSE. )     

C        Save getpar values to arrays
         DO IM = 1,N_MODE
            XXLSG( IM ) = AEROMODE_LNSG( IM )   
            DG( IM )    = AEROMODE_DIAM( IM )
            PDENS( IM ) = AEROMODE_DENS( IM )
         END DO
 
C        Calculate mean free path [ m ]:
         XLM = 6.6328E-8 * STDATMPA * AIRTEMP / ( T0 * AIRPRES )

C ***    Calculate dynamic and kinematic viscosity [ kg m**-1 s**-1 ]:
         AMU = 1.458E-6 * AIRTEMP * SQRT( AIRTEMP )
     &                 / ( AIRTEMP + 110.4 )
         NU = AMU / Met_Data%DENS1( C,R )   

C *** Calculate Knudsen numbers
         TWOXLM = XLM + XLM
         DO IM = 1, N_MODE
            KN = TWOXLM / DG( IM )

C *** Calculate functions of variable standard deviation.

            L2SG = XXLSG( IM ) ** 2
            
            E1   = EXP( 0.125 * L2SG )
            ES04 = E1 ** 4
            ES08 = ES04 ** 2
            ES12 = ES04 * ES08
            ES16 = ES08 ** 2
            ES20 = ES16 * ES04
            ES28 = ES20 * ES08
            ES32 = ES16 ** 2
            ES36 = ES16 * ES20
            ES48 = ES36 * ES12
            ES64 = ES32 ** 2

C *** calculate inverses:

            ESM12 = 1.0 / ES12
            ESM16 = 1.0 / ES16
            ESM20 = 1.0 / ES20
            ESM32 = 1.0 / ES32

            DCONST  = DKBOLTZ * Met_Data%TEMP2( C,R ) / ( THREEPI * AMU )
            DCONST1 = DCONST / DG( IM )

            DCONST2 = GRAV / ( 18.0 * AMU )
            DCONST3 = DCONST2 * PDENS( IM ) * DG( IM ) ** 2 ! Gravitational settling 
C Calculate characteristic parameters
            DCHAT0  = DCONST1 * ( ES04  + BHAT * KN * ES16 )
            DCHAT2  = DCONST1 * ( ESM12 + BHAT * KN * ESM16 )
            DCHAT3  = DCONST1 * ( ESM20 + BHAT * KN * ESM32 )
            VGHAT0  = DCONST3 * ( ES16  + BHAT * KN * ES04 )
            VGHAT2  = DCONST3 * ( ES48  + BHAT * KN * ES20 )
            VGHAT3  = DCONST3 * ( ES64  + BHAT * KN * ES28 )  

C Integrated Emerson et al 2020 Interception term.
            EINHAT0 = EXP(L2SG*((0.64)/2.0))
            EINHAT2 = EXP(L2SG*((1.6*2.0 + 0.64)/2.0))
            EINHAT3 = EXP(L2SG*((1.6*3.0 + 0.64)/2.0))

            SC0 = NU / DCHAT0
            SC2 = NU / DCHAT2   
            SC3 = NU / DCHAT3


            lu_loop: DO L = 1, Tile_Data%n_lufrac
C ***    Land use parameters
               lu_frac = Tile_Data%LUFRAC( C,R,L )
               l_width = Tile_Data%l_width( L )
               Alpha   = Tile_Data%Alpha( L )
               BAI     = Tile_Data%BAI( L )
               Ahair   = Tile_Data%Ahair( L )
               Fhair   = Tile_Data%Fhair( L )
               Aleaf   = Tile_Data%Aleaf( L )
               IF ( lu_frac .Eq. 0.0 ) Cycle lu_loop
               z0      = Calc_z0( is_ag(L), is_agmos(L), is_water(L), lu_frac, Tile_Data%Z00( L ), Met_Data%Z0( c,r ), 
     &                            Tile_Data%LAIMN0( L ), Tile_Data%LAI0( L ), Tile_Data%SOIT2( c,r ) )
               ustar   = Calc_USTAR( MET_DATA%USTAR( c,r ), Met_Data%ZH( c,r,1 ), Met_Data%Z0( c,r ), z0, lu_frac )
               Ra      = Calc_Ra( Met_Data%RA( c,r ), Met_Data%ZH( c,r,1 ), Met_Data%Z0( c,r ), z0, lu_frac )
               IF( water( L ) ) Then
                  swet = 1.0 - SeaIce
                  lai  = 0.0
                  veg  = 0.0                  
                  lwet = 0.0
               Else

                  lai     = Calc_LAI( L, Tile_Data%n_lufrac, is_ag, is_agmos, is_water, MET_DATA%LAI( c,r ), 
     &                       Tile_Data%LUFRAC( c,r,: ), Tile_Data%LAIMN0, Tile_Data%LAI0, Tile_Data%SOIT2( c,r ) )

                  veg     = Calc_Veg( L, Tile_Data%n_lufrac, is_ag, is_agmos, is_water, MET_DATA%VEG( c,r ), 
     &                       Tile_Data%LUFRAC( c,r,: ), Tile_Data%VEGMN0, Tile_Data%VEG0, Tile_Data%SOIT2( c,r ) )
                  lwet    = Calc_CanWat( veg, lai, Met_Data%WR( c,r ), WR_AVAIL, Met_Data%Q2( c,r ), 
     &                       Met_Data%QSS_GRND( c,r ), Met_Data%RN( c,r ), Met_Data%RC( c,r ) )
                  IF( TempS .lt. stdtemp ) Then ! assume ice
                     lwet = 0.0 
                     swet = 0.0      
                  End IF
               End If            
C now calculate the deposition velocities

C Parallel conductances are additive. Thus, the vegetated and non-vegetated deposition velocities are added. 
C first do 0th moment (number), second do 2nd moment (surface area), third do 3rd moment (mass)
C Emerson et al 2020 PNAS https://www.pnas.org/cgi/doi/10.1073/pnas.2014761117
               If( STAGE_E20 ) Then
                  VDEPJ( L,VDN( IM ) ) = aero_depv_e20(veg, lai, Ustar, Ra, lwet, swet, l_width, Alpha, 
     &                                             DG(IM), SC0, NU, VGHAT0, EINHAT0 ) ! Number
                  VDEPJ( L,VDS( IM ) ) = aero_depv_e20(veg, lai, Ustar, Ra, lwet, swet, l_width, Alpha,
     &                                             DG(IM), SC2, NU, VGHAT2, EINHAT2 ) ! Surface area
                  VDEPJ( L,VDM( IM ) ) = aero_depv_e20(veg, lai, Ustar, Ra, lwet, swet, l_width, Alpha, 
     &                                             DG(IM), SC3, NU, VGHAT3, EINHAT3 ) ! Mass
C Pleim et al 2022
               Else If( STAGE_P22 ) Then 
                  VDEPJ( L,VDN( IM ) ) = aero_depv_p22(veg, lai, BAI, Ustar, Ubar, Ra, Water(L), Aleaf, 
     &                                             Ahair, Fhair, SC0, NU, VGHAT0, SeaIce, SST ) ! Number
                  VDEPJ( L,VDS( IM ) ) = aero_depv_p22(veg, lai, BAI, Ustar, Ubar, Ra, Water(L), Aleaf, 
     &                                             Ahair, Fhair, SC2, NU, VGHAT2, SeaIce, SST ) ! Surface area
                  VDEPJ( L,VDM( IM ) ) = aero_depv_p22(veg, lai, BAI, Ustar, Ubar, Ra, Water(L), Aleaf, 
     &                                             Ahair, Fhair, SC3, NU, VGHAT3, SeaIce, SST ) ! Mass
               Else
C CMAQ v5.3
                  VDEPJ( L,VDN( IM ) ) = aero_depv(veg, Ra, VGHAT0, Ustar, SC0, NU, l_width, lai ) ! Number
                  VDEPJ( L,VDS( IM ) ) = aero_depv(veg, Ra, VGHAT2, Ustar, SC2, NU, l_width, lai ) ! Surface area
                  VDEPJ( L,VDM( IM ) ) = aero_depv(veg, Ra, VGHAT3, Ustar, SC3, NU, l_width, lai ) ! Mass
               End If

C now integrate to the area weighted grid cell
               VDEP( VDN( IM ) )    =  VDEP( VDN( IM ) ) + Lu_Frac * VDEPJ( L,VDN( IM ) ) ! Number
               VDEP( VDS( IM ) )    =  VDEP( VDS( IM ) ) + Lu_Frac * VDEPJ( L,VDS( IM ) ) ! Surface area
               VDEP( VDM( IM ) )    =  VDEP( VDM( IM ) ) + Lu_Frac * VDEPJ( L,VDM( IM ) ) ! Mass

            END DO lu_loop ! Tile_Data%n_lufrac
         END DO ! aerosol mode

C Return dry deposition velocities for aerosols (first layer only).
         n_diag = 0
         DO V = 1, N_AE_DEPV
            NDX = N_GC_DEPV + N_NR_DEPV + N_TR_DEPV + V
            IF ( DEPV_SUR( V ) .GT. 0 ) THEN
               Tile_Data%Grd_Vd( C,R,NDX )   = VDEP( DEPV_SUR( V ) )    * Tile_Data%Vd_Fac( NDX )
               if(MOSAIC) Tile_Data%Lu_Vd( C,R,NDX,: )  = VDEPJ( :,DEPV_SUR( V ) ) * Tile_Data%Vd_Fac( NDX )
            ELSE
               Tile_Data%Grd_Vd( C,R,NDX )   = 0.0
               if(MOSAIC) Tile_Data%Lu_Vd( C,R,NDX,: )  = 0.0  
            END IF 
         END DO
                    
         Return
         END SUBROUTINE AERO_X

      END MODULE STAGE_MOD
