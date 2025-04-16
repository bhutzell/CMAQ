
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

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module MOSAIC_MOD
      
C Contains the shared variables and subrountes needed estimate the resistances
C from natural and agricultural lands
 
C Revision History: J. Bash June 16 2011:    Created
C                   J. Young Oct 31 2011:    changed lai0, laimn0, rsmin, VEG0, vegmn0,
C                                             z00, & luf_fac to pointers to save memory
C                   D. Schwede Mar 12 2012:  fixed errors in crop lai
C                   D. Schwede Sept 07 2012: updated code for NLCD40 land use classification
C                   J. Bash:   Nov 07  2014: Modified for the restructuring of vidff. Most 
C                                            mosaic variables were moved to ASX_DATA_MOD. 
C                                            Algorithms were restructured using fortran 90 
C                                            array constructs for readability.
C                   D. Wong:   Feb 10  2019: removed all MY_N clauses
C                   D. Wong:   Apr 24  2019: removed unused BUFF2D_2 array
C-------------------------------------------------------------------------------

      Implicit None
      
C Private variables used in this module and subroutines       

      Integer,         PRIVATE :: ALLOCSTAT
      Integer, Save, PRIVATE :: l_ag, l_agmos
      Logical, Save, Allocatable  :: is_ag( : )     ! Agricultural flag
      Logical, Save, Allocatable  :: is_agmos( : )  ! Agricultural mosaic flag
      Logical, Save, Allocatable  :: is_water( : )  ! Water flag
      Real,    Save :: zsoil1             ! Layer 1 soil depth
      Real,    Save :: zsoil2             ! Layer 2 soil depth

C Buffer variables  
!      Real, Pointer, Private :: BUFF2D       ( :,: )

      Type :: Tile_Type                
         Integer                      :: n_vd ! number of gas species for tiled output
         Integer                      :: n_lufrac  ! number of land use for tiled output
         Real,            Allocatable :: RSMIN        ( : ) ! minimum stomatal resistance (s/m)
         Real,            Allocatable :: Z00          ( : ) ! momentum roughness length (m)
         Real,            Allocatable :: VEG0         ( : ) ! maximum vegetation coverage (%)
         Real,            Allocatable :: VEGMN0       ( : ) ! minimum vegetation coverage (%)
         Real,            Allocatable :: LAI0         ( : ) ! maximum lai (m2/m2)
         Real,            Allocatable :: LAIMN0       ( : ) ! minimum lai (m2/m2)
         Real,            Allocatable :: NH3_gam_grnd ( : ) ! soil/litter surface NH3 emission potential ([NH4]/[H])
         Real,            Allocatable :: NH3_gam_st   ( : ) ! vegetation NH3 emission potential ([NH4]/[H])
         Real,            Allocatable :: Hg_grnd      ( : ) ! soil Hg concentration (umol/g )
         Real,            Allocatable :: l_width      ( : ) ! aerodynamic leaf width (m)
         Real,            Allocatable :: Alpha        ( : ) ! Emerson et al 2020 PNAS empirical land use factor (unitless)
         Real,            Allocatable :: BAI          ( : ) ! building area index (m2/m2)
         Real,            Allocatable :: Ahair        ( : ) ! leaf hair width (m)
         Real,            Allocatable :: Fhair        ( : ) ! ratio of leaf covered in hairs
         Real,            Allocatable :: Aleaf        ( : ) ! aerodynamic leaf width 
!> Mapping for diagnostic ouputs
         Character( 16 ), Allocatable :: cat_lu       ( : ) ! Tiled LU name
         Character( 16 ), Allocatable :: name_lu      ( : ) ! Land use output name
         Logical,         Allocatable :: gas_out      ( : ) ! vector of length N_SPC_DIFF with TRUE for output
         Character( 16 ), Allocatable :: Vd_Name      ( : ) ! Deposition species output name
         Logical,         Allocatable :: Vd_out       ( : ) ! vector of length N_SPC_DEPV with TRUE for output
         Integer,         Allocatable :: dep2vdiff    ( : ) ! Vdiff location for dep species
         Real,            Allocatable :: Vd_fac       ( : ) ! Vd factor from the GC, NR, and TR namelists
!> STAGE specific met variables
         Real,            Allocatable :: SOIT2   ( :,: )  ! layer 2 soil temperature [K]
         Real,            Allocatable :: SOIM2   ( :,: )  ! layer 2 volumetric soil moisture [m3/m3]
!> Grid soil properties
         Real,            Allocatable :: RHOB   ( :,: )  ! soil bulk density
         Real,            Allocatable :: WSAT   ( :,: )  ! volumetric soil saturation concentration
         Real,            Allocatable :: WWLT   ( :,: )  ! soil wilting point
         Real,            Allocatable :: BSLP   ( :,: )  ! B Slope 
         Real,            Allocatable :: WRES   ( :,: )  ! Soil residual moisture point
         Real,            Allocatable :: WFC    ( :,: )  ! soil field capacity 
!> Mean grid cell output 
         Real,            Allocatable :: Grd_Vd     ( :,:,: ) ! Grid mean gaseous and aerosol deposition velocity
         Real,            Allocatable :: Bidi_Emis  ( :,:,: ) ! Grid bidirectional Emissions
!> Aggrigated fractional land use       
         Real,            Allocatable :: LUfrac     ( :,:,: ) ! land use fraction (ratio)
!> Variables below are only allocated if 
!> Sub grid cell meteorological variables:
         Real,            Allocatable :: USTAR      ( :,:,: )   ! surface friction velocity [m/s]
         Real,            Allocatable :: LAI        ( :,:,: )   ! leaf area index [m**2/m**2]
         Real,            Allocatable :: VEG        ( :,:,: )   ! vegetation fraction [ratio]
         Real,            Allocatable :: Z0         ( :,:,: )   ! momentum roughness length [m]
!> Sub grid cell resistances
         Real,            Allocatable :: RA         ( :,:,: )    ! aerodynamic resistance [s/m]
         Real,            Allocatable :: RSTW       ( :,:,: )    ! Stomatal Resistance of water [s/m]
!> Sub grid cell output:
         Real,            Allocatable :: Lu_Vd      ( :,:,:,: ) ! gaseous and aerosol deposition velocity
      End Type Tile_Type

      Type( Tile_Type ),     Save :: Tile_Data 
      
      Contains

         Subroutine Init_Mosaic( jdate, jtime, lufrac ) 
       
         Use HGRD_DEFN
         USE ASX_DATA_MOD, Only:Grid_Data, Met_Data
         Use STAGE_LU_Mod
         Use UTILIO_DEFN
         USE STAGE_DATA, Only:dep_gas_all ! needs to be n_gas_asx to save memory but will require remapping
         USE CGRID_SPCS          ! CGRID mechanism species
         USE RUNTIME_VARS
         USE CENTRALIZED_IO_MODULE, Only: WR_AVAIL
       
         Implicit None    

C...include files

         Include SUBST_FILES_ID   ! file name parameters                 
       
         Integer, Intent( In )  :: jdate
         Integer, Intent( In )  :: jtime
         Real, Intent( In )  :: lufrac( :,:,: )    
         Character( 240 )       :: xmsg = ' '
         Character(  16 ), save :: pname = 'Init_Mosaic'
         Integer c,r,n,l,s
         Integer N_GAS_DEPV
         Integer gxoff, gyoff            ! global origin offset from file
         Integer :: strtcolgc2, endcolgc2, strtrowgc2, endrowgc2

C File variables:
         Real, Pointer    :: SOILCAT ( :,: )

         Write(logdev,*) 'n_stage_lu : ', n_stage_lu
         Write(logdev,*) 'Allocated Tile_data? : ', Allocated(Tile_Data%cat_lu)
! Allocate Tile Data
         ALLOCATE ( Tile_Data%cat_lu       ( n_stage_lu ),
     &              Tile_Data%name_lu      ( n_stage_lu ), 
     &              Tile_Data%RSMIN        ( n_stage_lu ),
     &              Tile_Data%LAI0         ( n_stage_lu ),
     &              Tile_Data%LAIMN0       ( n_stage_lu ),
     &              Tile_Data%VEG0         ( n_stage_lu ),
     &              Tile_Data%VEGMN0       ( n_stage_lu ),
     &              Tile_Data%Z00          ( n_stage_lu ),
     &              Tile_Data%NH3_gam_st   ( n_stage_lu ),
     &              Tile_Data%NH3_gam_grnd ( n_stage_lu ),
     &              Tile_Data%Hg_grnd      ( n_stage_lu ),
     &              Tile_Data%l_width      ( n_stage_lu ),
     &              Tile_Data%Alpha        ( n_stage_lu ),
     &              Tile_Data%BAI          ( n_stage_lu ),
     &              Tile_Data%Ahair        ( n_stage_lu ),
     &              Tile_Data%Fhair        ( n_stage_lu ),
     &              Tile_Data%Aleaf        ( n_stage_lu ),STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating tile land use specific data'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         If( .not. Allocated(Met_Data%SOIT2) ) Then
            ALLOCATE( Met_Data%SOIT2    ( NCOLS,NROWS ),
     &                STAT = ALLOCSTAT )
            If ( ALLOCSTAT .Ne. 0 ) Then
               XMSG = 'Failure allocating layer 2 soil temperature'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
         End If

         If( .not. Allocated(Met_Data%SOIM2) ) Then
            ALLOCATE( Met_Data%SOIM2    ( NCOLS,NROWS ),
     &                STAT = ALLOCSTAT )
            If ( ALLOCSTAT .Ne. 0 ) Then
               XMSG = 'Failure allocating layer 2 soil moisture'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
         End If
! 
         CALL INIT_STAGE_LU( JDATE, JTIME )

! STAGE land use 
         Tile_Data%n_lufrac = n_stage_lu
         Tile_Data%cat_lu       = STAGE_LU_Data( 1:n_stage_lu )%LU_Cat
         Tile_Data%name_lu      = STAGE_LU_Data( 1:n_stage_lu )%LU_Name
         Tile_Data%RSMIN        = STAGE_LU_Data( 1:n_stage_lu )%RSMIN
         Tile_Data%LAI0         = STAGE_LU_Data( 1:n_stage_lu )%LAI0
         Tile_Data%LAIMN0       = STAGE_LU_Data( 1:n_stage_lu )%LAIMN0
         Tile_Data%VEG0         = STAGE_LU_Data( 1:n_stage_lu )%VEG0
         Tile_Data%VEGMN0       = STAGE_LU_Data( 1:n_stage_lu )%VEGMN0
         Tile_Data%Z00          = STAGE_LU_Data( 1:n_stage_lu )%Z00
         Tile_Data%NH3_gam_st   = STAGE_LU_Data( 1:n_stage_lu )%Gamma_NH3_st
         Tile_Data%NH3_gam_grnd = STAGE_LU_Data( 1:n_stage_lu )%Gamma_NH3_grnd
         Tile_Data%Hg_grnd      = STAGE_LU_Data( 1:n_stage_lu )%Hg_grnd
         Tile_Data%l_width      = STAGE_LU_Data( 1:n_stage_lu )%l_width
         Tile_Data%Alpha        = STAGE_LU_Data( 1:n_stage_lu )%Alpha
         Tile_Data%BAI          = STAGE_LU_Data( 1:n_stage_lu )%BAI
         Tile_Data%Ahair        = STAGE_LU_Data( 1:n_stage_lu )%Ahair
         Tile_Data%Fhair        = STAGE_LU_Data( 1:n_stage_lu )%Fhair
         Tile_Data%Aleaf        = STAGE_LU_Data( 1:n_stage_lu )%Aleaf

         N_GAS_DEPV = N_GC_DEPV + N_NR_DEPV + N_TR_DEPV
         Allocate ( Tile_Data%Grd_Vd    ( ncols,nrows,N_SPC_DEPV ),  
     &              Tile_Data%Bidi_Emis ( ncols,nrows,N_SPC_DEPV ),
     &              Tile_Data%LUfrac    ( ncols,nrows,Tile_Data%n_lufrac ),
     &              Tile_Data%Vd_Name   ( N_SPC_DEPV ),
     &              Tile_Data%Vd_Fac    ( N_SPC_DEPV ),
     &              Tile_Data%Vd_Out    ( N_SPC_DEPV ),        
     &              Tile_Data%dep2vdiff ( N_SPC_DEPV ),  
     &              STAT = ALLOCSTAT ) 
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating MOSAIC deposition velocities'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         Tile_Data%Grd_Vd    = 0.0
         Tile_Data%Bidi_Emis = 0.0
         Tile_Data%Vd_Fac    = 0.0  
         Tile_Data%dep2vdiff = 0        

         Allocate ( Tile_Data%RHOB      ( ncols,nrows ),  
     &              Tile_Data%WSAT      ( ncols,nrows ),
     &              Tile_Data%WWLT      ( ncols,nrows ),
     &              Tile_Data%BSLP      ( ncols,nrows ),
     &              Tile_Data%WRES      ( ncols,nrows ),
     &              Tile_Data%WFC       ( ncols,nrows ),
     &              Tile_Data%SOIT2     ( ncols,nrows ),
     &              Tile_Data%SOIM2     ( ncols,nrows ),
     &              STAT = ALLOCSTAT ) 
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating MOSAIC deposition velocities'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         Tile_Data%RHOB    = 0.0
         Tile_Data%WSAT    = 0.0
         Tile_Data%WWLT    = 0.0
         Tile_Data%BSLP    = 0.0
         Tile_Data%WRES    = 0.0
         Tile_Data%WFC     = 0.0
         Tile_Data%SOIT2   = 0.0
         Tile_Data%SOIM2   = 0.0

         If( PX_LSM ) Then
            Write(Logdev,*) 'MCIP/WRF used the PX LSM'
            zsoil1 = 0.01
            zsoil2 = 0.05 ! For abflux estimated from 1cm and 1 meter 
! Estimate a constistent soil hydrolic suction at saturation using the JN90 properties to
! calculate soil hydrolic proerties constistant with JN90 data. Note that the suction at 
! feild capacity is typically 33 kPa, wilting point is 1500 kPa and residual water content
! is between 1.0e5 and 1.0e6 kPa and 3.0e5 kPa is used here following Campbell and Norman 1998             
            Psi_Sat =1500.0 * ( WWLT_PX / WSAT_PX )**BSLP_PX 
            WFC_PX  = WSAT_PX * ( 33.0 / Psi_sat )**(-1.0/BSLP_PX)
            WRES_PX = WSAT_PX * ( 3.0e5 / Psi_sat )**(-1.0/BSLP_PX)

            Do C = 1, NCOLS
            Do R = 1, NROWS 
               If( Grid_Data%SLTYP(C,R) .Le. N_SOIL_TYPE ) Then
                  Tile_Data%WSAT( C,R ) = WSAT_PX( Grid_Data%SLTYP( C,R ) )
                  Tile_Data%WFC ( C,R ) = WFC_PX ( Grid_Data%SLTYP( C,R ) )
                  Tile_Data%WWLT( C,R ) = WWLT_PX( Grid_Data%SLTYP( C,R ) )
                  Tile_Data%WRES( C,R ) = WRES_PX( Grid_Data%SLTYP( C,R ) )
                  Tile_Data%BSLP( C,R ) = BSLP_PX( Grid_Data%SLTYP( C,R ) )
                  Tile_Data%RHOB( C,R ) = RHOB_PX( Grid_Data%SLTYP( C,R ) )
               Else
                  Tile_Data%WSAT( C,R ) = WSAT_PX( 3 )
                  Tile_Data%WFC ( C,R ) = WFC_PX ( 3 )
                  Tile_Data%WWLT( C,R ) = WWLT_PX( 3 )
                  Tile_Data%WRES( C,R ) = WRES_PX( 3 )
                  Tile_Data%BSLP( C,R ) = BSLP_PX( 3 )
                  Tile_Data%RHOB( C,R ) = RHOB_PX( 3 )
               End If
            End Do 
            End DO

         Else If( Noah_LSM ) Then
            Write(Logdev,*) 'MCIP/WRF used the Noah LSM'
            zsoil1 = 0.01 ! Adjusted based on Darmenova et al. 2009 doi:10.1029/2008JD011236
            zsoil2 = 0.05 ! For abflux estimated from 1cm and 1 meter 
            Forall( C = 1:NCOLS, R = 1:NROWS )
               Tile_Data%WSAT( C,R ) = WSAT_NOAH( Grid_Data%SLTYP( C,R ) )
               Tile_Data%WWLT( C,R ) = WWLT_NOAH( Grid_Data%SLTYP( C,R ) )
               Tile_Data%WFC ( C,R ) = WFC_NOAH ( Grid_Data%SLTYP( C,R ) )
               Tile_Data%WRES( C,R ) = WRES_NOAH( Grid_Data%SLTYP( C,R ) )
               Tile_Data%BSLP( C,R ) = BSLP_NOAH( Grid_Data%SLTYP( C,R ) )
               Tile_Data%RHOB( C,R ) = RHOB_NOAH( Grid_Data%SLTYP( C,R ) )
            End Forall
         Else If( CLM_LSM ) Then
            Write(Logdev,*) 'MCIP/WRF used the CLM LSM'
            zsoil1 = 0.0175
            zsoil2 = 0.0451
            Forall( C = 1:NCOLS, R = 1:NROWS )
               Tile_Data%WSAT( C,R ) = WSAT_CLM( Grid_Data%SLTYP( C,R ) )
               Tile_Data%WWLT( C,R ) = WWLT_CLM( Grid_Data%SLTYP( C,R ) )
               Tile_Data%WFC ( C,R ) = WFC_CLM ( Grid_Data%SLTYP( C,R ) )
               Tile_Data%WRES( C,R ) = WRES_CLM( Grid_Data%SLTYP( C,R ) )
               Tile_Data%BSLP( C,R ) = BSLP_CLM( Grid_Data%SLTYP( C,R ) )
               Tile_Data%RHOB( C,R ) = RHOB_CLM( Grid_Data%SLTYP( C,R ) )
            End Forall                            
         End If
        
         If( MOSAIC ) Then
            Allocate ( Tile_Data%Lu_Vd     ( ncols,nrows,N_SPC_DEPV,Tile_Data%n_lufrac ), 
     &                 Tile_Data%Ustar     ( ncols,nrows,Tile_Data%n_lufrac ),
     &                 Tile_Data%LAI       ( ncols,nrows,Tile_Data%n_lufrac ),  
     &                 Tile_Data%VEG       ( ncols,nrows,Tile_Data%n_lufrac ),  
     &                 Tile_Data%Z0        ( ncols,nrows,Tile_Data%n_lufrac ),
     &                 Tile_Data%RA        ( ncols,nrows,Tile_Data%n_lufrac ),
     &                 Tile_Data%RSTW      ( ncols,nrows,Tile_Data%n_lufrac ),       
     &                 STAT = ALLOCSTAT )          
            Tile_Data%Lu_Vd     = 0.0
            Tile_Data%Ustar     = 0.0
            Tile_Data%LAI       = 0.0
            Tile_Data%VEG       = 0.0
            Tile_Data%Z0        = 0.0
            Tile_Data%RA        = 0.0
            Tile_Data%RSTW      = 1.0e6
         End If

! Map Met land use to STAGE land use
         Tile_Data%LUfrac = 0.0
         Do n = 1, n_xref_lu
            s = MET_TO_STAGE_LU( n )%Dep_Index 
            l = MET_TO_STAGE_LU( n )%Met_Index
            Tile_Data%LUfrac(:,:,s) = Tile_Data%LUfrac(:,:,s) +
     &            MET_TO_STAGE_LU( n )%Factor * lufrac(:,:,l)
         End Do

         Allocate ( is_ag    ( Tile_Data%n_lufrac ),
     &              is_agmos ( Tile_Data%n_lufrac ), 
     &              is_water ( Tile_Data%n_lufrac ), STAT = ALLOCSTAT )   
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating is_ag, is_agmos, is_water'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         is_ag    = .FALSE.
         is_agmos = .FALSE.
         is_water = .FALSE.

! Get the location of ag and water in the land use fractions
         Do l = 1, Tile_Data%n_lufrac
            If(Tile_Data%cat_lu(l) .Eq. 'AG'   ) Then
               is_ag( l )    = .TRUE.
               l_ag          = l
            End If
            If(Tile_Data%cat_lu(l) .Eq. 'AGMOS') Then
               is_agmos( l ) = .TRUE.
               l_agmos       = l
            End If
            If(Tile_Data%cat_lu(l) .Eq. 'WATER') Then
               is_water( l ) = .TRUE.
            End If
         End Do

         Return   
          
         End Subroutine Init_Mosaic

C=======================================================================
         Subroutine GET_STAGE_MET ( JDATE, JTIME, TSTEP )

         use centralized_io_module
         USE GRID_CONF       ! horizontal & vertical domain specifications
         Use UTILIO_DEFN
#ifdef parallel
         USE SE_MODULES      ! stenex (using SE_COMM_MODULE)
#else
         USE NOOP_MODULES    ! stenex (using NOOP_COMM_MODULE)
#endif

         Implicit None
 
         Include SUBST_FILES_ID  ! file name parameters
         Include SUBST_PE_COMM   ! PE communication displacement and direction

         Integer, Intent( IN ) :: JDATE, JTIME, TSTEP      ! internal simulation date&time
         Character( 16 )  :: PNAME = 'GET_STAGE_MET'
         Character( 240 ) :: xmsg = ' '

C-----------------------------------------------------------------------
C Check consistency with WRF LSM and CMAQ options
C-----------------------------------------------------------------------
         If( .Not. PX_LSM .And. .Not. CLM_LSM .And. .Not. NOAH_LSM ) Then
            XMSG = 'WRF LSM is not supported. PX, CLM and NOAH are supported'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            If( ABFLUX ) Then
               XMSG = 'NH3 bidirectional exchange is not supported using this WRF LSM option. Set CTM_ABFLUX to N.'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End IF
            If( HGBIDI ) Then
               XMSG = 'Hg bidirectional exchange is not supported using this WRF LSM option. Set CTM_HGBIDI to N.'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End IF
         End IF

C Soil vars
#ifdef mpas
         Tile_Data%SOIM2 = smois_data(:,:,2)
#else
         call interpolate_var ('SOIM2', jdate, jtime, Tile_Data%SOIM2)
#endif
         call interpolate_var ('SOIT2', jdate, jtime, Tile_Data%SOIT2)

         Return
         End Subroutine GET_STAGE_MET

      End Module Mosaic_Mod
