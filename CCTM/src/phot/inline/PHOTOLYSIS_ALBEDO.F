
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

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE PHOTOLYSIS_ALBEDO
 
C-----------------------------------------------------------------------
C  FUNCTION: Module contains a function and subroutine use to calculate the diffuse
C  and direct spectral albedo based on the fractional land use for each grid cell
C
C  History:
C  06/04/13 Bill Hutzell - Initial based on the albedo algorithm from phot.F
C                          from CMAQ 5.01
C  08/08/14 Bill Hutzell - 1) commented out snow effect for water surfaces based on 
C                          assumption that snow disolves on contact and causes no
C                          change on reflectivity
C                          2) changed how snow correction for land and sea ice takes
C                          place. The change uses a snow albedo, computed in the
C                          initialization routine instead of using the the snow
C                          correction factor at each call for the surface albedo
C                          calculation. The goal is to make the code easier to modify.
C  02/01/19 David Wong   - Implemented centralized I/O approach, removed all MY_N
C                          clauses
C-----------------------------------------------------------------------

         IMPLICIT NONE 

         REAL, ALLOCATABLE :: SURFACE_ALBEDO( :,:,: )  ! time dependent surface albedo
         REAL, ALLOCATABLE :: DIFFUSE_ALBEDO( :,:,: )  ! time dependent surface albedo
         REAL, ALLOCATABLE :: WATER_FRACTION( :,: )    ! cell fraction covered by water or ocean
         REAL, ALLOCATABLE :: SEAICE        ( :,: )    ! sea ice cover (fraction)
         REAL, ALLOCATABLE :: SNOCOV        ( :,: )    ! snow cover (fractional)
         REAL, ALLOCATABLE :: SNOW_DEPTH    ( :,: )    ! actual snow depth (m)
         REAL, ALLOCATABLE :: SNOW_DEPTHO   ( :,: )    ! actual snow depth for previous timestep(m)
         REAL, ALLOCATABLE :: CALCULATED_SNOCOV( :,: ) ! grid cell snow cover based on landuse fractions (fractional)

         INTEGER :: STDATE ! starting GMT [YYYYDDD]
         INTEGER :: STTIME ! starting time [HHMMSS]
         INTEGER :: PRDATE ! previous GMT [YYYYDDD]
         INTEGER :: PRTIME ! previous time [HHMMSS]
         INTEGER :: STRTHR ! starting GMT hour [HH]
         REAL    :: JYFREQ ! 2PI/(# days in JYEAR)

! public variables:
         PUBLIC SURFACE_ALBEDO, DIFFUSE_ALBEDO, WATER_FRACTION, SEAICE, SNOCOV,
     &          CALCULATED_SNOCOV, STDATE, STTIME, STRTHR, JYFREQ

! public procedures:
         PUBLIC INITIALIZE_ALBEDO, GET_ALBEDO

         PRIVATE

         REAL, PARAMETER :: SEAICE_POINT = 271.36 ! [K] -threshold to form sea ice
                                                  ! based 2005 WRF model Documentation

         CHARACTER( 80 )      :: LAND_SCHEME
         INTEGER              :: NUMB_LANDUSE
         INTEGER              :: N_LAND_CLASSES            ! number of land classes in scheme
         INTEGER              :: N_WATER_CLASSES           ! number of water classes in scheme

         CHARACTER(60), ALLOCATABLE :: LANDUSE_NAME( : )   ! names of landuse categories for the used scheme
         CHARACTER(60), ALLOCATABLE :: TERRAIN     ( : )   ! type of terrestrail surface
         
         REAL,    ALLOCATABLE :: LANDMASK( :,: )           ! land-water mask: 1 for land and 0 for water
         INTEGER, ALLOCATABLE :: ALBMAP_TO_REF( : )        ! map from reference to used landuse for albedo
         REAL,    ALLOCATABLE :: ALBFAC_TO_REF( : )        ! factor from reference to used landuse for albedo

         REAL,    ALLOCATABLE :: TERRAIN_FRACTION( :,:,: )   ! Fractional coverage for terrestrail surface type
         REAL,    ALLOCATABLE :: MEAN_SNOW_BURIAL( :,: )    ! snow and ice fall (cm)
         REAL,    ALLOCATABLE :: LAND_ANNUAL   ( :,:,:,: ) ! annual average of land albedo weighted by class
         REAL,    ALLOCATABLE :: WATER_ANNUAL  ( :,:,:,: ) ! annual average for water albedo weighted class
         REAL,    ALLOCATABLE :: TERRIAN_SNOCOV( :,:,: )  ! snow cover for terrain categories (fractional)
         REAL,    ALLOCATABLE :: TERRAIN_SNOW_BURIAL ( : ) ! reciprocal of critical snow depth, water equivalent (1/m) 
         REAL,    ALLOCATABLE :: LAND_SNOW     ( :,:,:,: ) ! snow covered albedo for land weighted by class
         REAL,    ALLOCATABLE :: MAXIMUM_ALBEDO( : )       ! maximum allowed albedo per wavelength
         REAL,    ALLOCATABLE :: WATER_SEASONAL( : )   ! seasonal coefficient for water albedo
         REAL,    ALLOCATABLE :: WATER_ZENITH  ( : )   ! solar zenith coefficient for water albedo

         REAL,    ALLOCATABLE :: LAND_SEASONAL ( : )   ! seasonal coefficient for land albedo
         REAL,    ALLOCATABLE :: LAND_ZENITH   ( : )   ! solar zenith coefficient for land albedo

         REAL,    ALLOCATABLE :: SEAICE_ANNUAL ( : ) ! annual average for sea ice 
         REAL,    ALLOCATABLE :: SEAICE_SNOW   ( : ) ! snow covered albedo for sea ice 
         REAL                 :: SEAICE_SEASONAL     ! seasonal coefficient for sea ice albedo
         REAL                 :: SEAICE_ZENITH       ! solar zenith coefficient for sea ice albedo
         REAL                 :: SNOW_ZENITH         ! solar zenith coefficient for snow
         REAL                 :: SEAICE_SNOW_BURIAL  ! reciprocal of critical snow depth, water equivalent for sea ice albedo (m)
 
         REAL,    ALLOCATABLE :: SFACTOR_LAND ( : )   ! seasonal correction for land albedo for land class
         REAL,    ALLOCATABLE :: SFACTOR_WATER( : )   ! seasonal correction for water albedo for water class
         REAL,    ALLOCATABLE :: ZFACTOR_LAND ( : )   ! combined seasonal and solar zenith angle correction for land class
         REAL,    ALLOCATABLE :: ZFACTOR_WATER( : )   ! combined seasonal and solar zenith angle correction for water class
         REAL,    ALLOCATABLE    :: TEMPG  ( :,: )       ! ground surface temperature [K]

         REAL WATER_SCALE     ! water scaling factor used to calculate surface albedo
         REAL SEASONAL_COEFF  ! coefficient for seasonal correction to surface albedo
         REAL ZENITH_COEFF    ! coefficient for zenith angle correction to surface albedo
         REAL SNOW_COEFF      ! coefficient for snow cover correction to surface albedo
         REAL SEA_MODULATE    ! seasonal modulation in surface albedo
         REAL ZEN_MODULATE    ! zenith angle modulation in surface albedo

      CONTAINS

         FUNCTION INITIALIZE_ALBEDO( MDATE, MTIME ) RESULT ( SUCCESS )
C...        Function sets up arrays and data needed to calculate surface albedos
C           use in radiative transfer calculation for actinic fluxes

            USE UTILIO_DEFN       ! IOAPI declaratiion and utilities
#ifndef mpas
#ifdef parallel
            USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
            USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif
            USE PHOT_MOD          ! photolysis in-line module
            USE PCGRID_DEFN       ! get cgrid

            USE LSM_MOD, ONLY: N_LUFRAC, LSM_SCHEME => LAND_SCHEME

            USE CENTRALIZED_IO_MODULE, only : interpolate_var, LWMASK, LUFRAC, HAS_SEAICE,
     &                                        HAS_SNOWN, 
     &                                        MODEL_STDATE => STDATE, MODEL_STTIME => STTIME           

            IMPLICIT NONE

            INCLUDE SUBST_FILES_ID   ! file name parameters

C...Arguments:

            INTEGER, INTENT( IN ) :: MDATE     ! Julian date (YYYYDDD)
            INTEGER, INTENT( IN ) :: MTIME     ! time        (HHMMSS)

C...Local:
            REAL                       :: JYEAR                      ! year, ADE
            REAL                       :: MSCALE                    ! scaling factor 
            REAL, ALLOCATABLE          :: FRACTION_LANDUSE( :,:,: ) ! fractional cover for a landuse

            LOGICAL                    :: SUCCESS
            CHARACTER(  2 )            :: LU_INDEX
            CHARACTER( 17 ), PARAMETER :: PNAME  = 'INITIALIZE_ALBEDO' 
            CHARACTER( 16 )            :: VARNM
            CHARACTER( 240 )           :: XMSG   = ' '

            INTEGER                    :: ROW
            INTEGER                    :: COL
            INTEGER                    :: LEV
            INTEGER                    :: SPC
            INTEGER                    :: L
            INTEGER                    :: NL, NW
            INTEGER                    :: V, N, MODE
            INTEGER                    :: ALLOCSTAT
            INTEGER                    :: IWAVE

            LOGICAL, SAVE :: INITIALIZED  = .FALSE.

            IF ( INITIALIZED ) THEN
               RETURN
            END IF

C...compute start time data and frequency of annual cycle considering leap year

            STDATE = MDATE
            STTIME = MTIME
            STRTHR =  MTIME / 10000
            JYEAR  = FLOAT( MDATE / 1000 )   !   Check this more carefully

            IF ( MOD( JYEAR, 4.0 ) .EQ. 0.0 ) THEN
               JYFREQ = 2.0 * PI / 366.0
            ELSE
               JYFREQ = 2.0 * PI / 365.0
            END IF

            SELECT CASE( LSM_SCHEME )
               CASE( 'USGS24' )
                  NUMB_LANDUSE = NUMB_LANDUSE_USGS  ! 24
                  LAND_SCHEME  = LSM_SCHEME
               CASE( 'MODIS' )
                  NUMB_LANDUSE = NUMB_LANDUSE_MODIS ! 33
                  LAND_SCHEME  = LSM_SCHEME
               CASE( 'NLCD50' )
                  NUMB_LANDUSE = NUMB_LANDUSE_NLCD50  ! 50
                  LAND_SCHEME  = LSM_SCHEME
               CASE( 'NLCD40' )
                  NUMB_LANDUSE = NUMB_LANDUSE_NLCD40  ! 40
                  LAND_SCHEME  = LSM_SCHEME
                  IF ( NO_NLCD40 ) THEN
                     XMSG =  'GRID_CRO_2D uses NLCD40 landuse scheme but '
     &                    // 'CSQY_FILE does not have albedo factors for '
     &                    // 'NLCD40'
                     WRITE( LOGDEV,'( A )' ) TRIM( PNAME ) // ' : ' // XMSG
                     SUCCESS = .FALSE.
                     RETURN
                  END IF
               CASE DEFAULT
                  LAND_SCHEME = 'UNKNOWN'
                  NUMB_LANDUSE = 2 ! simple land-water surface albedo
            END SELECT


            IF ( NUMB_LANDUSE .NE. 2 ) THEN ! test N_LUFRAC
               IF ( NUMB_LANDUSE .NE. N_LUFRAC ) THEN
                  XMSG =  TRIM( LAND_SCHEME ) // ' Landuse Scheme from (LSM) does '
     &                 // 'not match number of classes expected in PHOT_OPTICS_DATA '
     &                 // 'file'
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  WRITE(LOGDEV,'( 2(A,I4) )')'PHOT_OPTICS_DATA Value: ',NUMB_LANDUSE,
     &            ' Value from Land Surface Module (LSM): ', N_LUFRAC
                  IF( TRIM( LAND_SCHEME ) .EQ. 'MODIS' .AND. N_LUFRAC .EQ. 20 )THEN
                      NUMB_LANDUSE = 20
                      XMSG = 'Special Case MODIS landuse: MET data has only first 20 classes'
                      WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                      XMSG = 'Albedo calculations use only these classes'
                      WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  ELSE
                      SUCCESS = .FALSE.
                      RETURN
                  END IF
               END IF
            END IF

            ALLOCATE ( ALBMAP_TO_REF( NUMB_LANDUSE ), ALBFAC_TO_REF( NUMB_LANDUSE ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ALBMAP_TO_REF and ALBFAC_TO_REF'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( FRACTION_LANDUSE( NUMB_LANDUSE,NCOLS,NROWS ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating FRACTION_LANDUSE'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( LANDUSE_NAME( NUMB_LANDUSE ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating LANDUSE_NAME'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            LANDUSE_NAME     = ' '            
            FRACTION_LANDUSE = 0.0
            ALBMAP_TO_REF    = -1
            ALBFAC_TO_REF    = 0.0

            SELECT CASE( LAND_SCHEME )
               CASE( 'USGS24' )
                  LANDUSE_NAME ( 1:NUMB_LANDUSE ) = LANDUSE_USGS( 1:NUMB_LANDUSE )
                  ALBMAP_TO_REF( 1:NUMB_LANDUSE ) = ALBMAP_REF2USGS( 1:NUMB_LANDUSE )
                  ALBFAC_TO_REF( 1:NUMB_LANDUSE ) = ALBFAC_REF2USGS( 1:NUMB_LANDUSE )
               CASE( 'MODIS' )
                  LANDUSE_NAME ( 1:NUMB_LANDUSE ) = LANDUSE_MODIS( 1:NUMB_LANDUSE )
                  ALBMAP_TO_REF( 1:NUMB_LANDUSE ) = ALBMAP_REF2MODIS( 1:NUMB_LANDUSE )
                  ALBFAC_TO_REF( 1:NUMB_LANDUSE ) = ALBFAC_REF2MODIS( 1:NUMB_LANDUSE )
               CASE( 'NLCD50' )
                  LANDUSE_NAME ( 1:NUMB_LANDUSE ) = LANDUSE_NLCD50( 1:NUMB_LANDUSE )
                  ALBMAP_TO_REF( 1:NUMB_LANDUSE ) = ALBMAP_REF2NLCD50( 1:NUMB_LANDUSE )
                  ALBFAC_TO_REF( 1:NUMB_LANDUSE ) = ALBFAC_REF2NLCD50( 1:NUMB_LANDUSE )
               CASE( 'NLCD40' )
                  LANDUSE_NAME ( 1:NUMB_LANDUSE ) = LANDUSE_NLCD40( 1:NUMB_LANDUSE )
                  ALBMAP_TO_REF( 1:NUMB_LANDUSE ) = ALBMAP_REF2NLCD40( 1:NUMB_LANDUSE )
                  ALBFAC_TO_REF( 1:NUMB_LANDUSE ) = ALBFAC_REF2NLCD40( 1:NUMB_LANDUSE )
               CASE DEFAULT
                  ALBMAP_TO_REF( 1 ) = INDEX_GRASSLAND_REF
                  LANDUSE_NAME ( 1 ) = 'Grassland'
                  ALBFAC_TO_REF( 1 ) = 1.0
                  ALBMAP_TO_REF( 2 ) = INDEX_OCEAN_REF
                  ALBMAP_TO_REF( 1 ) = INDEX_GRASSLAND_REF
                  LANDUSE_NAME ( 2 ) = 'Ocean'
                  ALBFAC_TO_REF( 2 ) = 1.0
                  N_WATER_CLASSES = 1
                  N_LAND_CLASSES  = 1
            END SELECT

            IF ( LAND_SCHEME .NE. 'UNKNOWN' ) THEN
               WRITE( LOGDEV,'( 5X, A /5X, A )' ) TRIM( PNAME )
     &              // ': Identified ' // TRIM( LAND_SCHEME ) // ' land use scheme',
     &                 ' for surface albedo used by inline photolysis calculation.'

               N_WATER_CLASSES = 0
               DO V = 1, NUMB_LANDUSE
                  IF ( ALBMAP_TO_REF( V ) .EQ. INDEX_OCEAN_REF ) THEN
                      N_WATER_CLASSES = N_WATER_CLASSES + 1
                  END IF
                  DO ROW = 1, NROWS
                     DO COL = 1, NCOLS
                        FRACTION_LANDUSE( V,COL,ROW ) = LUFRAC( COL,ROW,V )
                     END DO
                  END DO
               END DO
               N_LAND_CLASSES = NUMB_LANDUSE - N_WATER_CLASSES
               
               IF ( N_LAND_CLASSES .LE. 0 .OR. N_WATER_CLASSES .GE. NUMB_LANDUSE ) THEN
                    WRITE(LOGDEV,'( A, I3,1X,I3 )')' N_WATER_CLASSES, N_LAND_CLASSES = ',
     &              N_WATER_CLASSES, N_LAND_CLASSES
                    XMSG = 'No Land classes found in ' // GRID_CRO_2D //
     &              'as expected for the ' // TRIM( LAND_SCHEME ) // ' land use scheme'
                    WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                    SUCCESS = .FALSE.
                    RETURN
               END IF
               IF ( N_LAND_CLASSES .GE. NUMB_LANDUSE .OR. N_WATER_CLASSES .LE. 0 ) THEN
                  WRITE(LOGDEV,'( A, I3,1X,I3 )')' N_WATER_CLASSES, N_LAND_CLASSES = ',
     &            N_WATER_CLASSES, N_LAND_CLASSES
                  XMSG = 'No water classes found in ' // GRID_CRO_2D //
     &            'as expected for the ' // TRIM( LAND_SCHEME ) // ' land use scheme'
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  SUCCESS = .FALSE.
                  RETURN
               END IF
            ELSE
               XMSG = ': Undentified ' // TRIM( LAND_SCHEME )
     &              // ' land use scheme for inline photolysis calculation.'
     &              // ' Using default land-water albedo for inline photolysis'
     &              // ' calculation.'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               DO ROW = 1, NROWS
                  DO COL = 1, NCOLS
                     IF ( LWMASK( COL,ROW ) .LT. 0.5 ) THEN
                        FRACTION_LANDUSE( 2,COL,ROW ) = 1.0
                     ELSE
                        FRACTION_LANDUSE( 1,COL,ROW ) = 1.0
                     END IF
                  END DO
               END DO
            END IF

            ALLOCATE ( SNOCOV( NCOLS,NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating SNOCOV array'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( SNOW_DEPTH( NCOLS,NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating SNOW_DEPTH array'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF


            IF ( .NOT. HAS_SNOWN ) THEN

               ALLOCATE ( SNOW_DEPTHO( NCOLS,NROWS ), STAT = ALLOCSTAT )
               IF ( ALLOCSTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating SNOW_DEPTH array'
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  SUCCESS = .FALSE.
                  RETURN
               END IF
               
               call interpolate_var ('SNOWH', MODEL_STDATE, MODEL_STTIME, SNOW_DEPTHO)
            
            END IF
            
            PRDATE = MODEL_STDATE
            PRTIME = MODEL_STTIME

            ALLOCATE ( MEAN_SNOW_BURIAL( NCOLS,NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating MEAN_SNOW_BURIAL array'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF
            
            MEAN_SNOW_BURIAL = 0.0


            ALLOCATE ( CALCULATED_SNOCOV( NCOLS,NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating CALCULATED_SNOCOV array'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( TERRIAN_SNOCOV(  N_LAND_CLASSES,NCOLS,NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating TERRIAN_SNOCOV array'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( SEAICE( NCOLS,NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating SEAICE array'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            IF ( .NOT. HAS_SEAICE ) THEN
               XMSG = 'MET_CRO_2D DOES NOT CONTAIN SEA ICE DATA. THE SURFACE ALBEDO '
     &              // 'DOES NOT INCLUDE ITS EFFECTS. Setting to one if water surface '
     &              // 'temperaure is less than 271.36K (WRF formation threshold).'
               WRITE( LOGDEV, '(A)' ) XMSG
               ALLOCATE ( TEMPG( NCOLS,NROWS  ), STAT = ALLOCSTAT )
               IF ( ALLOCSTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating TEMPG array'
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  SUCCESS = .FALSE.
                  RETURN
               END IF
            END IF

            IF ( .NOT. HAS_SNOWN ) THEN
               XMSG = 'MET_CRO_2D DOES NOT CONTAIN SNOW FALL DATA. THE SURFACE ALBEDO '
     &              // ' cannot determine age of snow covered surfaces'
               WRITE( LOGDEV, '(A)' ) XMSG
            END IF

            ALLOCATE ( MAXIMUM_ALBEDO( NWL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating MAXIMUM_ALBEDO'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( SURFACE_ALBEDO( NWL,NCOLS,NROWS ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating SURFACE_ALBEDO'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( DIFFUSE_ALBEDO( NWL,NCOLS,NROWS ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating DIFFUSE_ALBEDO'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( LAND_ANNUAL ( N_LAND_CLASSES,NWL,NCOLS,NROWS ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating LAND_ANNUAL'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( LAND_SEASONAL( N_LAND_CLASSES ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating LAND_SEASONAL'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( TERRAIN( N_LAND_CLASSES ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating TERRAIN'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( TERRAIN_FRACTION( N_LAND_CLASSES,NCOLS,NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating TERRAIN_FRACTION'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( LAND_ZENITH( N_LAND_CLASSES ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating LAND_ZENITH'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( TERRAIN_SNOW_BURIAL( N_LAND_CLASSES ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating TERRAIN_SNOW_BURIAL' 
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( LAND_SNOW( N_LAND_CLASSES,NWL,NCOLS,NROWS ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ALBEDO_SNOW'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( SFACTOR_LAND( N_LAND_CLASSES ), ZFACTOR_LAND( N_LAND_CLASSES ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating SFACTOR_LAND and ZFACTOR_LAND'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( WATER_ANNUAL( N_WATER_CLASSES,NWL,NCOLS,NROWS ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating WATER_ANNUAL'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE( SFACTOR_WATER( N_LAND_CLASSES ), ZFACTOR_WATER( N_WATER_CLASSES ),
     &                STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating SFACTOR_WATER and ZFACTOR_WATER'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( SEAICE_ANNUAL( NWL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating SEAICE_ANNUAL'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( SEAICE_SNOW( NWL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating SEAICE_SNOW'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( WATER_FRACTION( NCOLS,NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating WATER_FRACTION'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( WATER_SEASONAL( N_WATER_CLASSES ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating WATER_SEASONAL'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( WATER_ZENITH( N_WATER_CLASSES ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating WATER_ZENITH'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            SURFACE_ALBEDO  = 0.0
            DIFFUSE_ALBEDO  = 0.0
            LAND_ANNUAL     = 0.0
            LAND_SEASONAL   = 0.0
            LAND_ZENITH     = 0.0
            LAND_SNOW       = 0.0

C...determine average albedo and its adjustment factors for zenith angle, season and snow cover

            WATER_FRACTION = 0.0

            WATER_SEASONAL = 1.0
            WATER_ZENITH   = 0.0
            WATER_ANNUAL   = 0.0

            TERRAIN(:)(1:60)  = " "
            TERRAIN_FRACTION  = 0.0
            LAND_SEASONAL  = 1.0
            LAND_ZENITH    = 0.0
            LAND_SNOW      = 0.0
            LAND_ANNUAL    = 0.0

            MAXIMUM_ALBEDO = 0.0

            NW = 0
            NL = 0
!           INDEX_SNOW = 15 ! permenant snow
            INDEX_SNOW = 19 ! fresh snow

!            DO V = 1, NUMB_LANDUSE
! set values for maximum allowed albedo; should correspond to fresh snow           
!               L = ALBMAP_TO_REF( V )
!               DO IWAVE = 1, NWL
!                  MAXIMUM_ALBEDO( IWAVE ) = MAX( MAXIMUM_ALBEDO( IWAVE ),
!     &                                      ALBFAC_TO_REF( V )* SPECTRAL_ALBEDO_REF( IWAVE, L ) )
!               END DO                 
!            END DO
             L = INDEX_SNOW
             SNOW_ZENITH = ZENITH_COEFF_REF( INDEX_SNOW )
             DO IWAVE = 1, NWL
                MAXIMUM_ALBEDO( IWAVE ) = SPECTRAL_ALBEDO_REF( IWAVE, INDEX_SNOW ) 
             END DO                 
             WRITE(LOGDEV,'(A,21(1X,F7.3))')'Fresh Snow Albedo ',
     &       (  MAXIMUM_ALBEDO( IWAVE ),IWAVE=1,NWL ) 
            
            DO V = 1, NUMB_LANDUSE
               L = ALBMAP_TO_REF( V )
               IF ( L .EQ. INDEX_OCEAN_REF ) THEN
                  NW = NW + 1
                  WATER_SEASONAL( NW ) = SEASON_COEFF_REF( L )
                  WATER_ZENITH  ( NW ) = ZENITH_COEFF_REF( L )
                  DO ROW = 1, NROWS
                     DO COL = 1, NCOLS
                        WATER_FRACTION( COL,ROW ) = WATER_FRACTION( COL,ROW )
     &                                             + FRACTION_LANDUSE( V,COL,ROW )
                        WATER_SCALE = ALBFAC_TO_REF( V ) * FRACTION_LANDUSE( V,COL,ROW )
                        DO IWAVE = 1, NWL
                           WATER_ANNUAL( NW,IWAVE,COL,ROW ) = WATER_SCALE * SPECTRAL_ALBEDO_REF( IWAVE, L )
                        END DO          
                     END DO
                  END DO 
               ELSE
                  NL = NL + 1
                  TERRAIN( NL ) = LANDUSE_NAME( V )
                  LAND_SEASONAL( NL ) = SEASON_COEFF_REF( L )
                  LAND_ZENITH  ( NL ) = ZENITH_COEFF_REF( L )
                  TERRAIN_SNOW_BURIAL( NL ) = 1.0/MAX( 1.0E-25,CRITICAL_SNOW_DEPTH_REF( L ) )
                  MSCALE = ALBFAC_TO_REF( V )*SNOW_COEFF_REF( L )
                  WRITE(LOGDEV,'(A,21(1X,F7.3))')'Snow Albedo for ' // TRIM(TERRAIN( NL )),
     &            (  MAXIMUM_ALBEDO( IWAVE ),IWAVE=1,NWL ) 
                  DO ROW = 1, NROWS
                     DO COL = 1, NCOLS
                        MSCALE = ALBFAC_TO_REF( V ) * FRACTION_LANDUSE( V, COL, ROW )
                        TERRAIN_FRACTION( NL,COL,ROW ) = FRACTION_LANDUSE( V, COL, ROW )
                        DO IWAVE = 1, NWL
                           LAND_ANNUAL( NL,IWAVE,COL,ROW ) = MSCALE * SPECTRAL_ALBEDO_REF( IWAVE, L )
!                           LAND_SNOW  ( NL,IWAVE,COL,ROW ) = MIN(SNOW_COEFF_REF( L ) * LAND_ANNUAL( NL,IWAVE,COL,ROW ),
!     &                                                           FRACTION_LANDUSE( V, COL, ROW )*MAXIMUM_ALBEDO( IWAVE ) ) 
                           LAND_SNOW  ( NL,IWAVE,COL,ROW ) = FRACTION_LANDUSE( V, COL, ROW )*MAXIMUM_ALBEDO( IWAVE )
                        END DO
                     END DO
                  END DO 
               END IF               
            END DO                       

            DO ROW = 1, NROWS
               DO COL = 1, NCOLS
                  DO V = 1, NUMB_LANDUSE
                     L = ALBMAP_TO_REF( V )
                     IF ( L .NE. INDEX_OCEAN_REF ) THEN                           
                         MEAN_SNOW_BURIAL( COL,ROW ) = MEAN_SNOW_BURIAL( COL,ROW )
     &                                               + FRACTION_LANDUSE( V, COL, ROW )*CRITICAL_SNOW_DEPTH_REF( L )                                              
                     END IF
                  END DO
                  IF( MEAN_SNOW_BURIAL( COL,ROW ) .LE. 1.0E-25 )THEN
                      MEAN_SNOW_BURIAL( COL,ROW ) = -1.0
                  ELSE 
                      MEAN_SNOW_BURIAL( COL,ROW ) = 1.0/MEAN_SNOW_BURIAL( COL,ROW )
                  END IF
               END DO
            END DO

C...set up sea ice annual and snow albedos along with correction factors

            L = INDEX_SEA_ICE
            SEAICE_SEASONAL = SEASON_COEFF_REF( L )
            SEAICE_ZENITH   = ZENITH_COEFF_REF( L )
            SEAICE_SNOW_BURIAL = 1.0/MAX( 1.0E-25,CRITICAL_SNOW_DEPTH_REF( L ) )
            DO IWAVE = 1, NWL
               SEAICE_ANNUAL( IWAVE ) = SPECTRAL_ALBEDO_REF( IWAVE, L )
!               SEAICE_SNOW  ( IWAVE ) = MIN( SNOW_COEFF_REF( L ) * SEAICE_ANNUAL( IWAVE ),
!     &                                       MAXIMUM_ALBEDO( IWAVE ) ) 
               SEAICE_SNOW  ( IWAVE ) = MAXIMUM_ALBEDO( IWAVE ) 
              write(logdev,'(A,21(1X,F7.3))')'SEAICE_ANNUAL, SEAICE_SNOW, SEAICE_SEASONAL, SEAICE_ZENITH = ', 
     &         SEAICE_ANNUAL( IWAVE), SEAICE_SNOW( IWAVE ),SEAICE_SEASONAL,SEAICE_ZENITH
            END DO
         
            SUCCESS = .TRUE.

            
            
            RETURN
            
         END FUNCTION INITIALIZE_ALBEDO
            
         SUBROUTINE GET_ALBEDO( MDATE, MTIME, COSZENS, LAT, LON )
            
C... Subroutine calculates diffuse and direct surface albedo versus wavelength over a set of
C    latitudes and longitudes

            USE UTILIO_DEFN       ! IOAPI declaratiion and utilities
            USE PHOT_MOD          ! photolysis in-line module
#ifndef mpas
#ifdef parallel
            USE SE_MODULES        ! stenex (using SE_UTIL_MODULE)
#else
            USE NOOP_MODULES      ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif
            USE PCGRID_DEFN       ! get cgrid

            USE LSM_MOD, ONLY: N_LUFRAC, LSM_SCHEME => LAND_SCHEME

            USE CENTRALIZED_IO_MODULE, only : interpolate_var, HAS_SEAICE, HAS_SNOWN, LUFRAC

            USE ASX_DATA_MOD, Only: Met_Data

            IMPLICIT NONE

            INCLUDE SUBST_FILES_ID   ! file name parameters

C arguments:

            INTEGER, INTENT( IN ) :: MDATE            ! Julian date (YYYYDDD)
            INTEGER, INTENT( IN ) :: MTIME            ! time        (HHMMSS)
            REAL,    INTENT( IN ) :: COSZENS( :,: )   ! cosine of the solar zenith angle
            REAL,    INTENT( IN ) :: LAT( :,: )       ! north lat at cell center [deg]
            REAL,    INTENT( IN ) :: LON( :,: )       ! west long at cell center [deg] 

C local:
            REAL, PARAMETER             :: SNOWH_TO_WATERD  = 0.10 ! convert actual snow depth (m) to water equivalent depth (cm)
            REAL, PARAMETER             :: SNOW_SHAPE       =  2.6 ! Snow Shape Distribution Parameter 
            REAL, PARAMETER             :: HOURS2DAYS       =  1.0/24.0
            REAL, PARAMETER             :: SECS2HOURS       =  1.0/3600.0
            
! SNOW_SHAPE value from 
! Koren, V., Schaake, J., Mitchell, K., Duan, Q.-Y., Chen, F., and Baker, J. M. (1999),
! A parameterization of snowpack and frozen ground intended for NCEP weather and 
! climate models, J. Geophys. Res., 104(D16), 19569–19585, doi:10.1029/1999JD900232. 

            REAL                        :: DELTA_TIME    ! time since initialization call [hr]
            REAL                        :: CALL_DTIME    ! time since last call [hr]
            REAL                        :: CURRENT_HOUR  ! current GMT hour [hr]
            REAL                        :: JULIAN_DAY    ! julian day       [days]
            REAL                        :: CURRHR_LST    ! local standard time at each grid cell
            REAL                        :: EQUATION_TIME ! equation of time
            REAL                        :: COSZEN        ! working cosine of the solar zenith angle
            REAL                        :: SINLAT        ! sine of latitude
            REAL                        :: COSLAT        ! cosine of latitude
            REAL                        :: MSCALE        ! scaling factor 
            REAL                        :: MAX_FACTOR    ! scaling factor 

            REAL                        :: ALBEDO_LAND   ! scratch variable for land fraction
            REAL                        :: ALBEDO_WATER  ! scratch variable for water fraction
            REAL                        :: ALBEDO_SEAICE ! scratch variable for seaice fraction

            REAL                        :: SNOCOV_SEAICE  ! calculated seaice snow cover
            REAL                        :: SFACTOR_SEAICE ! seasonal correction for seaice albedo
            REAL                        :: ZFACTOR_SEAICE ! solar zenith angle correction for seaice albedo
            REAL                        :: ZFACTOR_SNOW   ! solar zenith angle correction for snow albedo
            REAL                        :: SNOW_FREE      ! snow free fraction of cell
            REAL                        :: ICE_FREE       ! water fraction free sea ice 
            REAL                        :: SNOWAGE_FACTOR ! correction for age of snow cover
            REAL                        :: SNOW_ALBEDO    ! albedo for snow covered area
            REAL                        :: WEIGHT_CHECK
            REAL                        :: WEIGHT_SUM
            REAL                        :: FREQUENCY      ! (1/hr)
            
            CHARACTER(  17 ), PARAMETER :: PNAME  = 'GET_ALBEDO'
            CHARACTER(  16 )            :: VARNM
            CHARACTER( 240 )            :: XMSG   = ' '

            INTEGER                     :: ROW
            INTEGER                     :: COL
            INTEGER                     :: LEV
            INTEGER                     :: SPC
            INTEGER                     :: L
            INTEGER                     :: IWAVE
            INTEGER                     :: NW, NL
            INTEGER                     :: V, N, MODE
            INTEGER                     :: ALLOCSTAT

C...Read & Interpolate SNOCOV and SNOW_DEPTH

            call interpolate_var ('SNOCOV', mdate, mtime, SNOCOV)

            call interpolate_var ('SNOWH', mdate, mtime, SNOW_DEPTH)

            call interpolate_var ('SEAICE', mdate, mtime, SEAICE)

            call interpolate_var ('TEMPG', mdate, mtime, TEMPG)


C...Calculate current hour in GMT and julian day

            DELTA_TIME  =  REAL( SECSDIFF( STDATE, STTIME, MDATE, MTIME ), 4 )
     &                  *  SECS2HOURS
            CALL_DTIME  =  REAL( SECSDIFF( PRDATE, PRTIME, MDATE, MTIME ), 4 )
     &                        *  SECS2HOURS
            IF ( CALL_DTIME .LE. 1.0e-25 ) THEN
               FREQUENCY = 0.0
            ELSE
               FREQUENCY = 1.0 / CALL_DTIME
            END IF                            
            CURRENT_HOUR = REAL( STRTHR, 4 ) + DELTA_TIME 

            JULIAN_DAY   = REAL(MOD( MDATE, 1000 ), 4 )
            

C...Calculate cosines of the zenith angles

            DO ROW = 1, NROWS
               DO COL = 1, NCOLS

                  SINLAT = SIN( PI180 * LAT ( COL,ROW ) )
                  COSLAT = COS( PI180 * LAT ( COL,ROW ) )

C...correct  CURRHR for current *positive* West longitude convention
C...  to obtain LST.
C...this convention on longititude should be reexamined for different domains

            
                  CURRHR_LST = CURRENT_HOUR + LON( COL,ROW ) / 15.0

                  IF ( .NOT. HAS_SEAICE ) THEN ! determine sea ice can form
                     IF ( MET_DATA%TEMPG( COL,ROW ) .LT. SEAICE_POINT .AND.
     &                    WATER_FRACTION( COL,ROW ) .GE. 0.95 ) THEN
                        SEAICE( COL,ROW ) = 1.0
                     ELSE
                        SEAICE( COL,ROW ) = 0.0
                     END IF
                  END IF

                 IF ( .NOT. HAS_SNOWN ) THEN

                     IF( FREQUENCY*( SNOW_DEPTH( COL,ROW )-SNOW_DEPTHO( COL,ROW ) ) .GT. 0.01 
     &                     .OR. SNOCOV( COL,ROW ) .LT. 1.0E-25 ) THEN
!   A new or renewed snow cover                         
                        MET_DATA%SNOWAGE( COL,ROW ) = 0.0
                     ELSE IF ( SNOCOV( COL,ROW ) .GT. 1.0E-25 ) THEN
!   Aging snow cover                     
                        MET_DATA%SNOWAGE( COL,ROW ) = MET_DATA%SNOWAGE( COL,ROW ) 
     &                                              + CALL_DTIME*HOURS2DAYS
                     ELSE
!   No snow falling or existing snow cover                     
                        MET_DATA%SNOWAGE( COL,ROW ) = 0.0
                     END IF 
                     SNOW_DEPTHO( COL,ROW ) = SNOW_DEPTH( COL,ROW )
                     
                 END IF
                 
                 IF ( SNOW_DEPTH( COL,ROW ) .LE. 1.0E-6 .AND. SEAICE( COL,ROW ) .LE. 1.0E-6 ) THEN
                    SNOWAGE_FACTOR = 0.0                 
                    MAX_FACTOR     = 1.0
                 ELSE IF ( SEAICE( COL,ROW ) .GT. 1.0E-6 .AND. SNOCOV( COL,ROW ) .LE. 1.0E-6 ) THEN
                    SNOWAGE_FACTOR = 0.0
                    MAX_FACTOR     = 1.0
                 ELSE
C...compute correction factor for snow aging from 
C...Livneh et al. (2010), J. Hydromet., vol 11(3), pp 721-738,
C...DOI: https://doi.org/10.1175/2009JHM1174.1                  
                    IF (   MET_DATA%TEMPG( COL,ROW ) .LT. 272.0 ) THEN ! accumulation phase
                        SNOWAGE_FACTOR = 0.94**MET_DATA%SNOWAGE( COL,ROW )**0.58
                    ELSE ! ablation phase 
                        SNOWAGE_FACTOR = 0.82**MET_DATA%SNOWAGE( COL,ROW )**0.46                                     
                    END IF
                    SNOWAGE_FACTOR = MAX( 0.43,SNOWAGE_FACTOR )
                    MAX_FACTOR     = SNOWAGE_FACTOR
                 END IF

!                 IF ( SNOW_DEPTH( COL,ROW ) .GT. 1.0E-6 .AND. SEAICE( COL,ROW ) .LE. 1.0E-6 )THEN
!                    WRITE(6,'(A,2(I4,1X),6(F7.3,1X))')'COL,ROW,SNOWAGE,SNOWAGE_FACTOR = ',COL,ROW,
!     &              MET_DATA%SNOWAGE(COL,ROW),SNOWAGE_FACTOR
!                 ELSE IF ( SEAICE( COL,ROW ) .GT. 1.0E-6 .AND. SNOCOV( COL,ROW ) .GT. 1.0E-6 )THEN
!                    WRITE(6,'(A,2(I4,1X),6(F7.3,1X))')'COL,ROW,SEAICE,SNOWAGE,SNOWAGE_FACTOR = ',COL,ROW,
!     &              SEAICE( COL,ROW ),MET_DATA%SNOWAGE(COL,ROW),SNOWAGE_FACTOR                 
!                 END IF 

C...determine seasonal and snow corrections to surface albedo
C...  convert julian into time of year for grid cell
C...  seasonal adjustment has an 11 day phase delay in the solar cycle

                  IF ( LAT( COL,ROW ) .GE. 0.0 ) THEN
                     SEA_MODULATE = COS( JYFREQ * ( JULIAN_DAY + CURRHR_LST / 24.0 + 11.0 ) )
                  ELSE
                     SEA_MODULATE = COS( JYFREQ * ( JULIAN_DAY + CURRHR_LST / 24.0 + 11.0 ) + PI )
                  END IF

                  IF ( SEA_MODULATE .GE. 0.0 ) THEN
                     MSCALE = 0.5 * ( 1.0 + SQRT( SEA_MODULATE ) )
                  ELSE
                     SEA_MODULATE = ABS( SEA_MODULATE )
                     MSCALE = 0.5 * ( 1.0 - SQRT( SEA_MODULATE ) )
                  END IF

!.. MSCALE equals 1 and 0 on winter and summer soltices, respectively
!... Note that seasonal factors are equal to or less than 1.0
 
                  FORALL ( NL = 1:N_LAND_CLASSES )
                     SFACTOR_LAND( NL ) = 1.0 /( 1.0 + MSCALE * (LAND_SEASONAL ( NL ) - 1.0) )
                  END FORALL
! assume that open water has no other effect than sea ice that is a seasonal effect
!                 FORALL ( NW = 1:N_WATER_CLASSES )
!                    SFACTOR_WATER( NW ) = 1.0 /( 1.0 + MSCALE * (WATER_SEASONAL( NW ) - 1.0) )
!                 END FORALL
!                 SFACTOR_SEAICE = 1.0 /( 1.0 + MSCALE * (SEAICE_SEASONAL-1.0) )

C..Determine zenith angle correction to albedos
C...First, test whether zenith angle is greater than 90 degrees.
                  IF ( COSZENS( COL,ROW ) .LE. 0.0 ) THEN
                     FORALL ( NL = 1:N_LAND_CLASSES )
                        ZFACTOR_LAND( NL ) = MAX( 0.8, ( 1.0 + LAND_ZENITH( NL ) ) )
                        ZFACTOR_LAND( NL ) = ZFACTOR_LAND( NL ) * SFACTOR_LAND( NL )
                     END FORALL
! Note that water zenith correction is later combined with seasonal correction
                     FORALL ( NW = 1:N_WATER_CLASSES )
                        ZFACTOR_WATER( NW ) = MAX( 0.8,( 1.0 + WATER_ZENITH( NW ) ) )
                     END FORALL
                     ZFACTOR_SEAICE = MAX( 0.8, ( 1.0 + SEAICE_ZENITH ) )
                     ZFACTOR_SNOW   = MAX( 0.8, ( 1.0 + SNOW_ZENITH ) )
                  ELSE
                     FORALL ( NL = 1:N_LAND_CLASSES )
                        ZFACTOR_LAND( NL ) = MAX( 0.8, ( 1.0 + LAND_ZENITH( NL ) )
     &                                     / ( 1.0 + 2.0 * COSZENS( COL,ROW ) * LAND_ZENITH( NL ) ) )
                        ZFACTOR_LAND( NL ) = ZFACTOR_LAND( NL ) * SFACTOR_LAND( NL )
                     END FORALL
! Note that water zenith correction is later combined with seasonal correction
                     FORALL ( NW = 1:N_WATER_CLASSES )
                        ZFACTOR_WATER( NW ) = MAX( 0.8, ( 1.0 + WATER_ZENITH( NW ) )
     &                                      / ( 1.0 + 2.0 * COSZENS( COL,ROW ) * WATER_ZENITH( NW ) ) )
                     END FORALL
                     ZFACTOR_SEAICE = MAX( 0.8, ( 1.0 + SEAICE_ZENITH )
     &                              / ( 1.0 + 2.0 * COSZENS( COL,ROW ) * SEAICE_ZENITH ) )
                     ZFACTOR_SNOW   = MAX( 0.8, ( 1.0 + SNOW_ZENITH )
     &                              / ( 1.0 + 2.0 * COSZENS( COL,ROW ) * SNOW_ZENITH ) )
                  END IF

                  SNOW_FREE = MAX( ( 1.0 - SNOCOV( COL,ROW ) ), 0.0 )
                  ICE_FREE  = MAX( ( 1.0 - SEAICE( COL,ROW ) ), 0.0 )

!...Update the season and zenith corrections for water and sea ice based on ice and water coverage, in
!...the case for sea ice  
 
                  FORALL ( NW = 1:N_WATER_CLASSES )
                     SFACTOR_WATER( NW ) = ICE_FREE  !!! * SFACTOR_WATER( NW )
                     ZFACTOR_WATER( NW ) = ZFACTOR_WATER( NW ) * SFACTOR_WATER( NW )
                  END FORALL

                  SFACTOR_SEAICE = SEAICE( COL,ROW ) * WATER_FRACTION( COL,ROW )

                  
! determine snow cover based on properties of each landuse category
                  CALCULATED_SNOCOV( COL,ROW ) = 0.0
                  IF (  SEAICE( COL,ROW ) .GT. 0.0 ) THEN
! 1) add sea ice but use snow cover from METCRO2D file because the PX land surface model set snow height from sea ice
                     SNOCOV_SEAICE = SNOCOV( COL,ROW )
                     CALCULATED_SNOCOV( COL,ROW ) = SFACTOR_SEAICE * SNOCOV_SEAICE
                  END IF
                  IF ( MET_DATA%SNOWH( COL,ROW ) .GT. 1.0E-6 ) THEN
! 2) add land classes based on surface type
                     DO NL = 1, N_LAND_CLASSES
                        IF (  TERRAIN_FRACTION( NL,COL,ROW ) .GT. 1.0E-25 ) THEN
                           MSCALE = SNOWH_TO_WATERD * SNOW_DEPTH( COL,ROW )*TERRAIN_SNOW_BURIAL( NL ) 
                           IF ( MSCALE .LT. 1.0 ) THEN                      
                              TERRIAN_SNOCOV( NL,COL,ROW ) = 1.0
     &                                                     - EXP( -SNOW_SHAPE * MSCALE ) 
     &                                                     + MSCALE * EXP( -SNOW_SHAPE )
                              TERRIAN_SNOCOV( NL,COL,ROW ) = MAX( 0.0,
     &                                                       MIN( 1.0,TERRIAN_SNOCOV( NL,COL,ROW ) ) )
                           ELSE
                              TERRIAN_SNOCOV( NL,COL,ROW ) = 1.0
                           END IF 
                           CALCULATED_SNOCOV( COL,ROW ) = CALCULATED_SNOCOV( COL,ROW )
     &                                                  + TERRIAN_SNOCOV( NL,COL,ROW )
     &                                                  * TERRAIN_FRACTION( NL,COL,ROW )
                        ELSE
                           TERRIAN_SNOCOV( NL,COL,ROW ) = 0.0
                        END IF
                     END DO


                  ELSE
                     TERRIAN_SNOCOV( :,COL,ROW ) = 0.0
                  END IF

!                  MSCALE = SNOWH_TO_WATERD * SNOW_DEPTH( COL,ROW )*MEAN_SNOW_BURIAL( COL,ROW ) 
!                  IF ( MSCALE .LT. 1.0 ) THEN
!                      MSCALE =  1.0 - EXP( -SNOW_SHAPE*MSCALE ) + MSCALE*EXP( -SNOW_SHAPE )
!                  ELSE
!                      MSCALE = 1.0
!                  END IF
!                  CALCULATED_SNOCOV( COL,ROW ) = CALCULATED_SNOCOV( COL,ROW )
!     &                                         + MSCALE ! *SUM( TERRAIN_FRACTION( :,COL,ROW ) )

                  
                  DO IWAVE = 1, NWL

C...compute seasonal diffuse albedos for land, water and seaice separately

                     ALBEDO_LAND  = 0.0
                     ALBEDO_WATER = 0.0
                     SNOW_ALBEDO  = 0.0
                     MSCALE       = MAX_FACTOR*MAXIMUM_ALBEDO( IWAVE )


                     DO NL = 1, N_LAND_CLASSES
!                       IF ( LAND_ANNUAL( NL,IWAVE,COL,ROW ) .GE. 1.0E-6 ) THEN
                        IF ( TERRAIN_FRACTION( NL,COL,ROW ) .GT. 1.0E-25 ) THEN
                           SNOW_ALBEDO = SNOWAGE_FACTOR * LAND_SNOW( NL,IWAVE,COL,ROW ) 
                           ALBEDO_LAND  = ALBEDO_LAND +  SFACTOR_LAND( NL )
     &                                  * (1.0-TERRIAN_SNOCOV( NL,COL,ROW ))* LAND_ANNUAL( NL,IWAVE,COL,ROW )
     &                                  + SNOW_ALBEDO
!     &                                  * ( (1.0-TERRIAN_SNOCOV( NL,COL,ROW ))* LAND_ANNUAL( NL,IWAVE,COL,ROW )
!     &                                  +   SNOW_ALBEDO )
                        END IF
                     END DO
                     DO NW = 1, N_WATER_CLASSES
                        IF ( WATER_ANNUAL( NW,IWAVE,COL,ROW ) .LT. 1.0E-6 ) CYCLE
                        ALBEDO_WATER = ALBEDO_WATER  
     &                               + SFACTOR_WATER( NW ) * WATER_ANNUAL( NW,IWAVE,COL,ROW )
                     END DO

                     IF (  SFACTOR_SEAICE .GE. 1.0E-6 ) THEN
                        SNOW_ALBEDO = SNOWAGE_FACTOR * SEAICE_SNOW( IWAVE )
!                       SNOW_ALBEDO = SNOCOV( COL,ROW )*MAX( 0.40*SFACTOR_SEAICE,SNOW_ALBEDO )
!                WRITE(6,'(A,2(I4,1X),6(F7.3,1X))')'COL,ROW,SEAICE,SNOW_ALBEDO,SNOWAGE_FACTOR = ',COL,ROW,
!    &           SEAICE( COL,ROW ),SNOW_ALBEDO,SNOWAGE_FACTOR
                     ELSE 
                        SNOW_ALBEDO = 0.0
                     END IF


                     ALBEDO_SEAICE = SFACTOR_SEAICE
     &                             * ( SNOW_FREE * SEAICE_ANNUAL( IWAVE )
     &                             +   SNOW_ALBEDO  )

C...sum for net diffuse albedo

                     DIFFUSE_ALBEDO( IWAVE,COL,ROW ) = ALBEDO_LAND 
     &                                               + ALBEDO_WATER 
     &                                               + ALBEDO_SEAICE

                     DIFFUSE_ALBEDO( IWAVE,COL,ROW ) = MIN( MSCALE, DIFFUSE_ALBEDO( IWAVE,COL,ROW ) )

C...Calculate albedos for combined correction from solar zenith angle and season

                     ALBEDO_LAND  = 0.0
                     ALBEDO_WATER = 0.0
                     WEIGHT_CHECK = 0.0
                     WEIGHT_SUM   = 0.0

                     DO NL = 1, N_LAND_CLASSES
                        IF ( TERRAIN_FRACTION( NL,COL,ROW ) .GT. 1.0E-25 ) THEN
                           SNOW_ALBEDO = SNOWAGE_FACTOR * LAND_SNOW( NL,IWAVE,COL,ROW ) 
                           ALBEDO_LAND = ALBEDO_LAND + ZFACTOR_LAND( NL ) 
     &                                 * (1.0-TERRIAN_SNOCOV( NL,COL,ROW )) * LAND_ANNUAL( NL,IWAVE,COL,ROW )
     &                                 + ZFACTOR_SNOW * TERRIAN_SNOCOV( NL,COL,ROW ) * SNOW_ALBEDO 
!                           WEIGHT_CHECK = (1.0-TERRIAN_SNOCOV( NL,COL,ROW )) * LAND_ANNUAL(NL,IWAVE,COL,ROW )
!     &                                 +   TERRIAN_SNOCOV( NL,COL,ROW ) * SNOW_ALBEDO
!                           WEIGHT_SUM   = WEIGHT_CHECK + WEIGHT_SUM

!!!!!!             IF ( ALBEDO_LAND .GT. 0.500  .AND. IWAVE .LT. 2 )THEN
!                 IF ( ALBEDO_LAND .GT. 0.82900  )THEN
!                   WRITE(LOGDEV,'(A,2(I3,1X),A20,2(I4,1X),12(F9.5,1X))')
!    &              'IWAVE,NL,TERRAIN,COL,ROW,FRACTION,SNOWAGE_FACTOR,SNOCOV,' // 
!    &              'SNOW_ALBEDO,SNOW_CLEAR,LAND_ALBEDO,ALBEDO_LAND,WEIGHT_CHECK,DIR_ALBEDO = ',
!    &              IWAVE,NL,TERRAIN(NL),COL,ROW,TERRAIN_FRACTION( NL,COL,ROW),SNOWAGE_FACTOR,
!    &              TERRIAN_SNOCOV(NL,COL,ROW),SNOW_ALBEDO,(1.0-TERRIAN_SNOCOV( NL,COL,ROW )), 
!    &              LAND_ANNUAL( NL,IWAVE,COL,ROW ),
!    &              ALBEDO_LAND,WEIGHT_CHECK,WEIGHT_SUM
!                 END IF

                        END IF
                     END DO

!                 IF ( ALBEDO_LAND .GT. 0.9500 )THEN
!                   WRITE(LOGDEV,'(A,2(I4,1X),7(F7.3,1X))')'COL,ROW,CALCULATED_SNOCOV,ALBEDO,DIR_ALBEDO = ',
!    &              COL,ROW,
!    &              CALCULATED_SNOCOV(COL,ROW),ALBEDO_LAND,WEIGHT_SUM
!                 END IF

                     DO NW = 1, N_WATER_CLASSES
                        IF ( WATER_ANNUAL( NW,IWAVE,COL,ROW ) .LT. 1.0E-6 ) CYCLE
                        ALBEDO_WATER = ALBEDO_WATER 
     &                               + ZFACTOR_WATER( NW ) * WATER_ANNUAL(NW,IWAVE,COL,ROW )
                     END DO

! Note, seaice and snow have the same zenith angle correction
                     ALBEDO_SEAICE = ZFACTOR_SEAICE * ALBEDO_SEAICE
                  

C....sum for net direct albedo

                     SURFACE_ALBEDO( IWAVE,COL,ROW ) = ALBEDO_LAND 
     &                                               + ALBEDO_WATER 
     &                                               + ALBEDO_SEAICE

                     SURFACE_ALBEDO( IWAVE,COL,ROW ) = MIN( MSCALE, SURFACE_ALBEDO( IWAVE,COL,ROW ) )

                  END DO   ! iwave

               END DO   ! col
            END DO   ! row

            PRDATE = MDATE
            PRTIME = MTIME

            RETURN

         END SUBROUTINE GET_ALBEDO

      END MODULE PHOTOLYSIS_ALBEDO
