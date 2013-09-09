       MODULE PHOTOLYSIS_ALBEDO
 
C-----------------------------------------------------------------------
C  FUNCTION: Module contains a function and subroutine use to calculate the diffuse and
C  direct spectral albedo based on fraction land use for each grid cell
C
C  History:
C  06/04/13 Bill Hutzell - Initial based on the albedo algorithm from phot.F         
C                          from CMAQ 5.01
C-----------------------------------------------------------------------

         IMPLICIT NONE 
         
          REAL, PUBLIC, ALLOCATABLE, SAVE :: SURFACE_ALBEDO ( :,:,: ) ! time dependent surface albedo
          REAL, PUBLIC, ALLOCATABLE, SAVE :: DIFFUSE_ALBEDO ( :,:,: ) ! time dependent surface albedo

          INTEGER, PUBLIC, SAVE :: STDATE ! starting GMT [YYYYDDD]
          INTEGER, PUBLIC, SAVE :: STTIME ! starting time [HHMMSS]
          INTEGER, PUBLIC, SAVE :: STRTHR ! starting GMT hour [HH]
          REAL,    PUBLIC, SAVE :: JYFREQ ! 2PI/(# days in JYEAR)


          LOGICAL, PUBLIC, SAVE :: HAS_SEAICE = .FALSE. ! does metcro2d file contain sea ice data


          PUBLIC INITIALIZE_ALBEDO, GET_ALBEDO
          
          PRIVATE

          REAL, PARAMETER :: SEAICE_POINT = 271.36 ! 230.0 ! 271.36   ! threshold to form sea ice [K]
                                                 ! based a 2005 WRF model Documentation

          CHARACTER( 80 )         :: LAND_SCHEME
          INTEGER                 :: NUMB_LANDUSE

          REAL, ALLOCATABLE       :: FRACTION_LANDUSE( :,:,: ) ! fractional cover for a landuse
          REAL, ALLOCATABLE       :: BUFFER  ( :,: )           ! temp array for reading inputs
          REAL, ALLOCATABLE       :: LANDMASK( :,: )           ! land-water mask: 1 for land and 0 for water
          INTEGER, ALLOCATABLE    :: ALBMAP_TO_REF( : )        ! map from reference to used landuse for albedo
          REAL, ALLOCATABLE       :: ALBFAC_TO_REF( : )        ! factor from reference to used landuse for albedo

          REAL, ALLOCATABLE, SAVE :: WATER_FRACTION ( :,: )   ! cell fraction covered by water or ocean
          REAL, ALLOCATABLE, SAVE :: LAND_ANNUAL    ( :,:,: ) ! annual average of land albedo weighted land fraction
          REAL, ALLOCATABLE, SAVE :: WATER_ANNUAL   ( :,:,: ) ! annual average for water albedo weighted water fraction
          REAL, ALLOCATABLE, SAVE :: SEAICE_ANNUAL  ( : )     ! annual average for sea ice albedo weighted water fraction
          
          REAL, ALLOCATABLE, SAVE :: WATER_SEASONAL ( :,: )   ! seasonal coefficient for water albedo
          REAL, ALLOCATABLE, SAVE :: WATER_ZENITH   ( :,: )   ! solar zenith coefficient for water albedo
          REAL, ALLOCATABLE, SAVE :: WATER_SNOW     ( :,: )   ! snow cover coefficient for water albedo

          REAL, ALLOCATABLE, SAVE :: LAND_SEASONAL  ( :,: )   ! seasonal coefficient for land albedo
          REAL, ALLOCATABLE, SAVE :: LAND_ZENITH    ( :,: )   ! solar zenith coefficient for land albedo
          REAL, ALLOCATABLE, SAVE :: LAND_SNOW      ( :,: )   ! snow cover coefficient for land albedo

          REAL                    :: SEAICE_SEASONAL          ! seasonal coefficient for sea ice albedo
          REAL                    :: SEAICE_ZENITH            ! solar zenith coefficient for sea ice albedo
          REAL                    :: SEAICE_SNOW              ! snow cover coefficient for sea ice albedo
         
          REAL, ALLOCATABLE, SAVE :: SEAICE ( :,: )       ! sea ice cover (1=yes, 0=no)
          REAL, ALLOCATABLE, SAVE :: TEMPG  ( :,: )       ! ground surface temperature [K]
          REAL, ALLOCATABLE, SAVE :: SNOCOV ( :, : )      ! snow cover (fractional)
   
          REAL WATER_SCALE     ! water scaling factor used to calculate surface albedo
          REAL SEASONAL_COEFF  ! coefficient for seasonal correction to surface albedo
          REAL ZENITH_COEFF    ! coefficient for zenith angle correction to surface albedo
          REAL SNOW_COEFF      ! coefficient for snow cover correction to surface albedo
          REAL SEA_MODULATE    ! seasonal modulation in surface albedo
          REAL ZEN_MODULATE    ! zenith angle modulation in surface albedo
  
    
          INTEGER, SAVE :: GXOFF, GYOFF        ! global origin offset from file
C for INTERPX
          INTEGER, SAVE :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2
          INTEGER, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2
          INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

         
       CONTAINS
          
          FUNCTION INITIALIZE_ALBEDO( MDATE, MTIME, LOGDEV ) RESULT ( SUCCESS )

C...         Function sets up arrays and data needed to calculate surface albedos
C            use in radiative transfer calculation for actinic fluxes
  
             USE UTILIO_DEFN       ! IOAPI declaratiion and utilities
             USE SUBST_MODULES     ! stenex
             USE PHOT_MOD          ! photolysis in-line module
             USE PCGRID_DEFN       ! get cgrid

             USE LSM_MOD, ONLY: LSM_SCHEME => LAND_SCHEME
             
             IMPLICIT NONE


             INCLUDE SUBST_FILES_ID   ! file name parameters

C...Arguments:

             INTEGER, INTENT( IN ) :: MDATE     ! Julian date (YYYYDDD)
             INTEGER, INTENT( IN ) :: MTIME     ! time        (HHMMSS)
             INTEGER, INTENT( IN ) :: LOGDEV    ! messaging IO unit number  

C...Local:
             REAL                       :: JYEAR      ! year, ADE
             REAL                       :: MSCALE     ! scaling factor 
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
             INTEGER                    :: V, N, MODE
             INTEGER                    :: ALLOCSTAT


C...compute start time data and frequency of annual cycle considering leap year

             STDATE = MDATE
             STTIME = MTIME
             STRTHR = FLOAT( MTIME / 10000 )
             JYEAR  = FLOAT( MDATE / 1000 )   !   Check this more carefully

             IF ( MOD( ( 0.001 * MDATE ), 4.0 ) .EQ. 0 ) THEN
                JYFREQ = 2.0 * PI / 366.0
             ELSE
                JYFREQ = 2.0 * PI / 365.0
             END IF
             
             HAS_SEAICE  = .TRUE.
             VARNM = 'SEAICE'
             V = INDEX1( VARNM, NVARS3D, VNAME3D )

C...Get met file offsets

             CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                       STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )
             CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                       STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )
             CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                       STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )
             
             IF ( .NOT. DESC3( MET_CRO_2D ) ) THEN
                  XMSG = 'COULD NOT GET MET_CRO_2D  FILE DESCRIPTION '
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  SUCCESS = .FALSE.
                  RETURN
             END IF
             
             IF ( V .EQ. 0 ) THEN
                HAS_SEAICE = .FALSE.
                XMSG = 'Could not find ' // VARNM // 'in ' // MET_CRO_2D
                 WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
             END IF
             
#ifdef twoway
             LAND_SCHEME = LSM_SCHEME
#else             
             LAND_SCHEME = 'UNKNOWN'
             IF ( .NOT. DESC3( GRID_CRO_2D ) ) THEN
                 XMSG = 'COULD NOT GET GRID_CRO_2D FILE DESCRIPTION '
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  SUCCESS = .FALSE.
                  RETURN
             END IF
             
             DO V = 1, NVARS3D ! determine land character

                IF ( VNAME3D( V ) .EQ. 'DLUSE' ) THEN
                  IF ( INDEX( VDESC3D( V ), 'USGS24' ) .NE. 0 ) LAND_SCHEME = 'USGS24'
                  IF ( INDEX( VDESC3D( V ), ' MODIS' ) .NE. 0 ) LAND_SCHEME = 'MODIS'

C...overwrite if NLCD-MODIS scheme

                  IF ( INDEX( VDESC3D( V ), 'NLCD-MODIS' ) .NE. 0 ) LAND_SCHEME = 'NLCD50'
                  IF ( INDEX( VDESC3D( V ),     'NLCD50' ) .NE. 0 ) LAND_SCHEME = 'NLCD50'
                  IF ( INDEX( VDESC3D( V ),     'NLCD40' ) .NE. 0 ) LAND_SCHEME = 'NLCD40'
              END IF

            END DO
#endif
         
            SELECT CASE( LAND_SCHEME )
              CASE( 'USGS24' )
                 NUMB_LANDUSE = NUMB_LANDUSE_USGS  ! 24
              CASE( 'MODIS' )
                 NUMB_LANDUSE = NUMB_LANDUSE_MODIS ! 33
              CASE( 'NLCD50' )
                 NUMB_LANDUSE = NUMB_LANDUSE_NLCD50  ! 50
              CASE( 'NLCD40' )
                 NUMB_LANDUSE = NUMB_LANDUSE_NLCD40  ! 40
                 IF( NO_NLCD40 )THEN
                     XMSG =  'GRID_CRO_2D uses NLCD40 landuse scheme but '
     &                    // 'CSQY_FILE does not albedo factors for the '
     &                    // 'NLCD40'
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  SUCCESS = .FALSE.
                  RETURN
                 END IF
              CASE DEFAULT
                 NUMB_LANDUSE = 2 ! simple land-water suface albedo
            END SELECT
         
            ALLOCATE ( ALBMAP_TO_REF( NUMB_LANDUSE ), ALBFAC_TO_REF( NUMB_LANDUSE ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ALBMAP_TO_REF and ALBFAC_TO_REF'
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  SUCCESS = .FALSE.
                  RETURN
            END IF
         
            ALLOCATE ( FRACTION_LANDUSE( NUMB_LANDUSE, MY_NCOLS, MY_NROWS ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating FRACTION_LANDUSE'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF
         
            ALLOCATE ( BUFFER( MY_NCOLS, MY_NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating BUFFER'
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  SUCCESS = .FALSE.
                  RETURN
            END IF
         
            FRACTION_LANDUSE  = 0.0
         
            IF ( LAND_SCHEME .NE. 'UNKNOWN' ) THEN
               XMSG = ': Identified ' // TRIM( LAND_SCHEME )
     &              // ' land use scheme for surface albedo used by inline'
     &              // ' photolysis calculation.'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               
               DO V = 1, NUMB_LANDUSE
                  WRITE(LU_INDEX, '(I2.2)') V
                  VARNM = 'LUFRAC_' // LU_INDEX
                  IF ( .NOT. INTERPX( GRID_CRO_2D, VARNM, PNAME,
     &                                STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2,
     &                                1,1, 0000000, 000000, BUFFER ) ) THEN
                     XMSG =  'Could not read ' // TRIM( VARNM )
     &                    // ' from ' // GRID_CRO_2D
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  SUCCESS = .FALSE.
                  RETURN
               END IF
               DO COL = 1, MY_NCOLS
                  DO ROW = 1, MY_NROWS
                     FRACTION_LANDUSE( V, COL, ROW ) = BUFFER( COL, ROW )
                  END DO
               END DO
               END DO
            ELSE
               XMSG = ': Undentified ' // TRIM( LAND_SCHEME )
     &              // ' land use scheme for inline photolysis calculation.'
     &              // ' Using default land-water albedo for inline photolysis'
     &              // ' calculation.'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               VARNM = 'LWMASK'
               IF ( .NOT. INTERPX( GRID_CRO_2D, VARNM, PNAME,
     &                             STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2,
     &                             1,1, 0000000, 000000, BUFFER ) ) THEN
                  XMSG  = 'Could not read ' // TRIM( VARNM )
     &                  // ' from ' // GRID_CRO_2D
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
               END IF
               DO COL = 1, MY_NCOLS
                  DO ROW = 1, MY_NROWS
                     IF ( BUFFER( COL, ROW ) .LT. 0.5 ) THEN
                        FRACTION_LANDUSE( 2, COL, ROW ) = 1.0
                     ELSE
                        FRACTION_LANDUSE( 1, COL, ROW ) = 1.0
                     END IF
                  END DO
               END DO
            END IF
           
           
            ALLOCATE ( SNOCOV( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating SNOCOV array'
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  SUCCESS = .FALSE.
                  RETURN
            END IF
           
            ALLOCATE ( SEAICE( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating SEAICE array'
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  SUCCESS = .FALSE.
                  RETURN
            END IF

            IF ( .NOT. HAS_SEAICE ) THEN
          
               XMSG = 'MET_CRO_2D DOES NOT CONTAIN SEA ICE DATA. THE SURFACE ALBEDO '
     &              // 'DOES NOT INCLUDE ITS EFFECTS. Setting to one if water surface '
     &              // 'temperaure is less 271.36K (WRF formation threshold).'
               WRITE( LOGDEV, '(A)' ) XMSG
               ALLOCATE ( TEMPG( MY_NCOLS,MY_NROWS  ), STAT = ALLOCSTAT )
               IF ( ALLOCSTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating TEMPG array'
                  WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
                  SUCCESS = .FALSE.
                  RETURN
               END IF
          
            END IF
          
            ALLOCATE ( SURFACE_ALBEDO( NWL, MY_NCOLS, MY_NROWS ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating SURFACE_ALBEDO'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF

            ALLOCATE ( DIFFUSE_ALBEDO( NWL, MY_NCOLS, MY_NROWS ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating DIFFUSE_ALBEDO'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF
          
          
            ALLOCATE ( LAND_ANNUAL ( NWL, MY_NCOLS, MY_NROWS ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating LAND_ANNUAL'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF
         
            ALLOCATE ( LAND_SEASONAL( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating LAND_SEASONAL'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF
         
            ALLOCATE ( LAND_ZENITH( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ALBEDO_ZENITH'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF
          
            ALLOCATE ( LAND_SNOW( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ALBEDO_SNOW'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF
         
            ALLOCATE ( WATER_ANNUAL( NWL, MY_NCOLS, MY_NROWS ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating WATER_ANNUAL'
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
         
            ALLOCATE ( WATER_FRACTION( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating WATER_FRACTION'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF
         
            ALLOCATE ( WATER_SEASONAL( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating WATER_SEASONAL'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF
          
            ALLOCATE ( WATER_ZENITH( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating WATER_ZENITH'
               WRITE(LOGDEV,'( A )')TRIM( PNAME ) // ' : ' // XMSG
               SUCCESS = .FALSE.
               RETURN
            END IF
           
            ALLOCATE ( WATER_SNOW( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating WATER_SNOW'
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
            ALBMAP_TO_REF   = -1
            ALBFAC_TO_REF   = 0.0
          
            SELECT CASE( LAND_SCHEME )
               CASE( 'USGS24' )
                  ALBMAP_TO_REF( 1:NUMB_LANDUSE ) = ALBMAP_REF2USGS( 1:NUMB_LANDUSE )
                  ALBFAC_TO_REF( 1:NUMB_LANDUSE ) = ALBFAC_REF2USGS( 1:NUMB_LANDUSE )
               CASE( 'MODIS' )
                  ALBMAP_TO_REF( 1:NUMB_LANDUSE ) = ALBMAP_REF2MODIS( 1:NUMB_LANDUSE )
                  ALBFAC_TO_REF( 1:NUMB_LANDUSE ) = ALBFAC_REF2MODIS( 1:NUMB_LANDUSE )
               CASE( 'NLCD50' )
                  ALBMAP_TO_REF( 1:NUMB_LANDUSE ) = ALBMAP_REF2NLCD50( 1:NUMB_LANDUSE )
                  ALBFAC_TO_REF( 1:NUMB_LANDUSE ) = ALBFAC_REF2NLCD50( 1:NUMB_LANDUSE )
               CASE( 'NLCD40' )
                  ALBMAP_TO_REF( 1:NUMB_LANDUSE ) = ALBMAP_REF2NLCD40( 1:NUMB_LANDUSE )
                  ALBFAC_TO_REF( 1:NUMB_LANDUSE ) = ALBFAC_REF2NLCD40( 1:NUMB_LANDUSE )
               CASE DEFAULT
                  ALBMAP_TO_REF( 1 ) = INDEX_GRASSLAND_REF
                  ALBFAC_TO_REF( 1 ) = 1.0
                  ALBMAP_TO_REF( 2 ) = INDEX_OCEAN_REF
                  ALBFAC_TO_REF( 2 ) = 1.0
            END SELECT
            
C...dete   rmine average albedo and its adjustment factors for zenith angle, season and snow cover
           
            WATER_FRACTION = 0.0

            WATER_SEASONAL = 1.0
            WATER_ZENITH   = 0.0
            WATER_SNOW     = 1.0
            WATER_ANNUAL   = 0.0

            LAND_SEASONAL  = 1.0
            LAND_ZENITH    = 0.0
            LAND_SNOW      = 1.0
            WATER_ANNUAL   = 0.0
           
            DO COL = 1, MY_NCOLS
               DO ROW = 1, MY_NROWS
                  DO V = 1, NUMB_LANDUSE
                     L = ALBMAP_TO_REF( V )
                     IF ( L .LE. 0 ) CYCLE
                     IF ( L .EQ. INDEX_OCEAN_REF ) THEN
                        WATER_FRACTION( COL, ROW ) = WATER_FRACTION( COL, ROW )
     &                                             + FRACTION_LANDUSE( V, COL, ROW )
                        WATER_SCALE                = ALBFAC_TO_REF( V )
     &                                             * FRACTION_LANDUSE( V, COL, ROW )

                        WATER_SEASONAL( COL, ROW ) = WATER_SEASONAL( COL, ROW )
     &                                             + WATER_SCALE * SEASON_COEFF_REF( L )
                        WATER_ZENITH  ( COL, ROW ) = WATER_ZENITH( COL, ROW )
     &                                             + WATER_SCALE * ZENITH_COEFF_REF( L )
                        WATER_SNOW    ( COL, ROW ) = WATER_SNOW( COL, ROW )
     &                                             + WATER_SCALE * SNOW_COEFF_REF( L )
          
                        DO IWL = 1, NWL
                           WATER_ANNUAL( IWL, COL, ROW ) = WATER_ANNUAL( IWL, COL, ROW )
     &                                                   + WATER_SCALE * SPECTRAL_ALBEDO_REF( IWL, L )
          
                        END DO
          
                     ELSE
                        MSCALE = FRACTION_LANDUSE( V, COL, ROW )
                        LAND_SEASONAL( COL, ROW )   = LAND_SEASONAL( COL, ROW )
     &                                              + MSCALE * SEASON_COEFF_REF( L )
                        LAND_ZENITH( COL, ROW )     = LAND_ZENITH( COL, ROW )
     &                                              + MSCALE * ZENITH_COEFF_REF( L )
                        LAND_SNOW( COL, ROW )       = LAND_SNOW( COL, ROW )
     &                                              + MSCALE * SNOW_COEFF_REF( L )
                        MSCALE = MSCALE * ALBFAC_TO_REF( V )
                        DO IWL = 1, NWL
                           LAND_ANNUAL( IWL, COL, ROW ) = LAND_ANNUAL( IWL, COL, ROW )
     &                                                  + MSCALE * SPECTRAL_ALBEDO_REF( IWL, L )
                        END DO
                     END IF
                  END DO

C...Normalize land and water correction factors
                  
                  IF ( WATER_FRACTION( COL, ROW ) .GT. 1.0E-30 )THEN
                  
                       MSCALE = 1.0 / WATER_FRACTION( COL, ROW )
                       
                       WATER_SEASONAL( COL, ROW ) = MSCALE * WATER_SEASONAL( COL, ROW )
                       WATER_ZENITH  ( COL, ROW ) = MSCALE * WATER_ZENITH  ( COL, ROW )
                       WATER_SNOW    ( COL, ROW ) = MSCALE * WATER_SNOW    ( COL, ROW )
                  ELSE
                       WATER_SEASONAL( COL, ROW )   = 1.0
                       WATER_ZENITH  ( COL, ROW )   = 0.0
                       WATER_SNOW    ( COL, ROW )   = 1.0
                       WATER_ANNUAL( : , COL, ROW ) = 0.0
                  END IF

                  IF ( ( 1.0 - WATER_FRACTION( COL, ROW ) ) .GT. 1.0E-30 )THEN
                  
                       MSCALE = 1.0 / MAX( 1.0 - WATER_FRACTION( COL, ROW ), 1.0E-30 )
                       
                       LAND_SEASONAL( COL, ROW ) = MSCALE * LAND_SEASONAL( COL, ROW )
                       LAND_ZENITH  ( COL, ROW ) = MSCALE * LAND_ZENITH  ( COL, ROW )
                       LAND_SNOW    ( COL, ROW ) = MSCALE * LAND_SNOW    ( COL, ROW )
                  ELSE
                       LAND_SEASONAL( COL, ROW )   = 1.0
                       LAND_ZENITH  ( COL, ROW )   = 0.0
                       LAND_SNOW    ( COL, ROW )   = 1.0
                       LAND_ANNUAL( : , COL, ROW ) = 0.0
                  END IF
                       

               END DO
            END DO
         
         
            SUCCESS = .TRUE.
            
            RETURN
            
          END FUNCTION INITIALIZE_ALBEDO
C            
          SUBROUTINE GET_ALBEDO(MDATE, MTIME, LOGDEV, COSZENS, LAT, LON)
            
C... Subroutine calculates diffuse and direct surface albedo versus wavelenght over a set of
C    latitude and longitudes

             USE UTILIO_DEFN       ! IOAPI declaratiion and utilities
             USE SUBST_MODULES     ! stenex
             USE PHOT_MOD          ! photolysis in-line module
             USE PCGRID_DEFN       ! get cgrid
             
             IMPLICIT NONE

             INCLUDE SUBST_FILES_ID   ! file name parameters

C arguments:

             INTEGER, INTENT( IN ) ::  MDATE            ! Julian date (YYYYDDD)
             INTEGER, INTENT( IN ) ::  MTIME            ! time        (HHMMSS)
             INTEGER, INTENT( IN ) ::  LOGDEV           ! messaging IO unit number  
             REAL,    INTENT( IN ) ::  COSZENS( :,: )   ! cosine of the solar zenith angle
             REAL,    INTENT( IN ) ::  LAT( :,: )       ! north lat at cell center [deg]
             REAL,    INTENT( IN ) ::  LON( :,: )       ! west long at cell center [deg] 

C local:
             REAL                        :: CURRENT_HOUR  ! current GMT hour [sec]
             REAL                        :: JULIAN_DAY    ! julian day       [days]
             REAL                        :: CURRHR_LST    ! local standard time at each grid cell
             REAL                        :: EQUATION_TIME ! equation of time
             REAL                        :: COSZEN        ! working cosine of the solar zenith angle
             REAL                        :: SINLAT        ! sine of latitude
             REAL                        :: COSLAT        ! cosine of latitude
             REAL                        :: MSCALE        ! scaling factor 

             REAL                        :: ALBEDO_LAND   ! scratch variable for land fraction
             REAL                        :: ALBEDO_WATER  ! scratch variable for water fraction
             REAL                        :: ALBEDO_SEAICE ! scratch variable for seaice fraction

             REAL                        :: SFACTOR_LAND   ! seasonal correction for land albedo
             REAL                        :: SFACTOR_WATER  ! seasonal correction for water albedo
             REAL                        :: SFACTOR_SEAICE ! seasonal correction for seaice albedo
 
             REAL                        :: ZFACTOR_LAND   ! solar zenith angle correction for land albedo
             REAL                        :: ZFACTOR_WATER  ! solar zenith angle correction for water albedo
             REAL                        :: ZFACTOR_SEAICE ! solar zenith angle correction for seaice albedo
             
             REAL                        :: SNOW_FREE      ! snow free fraction of cell
             REAL                        :: ICE_FREE       ! water fraction free sea ice 
            
             CHARACTER(  17 ), PARAMETER :: PNAME  = 'GET_ALBEDO'
             CHARACTER(  16 )            :: VARNM
             CHARACTER( 240 )            :: XMSG   = ' '

             INTEGER                     :: ROW
             INTEGER                     :: COL
             INTEGER                     :: LEV
             INTEGER                     :: SPC
             INTEGER                     :: L
             INTEGER                     :: V, N, MODE
             INTEGER                     :: ALLOCSTAT

C...Read & Interpolate SNOCOV

             VARNM = 'SNOCOV'
             XMSG = 'Could not read SNOCOV from ' // MET_CRO_2D
             IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2,
     &                    1, 1, MDATE, MTIME, SNOCOV ) ) THEN
                CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
             END IF

             IF ( HAS_SEAICE ) THEN

                VARNM = 'SEAICE'
                XMSG = 'Could not read SEAICE from ' // MET_CRO_2D
                IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2,
     &                       1, 1, MDATE, MTIME, SEAICE ) ) THEN
                   CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
                END IF

             ELSE

                VARNM = 'TEMPG'
                XMSG = 'Could not read TEMPG from ' // MET_CRO_2D
                IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2,
     &                       1, 1, MDATE, MTIME, TEMPG ) ) THEN
                   CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
                END IF

             END IF
             
C...Calculate current hour in GMT and julian day

             CURRENT_HOUR = STRTHR
     &                    + FLOAT( SECSDIFF( STDATE, STTIME, MDATE, MTIME ) )
     &                    / 3600.0

             JULIAN_DAY   = FLOAT( MOD( MDATE, 1000 ) )


C...Calculate cosines of the zenith angles

             DO ROW = 1, MY_NROWS
                DO COL = 1, MY_NCOLS

                   SINLAT = SIN( PI180 * LAT ( COL, ROW ) )
                   COSLAT = COS( PI180 * LAT ( COL, ROW ) )

C...correct  CURRHR for current *positive* West longitude convention
C...  to obtain LST.

C...this convention on longititude should be reexamined for different domains

                   CURRHR_LST = CURRENT_HOUR + LON( COL, ROW ) / 15.0
              
                   IF ( .NOT. HAS_SEAICE ) THEN ! determine sea ice can form
                      IF ( TEMPG( COL, ROW ) .LT. SEAICE_POINT .AND.
     &                     WATER_FRACTION( COL, ROW ) .GE. 0.95 ) THEN
                         SEAICE( COL, ROW ) = 1.0
                      ELSE
                         SEAICE( COL, ROW ) = 0.0
                      END IF
                   END IF

C...add water or sea ice to annual albedo and its correction factors

                   MSCALE = WATER_FRACTION( COL, ROW )
                   IF ( MSCALE .GT. 0 ) THEN

                      L = INDEX_SEA_ICE
                      SEAICE_SEASONAL = SEASON_COEFF_REF( L )
                      SEAICE_ZENITH   = ZENITH_COEFF_REF( L )
                      SEAICE_SNOW     = SNOW_COEFF_REF( L )
                      DO IWL = 1, NWL
                         SEAICE_ANNUAL( IWL ) = MSCALE
     &                                        * SPECTRAL_ALBEDO_REF( IWL, L )
                      END DO
                   ELSE
                      SEAICE_SEASONAL = 1.0
                      SEAICE_ZENITH   = 0.0
                      SEAICE_SNOW     = 1.0
                      SEAICE_ANNUAL   = 0.0
                   END IF

C...determine seasonal and snow corrections to surface albedo
C...  convert julian into time of year for grid cell
C...  seasonal adjustment has an 11 day phase delay in the solar cycle

                   IF ( LAT( COL, ROW ) .GE. 0.0 ) THEN
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

                   SFACTOR_LAND   = 1.0 /( 1.0 + MSCALE * (LAND_SEASONAL (COL, ROW)-1.0) )
                   SFACTOR_WATER  = 1.0 /( 1.0 + MSCALE * (WATER_SEASONAL(COL, ROW)-1.0) )
                   SFACTOR_SEAICE = 1.0 /( 1.0 + MSCALE * (SEAICE_SEASONAL-1.0) )


C..Determine zenith angle correction to albedos
C...First, test whether zenith angle is greater than 90 degrees.
                   IF ( COSZENS( COL, ROW ) .LE. 0.0 ) THEN
                      ZFACTOR_LAND   = ( 1.0 + LAND_ZENITH ( COL, ROW ) )
                      ZFACTOR_WATER  = ( 1.0 + WATER_ZENITH( COL, ROW ) )
                      ZFACTOR_SEAICE = ( 1.0 + SEAICE_ZENITH )

                   ELSE
                      ZFACTOR_LAND   = ( 1.0 + LAND_ZENITH( COL, ROW ) )
     &                               / ( 1.0 + 2.0 * COSZENS( COL, ROW ) * LAND_ZENITH( COL, ROW ) )
                      ZFACTOR_WATER  = ( 1.0 + WATER_ZENITH( COL, ROW ) )
     &                               / ( 1.0 + 2.0 * COSZENS( COL, ROW ) * WATER_ZENITH( COL, ROW ) )
                      ZFACTOR_SEAICE = ( 1.0 + SEAICE_ZENITH )
     &                               / ( 1.0 + 2.0 * COSZENS( COL, ROW ) * SEAICE_ZENITH )
                   END IF

                   ZFACTOR_LAND   = MAX( 0.8, ZFACTOR_LAND )
                   ZFACTOR_WATER  = MAX( 0.8, ZFACTOR_WATER )
                   ZFACTOR_SEAICE = MAX( 0.8, ZFACTOR_SEAICE )
                   
                   SNOW_FREE = MAX( ( 1.0 - SNOCOV( COL, ROW ) ), 0.0 )
                   ICE_FREE  = MAX( ( 1.0 - SEAICE( COL, ROW ) ), 0.0 )

                   DO IWL = 1, NWL
                   
C...compute seasonal diffuse albedos for land, water and seaice separately
                   
                      ALBEDO_LAND   = SFACTOR_LAND * LAND_ANNUAL( IWL, COL, ROW )
     &                              * ( SNOCOV( COL, ROW ) * LAND_SNOW( COL, ROW ) + SNOW_FREE )
  
                      ALBEDO_WATER  = SFACTOR_WATER * WATER_ANNUAL( IWL, COL, ROW )
     &                              * ( SNOCOV( COL, ROW ) * WATER_SNOW( COL, ROW ) + SNOW_FREE )

                      ALBEDO_SEAICE = SFACTOR_SEAICE * SEAICE_ANNUAL( IWL )
     &                              * ( SNOCOV( COL, ROW ) * SEAICE_SNOW + SNOW_FREE )

C...sum for net diffuse albedo

                      DIFFUSE_ALBEDO( IWL, COL, ROW ) = ALBEDO_LAND 
     &                                                + ICE_FREE * ALBEDO_WATER 
     &                                                + SEAICE( COL, ROW ) * ALBEDO_SEAICE
                      
                      DIFFUSE_ALBEDO( IWL, COL, ROW ) = MIN( 1.0, DIFFUSE_ALBEDO( IWL, COL, ROW ) )
                      
C...Update scratch albedos for correction from solar zenith angle

                      ALBEDO_LAND   = ZFACTOR_LAND * ALBEDO_LAND
                      ALBEDO_WATER  = ZFACTOR_WATER * ALBEDO_WATER
                      ALBEDO_SEAICE = ZFACTOR_SEAICE * ALBEDO_SEAICE

C....sum for net direct albedo

                      SURFACE_ALBEDO( IWL, COL, ROW ) = ALBEDO_LAND 
     &                                                + ICE_FREE * ALBEDO_WATER 
     &                                                + SEAICE( COL, ROW ) * ALBEDO_SEAICE

                      SURFACE_ALBEDO( IWL, COL, ROW ) = MIN( 1.0, SURFACE_ALBEDO( IWL, COL, ROW ) )
                      
                                           
                   END DO


#ifdef verbose_phot
            iwl  = 1
            if(col .eq. 1 .and. row .eq. 1 )then
            
                  MSCALE = MAX( (1.0-WATER_FRACTION( COL, ROW ), 0.0)
                  
                  SEA_MODULATE = MSCALE * SFACTOR_LAND 
     &                         + WATER_FRACTION( COL, ROW )
     &                         * (ICE_FREE*SFACTOR_WATER + SEAICE( COL, ROW )*SFACTOR_SEAICE)

                  ZEN_MODULATE = MSCALE * ZFACTOR_LAND 
     &                         + WATER_FRACTION( COL, ROW )
     &                         * (ICE_FREE*ZFACTOR_WATER + SEAICE( COL, ROW )*ZFACTOR_SEAICE)

                  write(logdev, 5005)iwl,  COSZEN,ZEN_MODULATE, SEA_MODULATE,
     &            DIFFUSE_ALBEDO( IWL, COL, ROW ), SURFACE_ALBEDO( IWL, COL, ROW )

5005              format(' wlv index = ',i3,' COSZEN, Mean ZEN_MODULATE, Mean SEA_MODULATE = ',
     &            3(es12.4,1x), 
     &            'DIFFUSE_ALBEDO = ', es12.4,1x,'SURFACE_ALBEDO = ',es12.4,1x)
            end if
#endif

                END DO
             END DO
          
             RETURN
          
          END SUBROUTINE GET_ALBEDO
         
       END MODULE PHOTOLYSIS_ALBEDO        
         
                     
