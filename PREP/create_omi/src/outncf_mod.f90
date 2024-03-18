MODULE OUTNCF_FILE_ROUTINES

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! The definitions and declarations in this include file follow those
! given in the I/O-API include file FDESC3C.EXT and replace similar
! definitions.  This include file is compatible with FDESC3C.EXT.
!
! All variable names end in string "_GD", which is a grid identifier for
! multiple grid applications. "_GD" should be "_G1" for the first grid,
! "_G2" for the second grid, etc.
!
! The horizontal grid definition information is REAL*8 in order 
! to achieve the required precision in geographic-to/from-grid
! coordinate conversions.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! FORTRAN units and file names.
  INTEGER                           :: cdfid_CMAQ
  INTEGER                           :: cdfid_FULL
  
!-------------------------------------------------------------------------------
! GDTYP_GD:
! The map projection type:
!   1: LATGRD for lat-lon coordinates (unused)
!   2: LAMGRD for Lambert coordinates
!   3: MERGRD for Mercator coordinates
!   4: STEGRD for Stereographic coordinates
!   5: UTMGRD for UTM coordinates
!-------------------------------------------------------------------------------

  INTEGER                      :: gdtyp_gd = 1

!-------------------------------------------------------------------------------
! The definitions of the map projection specification parameters:
!   P_ALP_GD  (PROJ_ALPHA),
!   P_BET_GD  (PROJ_BETA),
!   P_GAM_GD  (PROJ_GAMMA),
! depend upon the projection type, as follows:
! (Note: if P_ALP_GD < AMISS (=-9E36, from PARMS3.EXT), then the grid
!  description is missing or invalid.)
! 
! Lambert        P_ALP_GD <= P_BET_GD are the two latitudes that
!                determine the projection cone; P_GAM_GD is the
!                central meridian.
! 
! Mercator       P_ALP_GD and P_BET_GD are the latitude and longitude
!                of the coordinate origin (within the tangent circle);
!                P_GAM_GD is the angle between the cylinder axis
!                and the North polar axis.
! 
! Stereographic  P_ALP_GD and P_BET_GD are the latitude and longitude
!                of the point of tangency; P_GAM_GD is the angle from
!                true North to the Y-axis.
!    
! UTM:           P_ALP_GD is the UTM zone.
!                P_BET_GD and P_GAM_GD are unused.
!    
! lat-lon:       Currently not used.  Coordinate units are degrees, with
!                -180.0 < X <= 180.0,  -90.0 <= Y <= 90.0   
!                Western longitudes and southern latitudes are negative.
!-------------------------------------------------------------------------------

  REAL(8)                      :: p_alp_gd  = 0.0  ! degrees
  REAL(8)                      :: p_bet_gd  = 0.0 ! degrees
  REAL(8)                      :: p_gam_gd  = 0.0 ! degrees

!-------------------------------------------------------------------------------
! (XCENT_GD, YCENT_GD):
! For Lambert, Mercator, and Stereographic, these are the 
!     longitude, -180 < X <= 180, and the latitude, -90 <= Y <= 90, 
!     for the center of the grid's respective Cartesian coordinate system.
! For UTM:  ?
! For Lat-Lon:  unused
!-------------------------------------------------------------------------------
     
  REAL(8)                      :: xcent_gd = 0.0  ! degrees longitude
  REAL(8)                      :: ycent_gd = 0.0  ! degrees latitude

!-------------------------------------------------------------------------------
! (XORIG_GD, YORIG_GD):
! For Lambert, Mercator, Stereographic, and UTM these are the
!     location in map units (Km) of the origin cell (1,1) (lower left corner)
!     of the of the horizontal grid measured from (XCENT_GD, YCENT_GD).
! For Lat-Lon: units are degrees - unused
!-------------------------------------------------------------------------------
     
  REAL(8)                      :: xorig_gd = -90.0  ! X-origin [m]
  REAL(8)                      :: yorig_gd = -180.0  ! Y-origin [m]

!-------------------------------------------------------------------------------
! (XCELL_GD, YCELL_GD):
! The X-direction and Y-direction cell dimensions (m) for a regular grid
! If zero, the grid is assumed irregular and described by other means (e.g.
! a grid-geometry file).
!-------------------------------------------------------------------------------
     
  REAL(8)                      :: xcell_gd = 1.0  ! X-cell dimension [m]
  REAL(8)                      :: ycell_gd = 1.0  ! Y-cell dimension [m]

!-------------------------------------------------------------------------------
! VGTYP_GD:
! The vertical grid type:
!   1: VGSIGP for sigma-P coordinates
!   2: VGSGP0 for sigma-P0 coordinates
!   3: VGSIGZ for sigma-Z coordinates
!   4: VGETAP for eta-P coordinates
!   5: VGPRES for pressure coordinates
!   6: VGZVAL for Z (meters above ground)
!   7: VHZVAL for H (meters above mean sea level)
!   8: IMISS  for vertical coordinates not stored in VGLVSD
!             (e.g., temporally or spatially changing vertical coordinates)
!-------------------------------------------------------------------------------
     
  INTEGER                      :: vgtyp_gd = 7

!-------------------------------------------------------------------------------
! VGTPUN_GD:
! The units of the vertical coordinate top.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=16)            :: vgtpun_gd

!-------------------------------------------------------------------------------
! VGTOP_GD:
! The value for the model top used in the definition of the sigma
! coordinate systems in the VGTPUN_GD units
! For sigma-P, the relationship between pressure levels P and sigma-P is
! given by the following formula:
!    sigma-P = ( P - VGTOP_GD ) / (P_srf - VGTOP_GD ),
! where P_srf is the surface pressure.
!-------------------------------------------------------------------------------

  REAL                         :: vgtop_gd = 5000.0

!-------------------------------------------------------------------------------
! VGLVUN_GD:
! The units of the vertical coordinate surface values
!-------------------------------------------------------------------------------

  CHARACTER(LEN=16)            :: vglvun_gd

!-------------------------------------------------------------------------------
! VGLVS_GD( 1...NLAYS+1 ):
! The list of vertical coordinate surface values in the VGLVUN_GD units
! Layer k extends from VGLVS3D( k ) to VGLVS3D( k+1 ).
!-------------------------------------------------------------------------------

  REAL :: vglvs_gd   ( 2 ) = (/ 0.0, 1.0 /)


!-------------------------------------------------------------------------------
! COORDNAM_GD:
! The coordinate system name used for I/O-API description and GRIDDESC.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=16)            :: coordnam_gd = 'OMI_GLOBE'

!-------------------------------------------------------------------------------
! GDNAME_GD:
! The grid name used for I/O-API description and GRIDDESC.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=16)            :: gdname_gd  = 'OMI_CMAQ'

!-------------------------------------------------------------------------------
! Dimensions of CTM domain.
!-------------------------------------------------------------------------------
  INTEGER            :: data_source = 1
  INTEGER            :: ncols = 1     ! number of grid columns (X direction)
  INTEGER            :: nrows = 1     ! number of grid rows (Y direction)
  INTEGER            :: nlays = 1     ! number of vertical layers
  INTEGER, PARAMETER :: nthik = 1     ! boundary thickness (cells)
  INTEGER            :: nbndy = 1     ! number of cells in one layer of boundary
  INTEGER            :: ncg_x = 0.5   ! coarse grid X
  INTEGER            :: ncg_y = 0.5   ! coarse grid Y

  REAL,    PARAMETER :: eradm = 6.37E+5 ! earth radius [m]

  INTEGER, PARAMETER :: maxlays = 100   ! max allowed in NLAYS

  INTEGER, PARAMETER :: ttol_sec = 300  ! time tolerance [in seconds] for output
                                        ! from the meteorological model to
                                        ! deviate from valid time and still be
                                        ! considered valid at that time

  INTEGER, PARAMETER :: MAX_LEN_FILENAME = 657 ! maximum length of filename

!-------------------------------------------------------------------------------
! Dimensions for netCDF output.
!-------------------------------------------------------------------------------

  INTEGER           :: nlucat
  INTEGER           :: nmos
  INTEGER           :: nperim
  INTEGER           :: nsoi
  INTEGER           :: nsoicat
  INTEGER           :: nx
  INTEGER           :: nxp1       ! nx + 1
  INTEGER           :: ny
  INTEGER           :: nyp1       ! ny + 1
  INTEGER           :: nz
  INTEGER           :: nzp1       ! nz + 1

!-------------------------------------------------------------------------------
! Program and version descriptors.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=16),  PARAMETER     :: progname   = 'CREATE_OMI'
  CHARACTER(LEN=10),  PARAMETER     :: vdate      = '07/12/2024'
  CHARACTER(LEN=8),   PARAMETER     :: ver        = 'V6.0'
  CHARACTER(LEN=24)                 :: omi_start  = '1969-07-16-00:00:00.0000'
  CHARACTER(LEN=24)                 :: omi_now    = '1969-07-16-00:00:00.0000'
  CHARACTER(LEN=24)                 :: cmaq_start = '1969-07-16-00:00:00.0000'
  
!-------------------------------------------------------------------------------
! Time-varying 2d fields at cell centers.
!-------------------------------------------------------------------------------

  INTEGER           :: nfld2dxyt_CMAQ 
  INTEGER           :: nfld2dxyt_FULL

  TYPE fld2ddata
    REAL, ALLOCATABLE :: fld( : , : )
    INTEGER           :: id_time = 0
    INTEGER           :: id_fld  = 0
    CHARACTER(LEN=16) :: fldname = ''
    CHARACTER(LEN=80) :: long_name = ''
    CHARACTER(LEN=16) :: units = ''
    CHARACTER(LEN=16) :: dimnames( 4 ) = ''
    INTEGER           :: istart( 4 ) = 1
    INTEGER           :: iend( 4 ) = 1
  END TYPE fld2ddata

  TYPE(fld2ddata), ALLOCATABLE :: fld2dxyt_CMAQ( : )
  TYPE(fld2ddata), ALLOCATABLE :: fld2dxyt_FULL( : )

  TYPE file_2dxyt
    CHARACTER( MAX_LEN_FILENAME )  :: filename = 'file_2dxyt.ncf'
    LOGICAL            :: CREATED = .FALSE.
    LOGICAL            :: INITIALIZED = .FALSE.
    INTEGER            :: cdfid_m = 0
    INTEGER            :: id_time = 0
    INTEGER            :: itstep  = 0
    INTEGER            :: gdtyp_gd = 1
    REAL(8)            :: p_alp_gd = 0.0  ! degrees
    REAL(8)            :: p_bet_gd = 0.0 ! degrees
    REAL(8)            :: p_gam_gd = 0.0 ! degrees
    REAL(8)            :: xcent_gd = 0.0  ! degrees longitude
    REAL(8)            :: ycent_gd = 0.0  ! degrees latitude
    REAL(8)            :: xorig_gd = -90.0  ! X-origin [deg]
    REAL(8)            :: yorig_gd = -180.0  ! Y-origin [deg]
    REAL(8)            :: xcell_gd = 1.0  ! X-cell dimension [deg]
    REAL(8)            :: ycell_gd = 1.0  ! Y-cell dimension [deg]
    INTEGER            :: vgtyp_gd = 7
    REAL               :: vgtop_gd = 5000.0
    CHARACTER(LEN=16)  :: vgtpun_gd  = ''
    CHARACTER(LEN=16)  :: vglvun_gd  = ''
    REAL               :: vglvs_gd   ( 2 ) = (/ 0.0, 1.0 /)
    CHARACTER(LEN=16)  :: coordnam_gd = 'OMI_GLOBE'
    CHARACTER(LEN=16)  :: gdname_gd  = 'OMI_CMAQ'
    INTEGER            :: ncols   = 0
    INTEGER            :: nrows   = 0
    INTEGER            :: nlays   = 1
    INTEGER            :: nthik   = 1     ! boundary thickness (cells)
    INTEGER            :: nbndy   = 1     ! number of cells in one layer of boundary
    CHARACTER(LEN=16)  :: dimnames( 4 ) = ''
    INTEGER            :: istart( 4 )   = 1
    INTEGER            :: iend( 4 )     = 1
    INTEGER            :: nfld2dxyt = 0
    REAL,               ALLOCATABLE :: fld( : , :, : )
    INTEGER,            ALLOCATABLE :: id_fld( : ) 
    CHARACTER(LEN=16),  ALLOCATABLE :: fldname( : )
    CHARACTER(LEN=100), ALLOCATABLE :: long_name( : )
    CHARACTER(LEN=16),  ALLOCATABLE :: units( : )
  END TYPE file_2dxyt

  TYPE(file_2dxyt)    :: file_CMAQ_omi
  TYPE(file_2dxyt)    :: file_FULL_omi
  
CONTAINS

SUBROUTINE ALLOC_fld2xyt(fld2dxyt,nvars,ncols,nrows)
 IMPLICIT NONE
 
!Arguments:
  Type(fld2ddata), ALLOCATABLE, INTENT(INOUT) :: fld2dxyt( : )
  INTEGER,                      INTENT(IN)    :: nvars
  INTEGER,                      INTENT(IN)    :: ncols
  INTEGER,                      INTENT(IN)    :: nrows
!Local:
  INTEGER  :: nn 
  
  ALLOCATE ( fld2dxyt ( nvars ) )

  
  DO nn = 1, nvars
    ALLOCATE ( fld2dxyt(nn)%fld(ncols,nrows) )
    fld2dxyt(nn)%fld = -1.0
    fld2dxyt(nn)%istart(:) = 1
    fld2dxyt(nn)%iend(1)   = ncols
    fld2dxyt(nn)%iend(2)   = nrows
  ENDDO


END SUBROUTINE ALLOC_fld2xyt

SUBROUTINE INIT_file2dxyt(file,nvars,ncols,nrows)
 IMPLICIT NONE

!Arguments:
  Type(file_2dxyt),             INTENT(INOUT) :: file
  INTEGER,                      INTENT(IN)    :: nvars
  INTEGER,                      INTENT(IN)    :: ncols
  INTEGER,                      INTENT(IN)    :: nrows
!Local:
  INTEGER  :: nn

  IF ( file%INITIALIZED )RETURN
  
! ALLOCATE ( file%fld2dxyt ( nvars ) )
  ALLOCATE ( file%id_fld(nvars) )
  ALLOCATE ( file%fldname(nvars) )
  ALLOCATE ( file%long_name(nvars) )
  ALLOCATE ( file%units(nvars) )
  ALLOCATE ( file%fld(ncols,nrows,nvars) )
  
  file%nfld2dxyt = nvars
  file%ncols = ncols
  file%nrows = nrows
  file%nlays = 1

  DO nn = 1, nvars
    file%id_fld(nn)  = -1.0
    file%fld(:,:,nn) = -1.0
    file%fldname(nn) = ''
    file%long_name(nn) = ''
    file%units(nn)    = ''
!   file%fld2dxyt(nn)%istart(:) = 1
!   file%fld2dxyt(nn)%iend(1)   = ncols
!   file%fld2dxyt(nn)%iend(2)   = nrows
  ENDDO
  file%istart(:) = 1
  file%iend(1)  = ncols
  file%iend(2)   = nrows

  file%INITIALIZED = .FALSE.

END SUBROUTINE INIT_file2dxyt

SUBROUTINE file_out_ncf (outfile_2dxyt,time_now, sdate, stime )

!-------------------------------------------------------------------------------
! Name:     Output netCDF File
! Purpose:  Create a netCDF file of output.
!-------------------------------------------------------------------------------

  use netcdf
 
  IMPLICIT NONE
! Arguments:
  Type(file_2dxyt),    INTENT(INOUT) :: outfile_2dxyt
  CHARACTER( 24 ),    INTENT(INOUT) :: time_now
  INTEGER,            INTENT(IN)    :: sdate
  INTEGER,            INTENT(IN)    :: stime
! LOCAL:

  INTEGER                         :: cdfid_m
  INTEGER                         :: dim_nlucat
  INTEGER                         :: dim_nmos
  INTEGER                         :: dim_nsoillay
  INTEGER                         :: dim_nx
  INTEGER                         :: dim_nxp1
  INTEGER                         :: dim_ny
  INTEGER                         :: dim_nyp1
  INTEGER                         :: dim_nz
  INTEGER                         :: dim_nzp1
  INTEGER                         :: dim_time
  INTEGER                         :: dim_timestr
  LOGICAL,            SAVE        :: first      = .TRUE.
  CHARACTER( MAX_LEN_FILENAME )   :: fl
  INTEGER,  SAVE,     ALLOCATABLE :: id_fld     ( : )
  INTEGER,  SAVE                  :: id_time
  INTEGER,  SAVE                  :: it         = 0
  INTEGER,            PARAMETER   :: len_time   = 19
  INTEGER                         :: mydimx
  INTEGER                         :: mydimy
  INTEGER                         :: myendx
  INTEGER                         :: myendy
  INTEGER                         :: myendz
  INTEGER                         :: n
  INTEGER                         :: nn
  INTEGER                         :: ntot
  INTEGER                         :: nvars
  CHARACTER(LEN=16),  PARAMETER   :: pname      = 'FILE_OUT_NCF'
  INTEGER                         :: rcode
  CHARACTER(LEN=32)               :: var

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CREATING DIMENSION FOR ', a, &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9200 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR DEFINING VARIABLE ', a, &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CREATING ATTRIBUTE FOR', a, &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9350 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR ENDING DEFINITIONS ', &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9400 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR WRITING VARIABLE ', a, &
    & /, 1x, '***   TO FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9500 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CREATING NETCDF FILE', &
    & /, 1x, '***   FILE = ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9700 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CLOSING NETCDF FILE', &
    & /, 1x, '***   FILE = ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------


  nvars   = outfile_2dxyt%nfld2dxyt  

! IF ( .NOT. ALLOCATED ( id_fld ) ) ALLOCATE ( id_fld ( nvars ) )


!-------------------------------------------------------------------------------
! overwrite time_now based sdate and stime
!-------------------------------------------------------------------------------

!  year = int(sdate/1000)
!  month = 

!-------------------------------------------------------------------------------
! If first time calling this routine, set up the netCDF output file.
!-------------------------------------------------------------------------------
  
  IF ( .Not. outfile_2dxyt%CREATED ) THEN

  !-----------------------------------------------------------------------------
  ! Create netCDF file.
  !-----------------------------------------------------------------------------

    fl = TRIM(outfile_2dxyt%filename)

    rcode = nf90_create (fl, nf90_clobber, cdfid_m)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9500) TRIM(pname), TRIM(fl), TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    outfile_2dxyt%cdfid_m = cdfid_m 

  !-----------------------------------------------------------------------------
  ! Set up dimensions.
  !-----------------------------------------------------------------------------

    var = "time"
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nf90_unlimited, dim_time)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "timestr"
    rcode = nf90_def_dim (cdfid_m, TRIM(var), len_time, dim_timestr)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "nx"
    nx = outfile_2dxyt%ncols
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nx, dim_nx)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "ny"
    ny = outfile_2dxyt%nrows
    rcode = nf90_def_dim (cdfid_m, TRIM(var), ny, dim_ny)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "nz"
    nz = outfile_2dxyt%nlays    
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nz, dim_nz)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "nxp1"
    nxp1 = nx + 1
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nxp1, dim_nxp1)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "nyp1"
    nyp1 = ny + 1
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nyp1, dim_nyp1)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "nzp1"
    nzp1 = nz + 1
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nzp1, dim_nzp1)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

  !-----------------------------------------------------------------------------
  ! Define variables that will populate the file.
  !-----------------------------------------------------------------------------

    var = "mtime"
    rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_char,  &
                          (/ dim_timestr, dim_time /), outfile_2dxyt%id_time)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    DO n = 1, nvars
      nn = n
      var = TRIM(outfile_2dxyt%fldname(n))
      rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                            (/ dim_nx, dim_ny, dim_time /), outfile_2dxyt%id_fld(n))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname,sdate,stime)
      ENDIF
    ENDDO
    ntot = nvars

  !-----------------------------------------------------------------------------
  ! Define global attributes.
  !-----------------------------------------------------------------------------

     CALL file_out_ncfglobal(outfile_2dxyt,sdate,stime)

  !-----------------------------------------------------------------------------
  ! Define attributes for the variables.
  !-----------------------------------------------------------------------------

    DO n = 1, nvars
      nn = n
      var = TRIM(outfile_2dxyt%long_name(n))
      rcode = nf90_put_att (cdfid_m, outfile_2dxyt%id_fld(n), 'long_name',  &
                            var)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname,sdate,stime)
      ENDIF
      rcode = nf90_put_att (cdfid_m, outfile_2dxyt%id_fld(n), 'units',  &
                            TRIM(outfile_2dxyt%units(n)) )
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname,sdate,stime)
      ENDIF
    ENDDO
    ntot = nvars
    
    rcode = nf90_enddef (cdfid_m)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9350) TRIM(pname), TRIM(fl), TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    first = .FALSE.
    outfile_2dxyt%CREATED = .TRUE.

    Return

  ENDIF  ! outfile created 

  cdfid_m = outfile_2dxyt%cdfid_m

!-------------------------------------------------------------------------------
! Write variables.
!-------------------------------------------------------------------------------

  outfile_2dxyt%itstep = outfile_2dxyt%itstep + 1
  var = "mtime"
  rcode = nf90_put_var (cdfid_m, outfile_2dxyt%id_time, time_now(1:len_time),  &
                        start = (/ 1, outfile_2dxyt%itstep /), count = (/ len_time, 1 /) )
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl), TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF


  DO n = 1, nvars
    nn = n
    var = TRIM(outfile_2dxyt%fldname(n))
    rcode = nf90_put_var (cdfid_m, outfile_2dxyt%id_fld(n), outfile_2dxyt%fld(:,:,n),  &
                          start = (/ 1, 1, outfile_2dxyt%itstep /),  &
                          count = (/ outfile_2dxyt%iend(1), &
                                     outfile_2dxyt%iend(2), 1 /) )
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF
  ENDDO
  ntot = nvars


END SUBROUTINE file_out_ncf
SUBROUTINE file_out_ncfglobal(outfile_2dxyt,sdate,stime)

!-------------------------------------------------------------------------------
! Name:     Output netCDF Global Attributes
! Purpose:  Write netCDF global attributes.
!-------------------------------------------------------------------------------

  use netcdf
 
  IMPLICIT NONE
! Arguments
  TYPE(file_2dxyt),   INTENT(IN)  :: outfile_2dxyt
  INTEGER,            INTENT(IN)  :: sdate
  INTEGER,            INTENT(IN)  :: stime
! local:
  INTEGER                         :: cdfid_in
  CHARACTER( MAX_LEN_FILENAME )   :: fl
  CHARACTER(LEN=32)               :: cstr
  CHARACTER(LEN=16),  PARAMETER   :: pname      = 'OUTFILE_NCFGLOBAL'
  INTEGER                         :: rcode
  CHARACTER(LEN=32)               :: var

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CREATING ATTRIBUTE FOR', a, &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Define global attributes.
!-------------------------------------------------------------------------------
  
  cdfid_in = outfile_2dxyt%cdfid_m
  fl       = outfile_2dxyt%filename
  
  var = "PROGNAME"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, progname)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "VERSION"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, ver)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "CODE_DATE"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, vdate)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "INPUT_MODEL"
  IF ( data_source == 2 ) THEN
    cstr = "TOMS " 
  ELSE
    cstr = "unspecified"
  ENDIF
  rcode = nf90_put_att (cdfid_in, nf90_global, var, cstr)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "NCOLS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%ncols)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "NROWS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%nrows)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "NLAYS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%nlays)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "NTHIK"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%nthik)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "GDTYP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%gdtyp_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "P_ALP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%p_alp_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "P_BET"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%p_bet_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "P_GAM"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%p_gam_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "XCENT"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%xcent_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "YCENT"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%ycent_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "XORIG"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%xorig_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "YORIG"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%yorig_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "XCELL"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%xcell_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "YCELL"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%ycell_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "VGTYP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%vgtyp_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "VGTOP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%vgtop_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF


  var = "VGLVLS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, outfile_2dxyt%vglvs_gd(:))
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "EARTH_RADIUS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, eradm)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

END SUBROUTINE file_out_ncfglobal
SUBROUTINE outncf (fld2dxyt,nfld2dxyt,time_now, sdate, stime, cdfid_m)

!-------------------------------------------------------------------------------
! Name:     Output netCDF File
! Purpose:  Create a netCDF file of output.
!-------------------------------------------------------------------------------

  use netcdf
 
  IMPLICIT NONE
! Arguments:
  Type(fld2ddata),    INTENT(INOUT) :: fld2dxyt( : )
  INTEGER,            INTENT(IN)    :: nfld2dxyt
  CHARACTER( 24 ),    INTENT(INOUT) :: time_now
  INTEGER,            INTENT(IN)    :: sdate
  INTEGER,            INTENT(IN)    :: stime
  INTEGER,            INTENT(INOUT) :: cdfid_m
! LOCAL:

  INTEGER                         :: dim_nlucat
  INTEGER                         :: dim_nmos
  INTEGER                         :: dim_nsoillay
  INTEGER                         :: dim_nx
  INTEGER                         :: dim_nxp1
  INTEGER                         :: dim_ny
  INTEGER                         :: dim_nyp1
  INTEGER                         :: dim_nz
  INTEGER                         :: dim_nzp1
  INTEGER                         :: dim_time
  INTEGER                         :: dim_timestr
  LOGICAL,            SAVE        :: first      = .TRUE.
  CHARACTER(LEN=256)              :: fl
  INTEGER,  SAVE,     ALLOCATABLE :: id_fld     ( : )
  INTEGER,  SAVE                  :: id_time
  INTEGER,  SAVE                  :: it         = 0
  INTEGER,            PARAMETER   :: len_time   = 19
  INTEGER                         :: mydimx
  INTEGER                         :: mydimy
  INTEGER                         :: myendx
  INTEGER                         :: myendy
  INTEGER                         :: myendz
  INTEGER                         :: n
  INTEGER                         :: nn
  INTEGER                         :: ntot
  INTEGER                         :: nvars
  CHARACTER(LEN=16),  PARAMETER   :: pname      = 'OUTNCF'
  INTEGER                         :: rcode
  CHARACTER(LEN=32)               :: var

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CREATING DIMENSION FOR ', a, &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9200 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR DEFINING VARIABLE ', a, &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CREATING ATTRIBUTE FOR', a, &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9350 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR ENDING DEFINITIONS ', &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9400 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR WRITING VARIABLE ', a, &
    & /, 1x, '***   TO FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9500 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CREATING NETCDF FILE', &
    & /, 1x, '***   FILE = ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9700 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CLOSING NETCDF FILE', &
    & /, 1x, '***   FILE = ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------


  nvars = nfld2dxyt  

! IF ( .NOT. ALLOCATED ( id_fld ) ) ALLOCATE ( id_fld ( nvars ) )


!-------------------------------------------------------------------------------
! overwrite time_now based sdate and stime
!-------------------------------------------------------------------------------

!  year = int(sdate/1000)
!  month = 

!-------------------------------------------------------------------------------
! If first time calling this routine, set up the netCDF output file.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

  !-----------------------------------------------------------------------------
  ! Create netCDF file.
  !-----------------------------------------------------------------------------

    fl = TRIM('test.ncf')

    rcode = nf90_create (fl, nf90_clobber, cdfid_m)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9500) TRIM(pname), TRIM(fl), TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

  !-----------------------------------------------------------------------------
  ! Set up dimensions.
  !-----------------------------------------------------------------------------

    var = "time"
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nf90_unlimited, dim_time)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "timestr"
    rcode = nf90_def_dim (cdfid_m, TRIM(var), len_time, dim_timestr)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "nx"
    nx = ncols
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nx, dim_nx)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "ny"
    ny = nrows
    rcode = nf90_def_dim (cdfid_m, TRIM(var), ny, dim_ny)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "nz"
    nz = nlays    
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nz, dim_nz)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "nxp1"
    nxp1 = nx + 1
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nxp1, dim_nxp1)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "nyp1"
    nyp1 = ny + 1
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nyp1, dim_nyp1)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    var = "nzp1"
    nzp1 = nz + 1
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nzp1, dim_nzp1)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

  !-----------------------------------------------------------------------------
  ! Define variables that will populate the file.
  !-----------------------------------------------------------------------------

    var = "mtime"
    rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_char,  &
                          (/ dim_timestr, dim_time /), fld2dxyt( 1 )%id_time)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    DO n = 1, nfld2dxyt
      nn = n
      var = TRIM(fld2dxyt(n)%fldname)
      rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                            (/ dim_nx, dim_ny, dim_time /), fld2dxyt(nn)%id_fld)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname,sdate,stime)
      ENDIF
    ENDDO
    ntot = nfld2dxyt

  !-----------------------------------------------------------------------------
  ! Define global attributes.
  !-----------------------------------------------------------------------------

    CALL outncfglobal (cdfid_m, fl, sdate, stime)

  !-----------------------------------------------------------------------------
  ! Define attributes for the variables.
  !-----------------------------------------------------------------------------

    DO n = 1, nfld2dxyt
      nn = n
      var = TRIM(fld2dxyt(n)%fldname)
      rcode = nf90_put_att (cdfid_m, fld2dxyt(n)%id_fld, 'long_name',  &
                            TRIM(fld2dxyt(n)%long_name))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname,sdate,stime)
      ENDIF
      rcode = nf90_put_att (cdfid_m, fld2dxyt(n)%id_fld, 'units',  &
                            TRIM(fld2dxyt(n)%units))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname,sdate,stime)
      ENDIF
    ENDDO
    ntot = nfld2dxyt
    
    rcode = nf90_enddef (cdfid_m)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9350) TRIM(pname), TRIM(fl), TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF

    first = .FALSE.
    Return

  ENDIF  ! first = .TRUE.

!-------------------------------------------------------------------------------
! Write variables.
!-------------------------------------------------------------------------------

  it = it + 1
  var = "mtime"
  rcode = nf90_put_var (cdfid_m, fld2dxyt( 1 )%id_time, time_now(1:len_time),  &
                        start = (/ 1, it /), count = (/ len_time, 1 /) )
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl), TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF


  DO n = 1, nfld2dxyt
    nn = n
    var = TRIM(fld2dxyt(n)%fldname)
    rcode = nf90_put_var (cdfid_m, fld2dxyt(n)%id_fld, fld2dxyt(n)%fld,  &
                          start = (/ 1, 1, it /),  &
                          count = (/ fld2dxyt(n)%iend(1), &
                                     fld2dxyt(n)%iend(2), 1 /) )
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname,sdate,stime)
    ENDIF
  ENDDO
  ntot = nfld2dxyt


END SUBROUTINE outncf

SUBROUTINE outncfglobal (cdfid_in,fl,sdate,stime)

!-------------------------------------------------------------------------------
! Name:     Output netCDF Global Attributes
! Purpose:  Write netCDF global attributes.
!-------------------------------------------------------------------------------

  use netcdf
 
  IMPLICIT NONE
! Arguments
  INTEGER,            INTENT(IN)  :: cdfid_in
  CHARACTER(LEN=256), INTENT(IN)  :: fl
  INTEGER,            INTENT(IN)  :: sdate
  INTEGER,            INTENT(IN)  :: stime
! local:
  CHARACTER(LEN=32)               :: cstr
  CHARACTER(LEN=16),  PARAMETER   :: pname      = 'OUTNCFGLOBAL'
  INTEGER                         :: rcode
  CHARACTER(LEN=32)               :: var

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CREATING ATTRIBUTE FOR', a, &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Define global attributes.
!-------------------------------------------------------------------------------

  var = "PROGNAME"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, progname)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "VERSION"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, ver)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "CODE_DATE"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, vdate)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "INPUT_MODEL"
  IF ( data_source == 2 ) THEN
    cstr = "TOMS " 
  ELSE
    cstr = "unspecified"
  ENDIF
  rcode = nf90_put_att (cdfid_in, nf90_global, var, cstr)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "NCOLS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, ncols)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "NROWS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, nrows)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "NLAYS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, nlays)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "NTHIK"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, nthik)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "GDTYP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, gdtyp_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "P_ALP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, p_alp_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "P_BET"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, p_bet_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "P_GAM"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, p_gam_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "XCENT"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, xcent_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "YCENT"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, ycent_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "XORIG"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, xorig_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "YORIG"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, yorig_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "XCELL"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, xcell_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "YCELL"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, ycell_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "VGTYP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, vgtyp_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "VGTOP"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, vgtop_gd)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF


  var = "VGLVLS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, vglvs_gd(:))
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

  var = "EARTH_RADIUS"
  rcode = nf90_put_att (cdfid_in, nf90_global, var, eradm)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname,sdate,stime)
  ENDIF

END SUBROUTINE outncfglobal
SUBROUTINE graceful_stop (pname,sdate,stime)

!-------------------------------------------------------------------------------
! Name:     Graceful Stop
! Purpose:  Gracefully stop program and close I/O API files.
! Revised:  09 Jan 2002  Original version.  (T. Otte)
!           30 Aug 2011  Changed F77 character declarations to F90 standard.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------


  IMPLICIT NONE

! Arguements:
  CHARACTER(LEN=16),  INTENT(IN)    :: pname
  INTEGER,            INTENT(IN)    :: sdate
  INTEGER,            INTENT(IN)    :: stime
! local:
  CHARACTER(LEN=80)                 :: xmsg

  xmsg = 'ABNORMAL TERMINATION IN ' // TRIM(pname) // ' at '
  write(6,'(A,2(I8,1X))')TRIM(xmsg), sdate, stime
  stop

END SUBROUTINE graceful_stop
SUBROUTINE close_files(cdfid,sdate,stime)

!-------------------------------------------------------------------------------
! Name:     Close NETCDF Files
! Purpose:  Close I/O API files.
!-------------------------------------------------------------------------------

  USE netcdf

  IMPLICIT NONE

! Arguments:
  INTEGER,            INTENT(IN)    :: cdfid
  INTEGER,            INTENT(IN)    :: sdate
  INTEGER,            INTENT(IN)    :: stime
! local:
  CHARACTER(LEN=16),  PARAMETER     :: pname      = 'CLOSE_FILES'
  INTEGER                           :: rcode

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   COULD NOT CLOSE I/O API OUTPUT FILES', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CLOSING NETCDF FILE', &
    & /, 1x, '***   FILE UNIT = ', i8, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Gracefully close output files.
!-------------------------------------------------------------------------------

      rcode = nf90_close (cdfid)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9100) TRIM(pname), cdfid,  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname,sdate,stime)
      ENDIF


END SUBROUTINE close_files

END MODULE OUTNCF_FILE_ROUTINES
