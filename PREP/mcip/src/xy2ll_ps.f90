SUBROUTINE xy2ll_ps (xx, yy, phi1, lambda0, phi, lambda)

!-------------------------------------------------------------------------------
! Name:     (X,Y) to Latitude-Longitude for Polar Stereographic Projection
! Purpose:  Calcluates latitude-longitude for a given (X,Y) pair from origin
!           and polar stereographic projection information.
! Notes:    Equations taken from "Map Projections: Theory and Applications"
!           by Frederick Pearson, II (1990), pp. 190-192.
! Revised:  18 Sep 2009  Original version.  (T. Otte)
!           14 May 2025  Made as a standalone routine. (T. Spero)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL(8)                      :: cc
  REAL(8)                      :: deg2rad    ! convert degrees to radians
  REAL(8),       PARAMETER     :: drearth    = 6370000.0d0  ! earth radius [m]
  REAL(8)                      :: hemi       ! +/-1 for Northern/Southern Hemis
  REAL,          INTENT(OUT)   :: lambda     ! longitude [deg]
  REAL(8)                      :: lambdarad  ! longitude [rad]
  REAL,          INTENT(IN)    :: lambda0    ! standard longitude [deg]
  REAL(8)                      :: lambda0rad ! standard longitude [rad]
  REAL,          INTENT(OUT)   :: phi        ! latitude [deg]
  REAL(8)                      :: phirad     ! latitude [rad]
  REAL,          INTENT(IN)    :: phi1       ! true latitude 1 [deg]
  REAL(8)                      :: phi1rad    ! true latitude 1 [rad]
  REAL(8)                      :: pi
  REAL(8)                      :: piover2    ! pi/2
  REAL(8)                      :: rad2deg
  REAL(8)                      :: rho
  REAL(8)                      :: sigma
  REAL(8),       INTENT(IN)    :: xx         ! X-coordinate from origin
  REAL(8),       INTENT(IN)    :: yy         ! Y-coordinate from origin

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover2 = 2.0d0 * DATAN(1.0d0)
  pi      = 2.0d0 * piover2
  deg2rad = pi / 1.8d2
  rad2deg = 1.8d2 / pi

!-------------------------------------------------------------------------------
! Set up geometric constants.
!-------------------------------------------------------------------------------

  hemi = DSIGN (1.0d0, DBLE(phi1))

!-------------------------------------------------------------------------------
! Compute latitude (PHI).
!-------------------------------------------------------------------------------

  phi1rad = DBLE(phi1) * deg2rad
  sigma   = (1.0d0 + DSIN(phi1rad)) / 2.0d0 * hemi
  rho     = DSQRT ( xx*xx + yy*yy )
  cc      = 2.0d0 * DATAN2( rho , 2.0d0 * drearth * sigma )
  phirad  = ( piover2 - cc ) * hemi
  phi     = REAL( phirad * rad2deg )

!-------------------------------------------------------------------------------
! Compute longitude (LAMBDA).
!-------------------------------------------------------------------------------

  lambda0rad = DBLE(lambda0) * deg2rad
  lambdarad  = lambda0rad + DATAN2 ( xx, -hemi*yy )
  lambda     = REAL( lambdarad * rad2deg )
  IF ( lambda < -180.0 ) THEN
    lambda = 360.0 + lambda
  ENDIF

END SUBROUTINE xy2ll_ps
