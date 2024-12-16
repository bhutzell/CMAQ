
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
      MODULE VGRD_DEFN

C Define the vertical layer structure (replaces COORD.EXT and VGRD.EXT)
C Revision History: Jeff Young: 6 Dec 04: created
C                              26 Jul 06: comments in line with PARMS3.EXT
C                   S. Roselle 29 Mar 11: Replaced I/O API include files
C                                         with UTILIO_DEFN
C                   D. Wong    11 May 11: incorporated twoway model implementation
C.......................................................................

      USE RUNTIME_VARS

      IMPLICIT NONE

      INTEGER, SAVE :: NLAYS = 0

      REAL, ALLOCATABLE, SAVE :: VGLVS_GD( : )  ! vert layer surface values
      REAL, ALLOCATABLE, SAVE :: X3FACE_GD( : ) ! mono. incr. vert coord values
                                                ! = 1 - VGLVS_GD
      CHARACTER( 16 ), SAVE :: GDNAME_GD

C The definitions and declarations in this F90 Module follow those
C given in the I/O-API include file FDESC3C.EXT and replace similar
C definitions. This F90 Module is compatible with FDESC3C.EXT.

C VGTYP_GD:
C The vertical grid type:
C  1: VGSGPH3 : hydrostatic sigma-P
C  2: VGSGPN3 : non-h sigma-P
C  3: VGSIGZ3 : sigma-Z
C  4: VGPRES3 : pressure (mb)
C  5: VGZVAL3 : Z (m) (above sea lvl)
C  6: VGHVAL3 : H (m) (above ground)
C  7: VGWRFEM : WRF mass-core sigma
C  8: VGWRFNM : WRF NMM
C  9: IMISS3 for vertical coordinates not stored in VGLVSD
C            (e.g., temporally or spatially changing vertical coordinates)

      INTEGER, SAVE :: VGTYP_GD

C VGTPUN_GD:
C The units of the vertical coordinate top.

      CHARACTER( 16 ), SAVE :: VGTPUN_GD

C VGTOP_GD:
C The value for the model top used in the definition of the sigma coordinate
C systems in the VGTPUN_GD units.

      REAL, SAVE :: VGTOP_GD

C For sigma-P, the relationship between pressure levels P and sigma-P is
C given by:
C    sigma-P = ( P - VGTOP_GD ) / (P_srf - VGTOP_GD ),
C    where P_srf is the surface pressure.

C VGLVUN_GD:
C The units of the vertical coordinate surface values,

      CHARACTER( 16 ) :: VGLVUN_GD

C-----------------------------------------------------------------------

      CONTAINS

         FUNCTION VGRD_INIT ( ) RESULT ( SUCCESS )

            USE UTILIO_DEFN
            use mio_module

            IMPLICIT NONE

            INCLUDE SUBST_FILES_ID
 
            LOGICAL :: SUCCESS

            LOGICAL, SAVE :: FIRSTIME = .TRUE.
            CHARACTER( 96 ) :: XMSG = ' '

            INTEGER L, ALLOCSTAT, floc
            real, allocatable :: vglvs_mc3(:)

C This function is expected to be called only once - at startup

            IF ( FIRSTIME ) THEN
               FIRSTIME = .FALSE.
               SUCCESS = .TRUE.

#ifndef mpas

C get vertical layer definition 

               WRITE( LOGDEV, * )
               CALL LOG_HEADING( LOGDEV, "Retrieve Vertical Grid" )

               call mio_fopen ( MET_CRO_3D, mio_read_only )
               floc = mio_search ( MET_CRO_3D )
               NLAYS = mio_file_data(floc)%nlays
      
               call mio_get_global_attr( MET_CRO_3D, 'GDNAM', GDNAME_GD )
               call mio_get_global_attr( MET_CRO_3D, 'VGTYP', VGTYP_GD )
               call mio_get_global_attr( MET_CRO_3D, 'VGTOP', VGTOP_GD )
               VGTPUN_GD = 'Pa'
               VGLVUN_GD = 'none'

! For array-valued attributes, mio_get_global_attr returns the length of the array
! in the first element. We know what this value should be already, so skip over it.
               ALLOCATE ( VGLVS_GD( NLAYS + 1 ), 
     &                    vglvs_mc3( nlays + 2), 
     &                    X3FACE_GD( 0:NLAYS ), STAT = ALLOCSTAT )
               IF ( ALLOCSTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating VGLVS_GD, X3FACE_GD'
                  CALL LOG_WARNING( 'VGRD_INIT', LOGDEV, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

               call mio_get_global_attr( MET_CRO_3D, 'VGLVLS', VGLVS_mc3 )

C set layer layer thicknesses

               DO L = 0, NLAYS
                  VGLVS_GD( L+1 ) = VGLVS_MC3( L+2 )
                  X3FACE_GD( L ) = 1.0 - VGLVS_GD( L + 1 )
               END DO

               VGLVS3D(1:NLAYS+1) = VGLVS_GD(1:NLAYS+1)
               deallocate( VGLVS_MC3 )
            ELSE
               XMSG = 'Vertical layer structure already defined'
               CALL LOG_WARNING( 'VGRD_INIT', LOGDEV, XMSG )
               SUCCESS = .FALSE.; RETURN
#endif
            END IF   ! FIRSTIME

         RETURN
         END FUNCTION VGRD_INIT

      END MODULE VGRD_DEFN

