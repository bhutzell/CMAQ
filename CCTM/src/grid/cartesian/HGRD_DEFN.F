
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
      MODULE HGRD_DEFN

C Define the horizontal domain, globally and for each processor, if parallel
C.......................................................................
      USE RUNTIME_VARS

      IMPLICIT NONE

C returned coordinate system (projection)
      CHARACTER( 16 ), SAVE :: COORD_SYS_NAME

C map projection type (should be named PRTYP_GD!)
      INTEGER, SAVE :: GDTYP_GD = 2 ! LAMGRD3

C first map projection parameter (degrees)
      REAL( 8 ), SAVE :: P_ALP_GD = 30.D0

C second map projection parameter (degrees)
      REAL( 8 ), SAVE :: P_BET_GD = 60.D0

C third map projection parameter (degrees)
      REAL( 8 ), SAVE :: P_GAM_GD = -90.D0

C longitude for coord-system center (degrees)
      REAL( 8 ), SAVE :: XCENT_GD = -90.D0

C latitude for coord-system center (degrees)
      REAL( 8 ), SAVE :: YCENT_GD = 40.D0

      REAL( 8 ), SAVE :: XORIG_GD ! X-coordinate origin of computational grid
      REAL( 8 ), SAVE :: YORIG_GD ! Y-coordinate origin of computational grid

      REAL( 8 ), SAVE :: XCELL_GD ! X-coordinate cell width (M)
      REAL( 8 ), SAVE :: YCELL_GD ! Y-coordinate cell width (M)

      INTEGER, SAVE :: GL_NCOLS   ! no. of columns in global grid
      INTEGER, SAVE :: GL_NROWS   ! no. of rows in global grid
 
      INTEGER, SAVE :: NCOLS      ! grid columns array dimension
      INTEGER, SAVE :: NROWS      ! grid rows array dimension
      INTEGER, SAVE :: NBNDY      ! no. of cells in one layer of local boundary
      INTEGER, SAVE :: NTHIK      ! boundary thickness (cells)
 
C column range for each processor
      INTEGER, ALLOCATABLE, SAVE :: COLSX_PE( :,: )
      INTEGER, ALLOCATABLE, SAVE :: COLSD_PE( :,: )  ! dot file
C row range for each processor
      INTEGER, ALLOCATABLE, SAVE :: ROWSX_PE( :,: )
      INTEGER, ALLOCATABLE, SAVE :: ROWSD_PE( :,: )  ! dot file

C maximum stencil displacement in the north, east, south, and west direction
      INTEGER, PARAMETER :: MNDIS = 2
      INTEGER, PARAMETER :: MEDIS = 2
      INTEGER, PARAMETER :: MSDIS = 2
      INTEGER, PARAMETER :: MWDIS = 2

      LOGICAL :: IO_PE_INCLUSIVE

      CONTAINS

         FUNCTION HGRD_INIT ( NPROCS, MYID ) RESULT ( SUCCESS )

         USE UTILIO_DEFN
         use replacement_util_module
         use mio_module

         INCLUDE SUBST_FILES_ID

         INTEGER, INTENT( IN ) :: NPROCS  ! total worker processors
         INTEGER, INTENT( IN ) :: MYID
         LOGICAL :: SUCCESS

         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER :: STATUS, ALST

! These arrays are duplicative of those in mio...
         INTEGER, ALLOCATABLE :: NCOLS_PE( : )  ! Column range for each PE
         INTEGER, ALLOCATABLE :: NROWS_PE( : )  ! Row range for each PE

         INTEGER I

C-----------------------------------------------------------------------

C This function is expected to be called only once - at startup

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            SUCCESS = .TRUE.

            MYPE = MYID

#ifndef mpas
C With GRID_NAME (only input) retrieve all horizontal grid parameters from
C the grid description file pointed to by the GRIDDESC env var:
            IF ( .NOT. RDSCGRID( GRID_NAME,
     &                           COORD_SYS_NAME, GDTYP_GD, 
     &                           P_ALP_GD, P_BET_GD, P_GAM_GD,
     &                           XCENT_GD, YCENT_GD,
     &                           XORIG_GD, YORIG_GD, XCELL_GD, YCELL_GD,
     &                           GL_NCOLS, GL_NROWS, NTHIK ) ) THEN
               XMSG = 'Failure retrieving horizontal grid parameters'
               CALL LOG_WARNING( 'HGRD_INIT', LOGDEV, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
#endif
 
            IF ( GL_NROWS .LE. 1 .AND. GL_NCOLS .LE. 1 ) COLUMN_MODEL = .TRUE.

C Check NPROCS against NPCOL*NPROW
            IF ( NPROCS .NE. NPCOL*NPROW ) THEN
               WRITE( LOGDEV,* ) ' --- Nprocs, NProw, NPcol ',
     &                                 NPROCS, NPROW, NPCOL
               XMSG = 'NPROCS is not equal to NPCOL*NPROW'
               CALL LOG_WARNING( 'HGRD_INIT', LOGDEV, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            IF ( NPROCS .GT. GL_NCOLS*GL_NROWS ) THEN

               WRITE( LOGDEV,* ) ' --- Nprocs, TOTAL_NCOLS, TOTAL_NROWS ',
     &                                 NPROCS, GL_NCOLS, GL_NROWS
               IF ( MYPE .EQ. 0 ) THEN
                  WRITE( OUTDEV,'(A,4(1X,I4))') ' --- Nprocs, TOTAL_NCOLS, TOTAL_NROWS ',
     &                                                NPROCS, GL_NCOLS, GL_NROWS
               END IF

               XMSG = 'NPROCS must be less than or equal to TOTAL_NCOLS*TOTAL_NROWS'
               IF ( MYPE .EQ. 0 ) THEN
                  WRITE(OUTDEV,'(A)')TRIM( XMSG )
               END IF 
               CALL LOG_WARNING( 'HGRD_INIT', LOGDEV, XMSG )

               XMSG = 'Change NPROCS, NPCOL, NPROW in runscript to fit TOTAL_NCOLS and TOTAL_NROWS'
               IF ( MYPE .EQ. 0 ) THEN
                  WRITE(OUTDEV,'(A,/)')TRIM( XMSG )
               END IF 
               WRITE(LOGDEV,'(A)')TRIM( XMSG )

               SUCCESS = .FALSE.; RETURN

            END IF

            ALLOCATE ( COLSX_PE( 2,NPROCS ),
     &                 ROWSX_PE( 2,NPROCS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** COLSX_PE or ROWSX_PE Memory allocation failed'
               CALL LOG_WARNING( 'HGRD_INIT', LOGDEV, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            ALLOCATE ( COLSD_PE( 2,NPROCS ),
     &                 ROWSD_PE( 2,NPROCS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** COLSD_PE or ROWSD_PE  Memory allocation failed'
               CALL LOG_WARNING( 'HGRD_INIT', LOGDEV, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

           call mio_fopen( INIT_CONC_1, mio_read_only )
           call mio_setfile( INIT_CONC_1 )
           ncols = mio_ncols
           nrows = mio_nrows
           nbndy = 2*mio_domain_nthik * ( ncols + nrows + 2*mio_domain_nthik )

            do i = 1, nprocs 
               colsx_pe(:,i) = mio_domain_colde_pe(:,i,1)   ! starting and ending column for cross points
               rowsx_pe(:,i) = mio_domain_rowde_pe(:,i,1)   ! starting and ending row for cross points
               colsd_pe(:,i) = mio_domain_colde_pe(:,i,2)   ! starting and ending column for dot points
               rowsd_pe(:,i) = mio_domain_rowde_pe(:,i,2)   ! starting and ending row for dot points
            end do

         ELSE
            XMSG = 'Horizontal domain decomposition already defined'
            CALL LOG_WARNING( 'HGRD_INIT', LOGDEV, XMSG )
            SUCCESS = .FALSE.; RETURN

         END IF   ! FIRSTIME

         RETURN
         END FUNCTION HGRD_INIT
 
      END MODULE HGRD_DEFN
