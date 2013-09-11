
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

      MODULE PSAEM_DEFN
c
c
C     This module is referenced in driver and sciproc. driver must call 
c     HGRD_INIT, CGRID_MAP, and PAR_INIT prior to calling PSAEM_INIT (below).
c
c     Author: bo wang
c     Email:  bwang@cert.ucr.edu
c 
c-----------------------------------------------------------------------

!      USE HGRD_DEFN    ! horizontal domain specifications
      USE GRID_CONF    ! horizontal domain specifications
      USE UTILIO_DEFN  ! 20120712

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE, TARGET :: PSAEM( :,:,:,:,: )

      CONTAINS
        FUNCTION PSAEM_INIT (NSPC_TSSA, NTAG_TSSA) RESULT ( SUCCESS )

!         INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
!0712    INCLUDE SUBST_IODECL      ! I/O definitions and declarations

         INTEGER :: NSPC_TSSA
         INTEGER :: NTAG_TSSA

         LOGICAL :: SUCCESS
         INTEGER :: ALLOCSTAT
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 120 ) :: XMSG = ' '

C PSAEM extents
         INTEGER :: LCOL
         INTEGER :: HCOL
         INTEGER :: LROW
         INTEGER :: HROW

         SUCCESS = .TRUE.

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.

#ifdef parallel
            LCOL = 1 - NTHIK
            HCOL = NCOLS + NTHIK
            LROW = 1 - NTHIK
            HROW = NROWS + NTHIK
#else
            LCOL = 0
            HCOL = NCOLS+1
            LROW = 0
            HROW = NROWS+1
#endif
            
            ALLOCATE ( PSAEM( LCOL:HCOL,
     &                        LROW:HROW,
     &                        NLAYS,
     &                        NSPC_TSSA,
     &                        NTAG_TSSA ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating PSAEM'
               CALL M3WARN ( 'PSAEM_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
            
            PSAEM = 0.0
            
         ELSE
            XMSG = 'PSAEM already ALLOCATED'
            CALL M3WARN ( 'PSAEM_INIT', 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         
         RETURN
         END FUNCTION PSAEM_INIT
      
      END MODULE PSAEM_DEFN
