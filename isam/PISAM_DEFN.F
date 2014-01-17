      MODULE PISAM_DEFN
c
c
C     This module is referenced in driver and sciproc. driver must call 
c     HGRD_INIT, CGRID_MAP, and PAR_INIT prior to calling PISAM_INIT (below).
c
c     Author: bo wang
c     Email:  bwang@cert.ucr.edu
c 
c-----------------------------------------------------------------------

!      USE HGRD_DEFN    ! horizontal domain specifications
      USE GRID_CONF    ! horizontal domain specifications
      USE UTILIO_DEFN  ! 20120615

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE, TARGET :: PISAM( :,:,:,:,: )

      CONTAINS
        FUNCTION PISAM_INIT (NSPC_TSSA, NTAG_TSSA) RESULT ( SUCCESS )

!         INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
!0615    INCLUDE SUBST_IODECL      ! I/O definitions and declarations

         INTEGER :: NSPC_TSSA
         INTEGER :: NTAG_TSSA

         LOGICAL :: SUCCESS
         INTEGER :: ALLOCSTAT
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 120 ) :: XMSG = ' '

C PISAM extents
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
            
            ALLOCATE ( PISAM( LCOL:HCOL,
     &                        LROW:HROW,
     &                        NLAYS,
     &                        NSPC_TSSA,
     &                        NTAG_TSSA ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating PISAM'
               CALL M3WARN ( 'PISAM_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
            
            PISAM = 0.0
            
         ELSE
            XMSG = 'PISAM already ALLOCATED'
            CALL M3WARN ( 'PISAM_INIT', 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         
         RETURN
         END FUNCTION PISAM_INIT
      
      END MODULE PISAM_DEFN
