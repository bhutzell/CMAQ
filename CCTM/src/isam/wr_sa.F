
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

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     
C Writes ISAM values to SA_CONC_1 file
C
C          Called by driver.F
C     
C     Revision history
C     01 Nov 2018: S.Napelenok Updates for cmaq5.3 release 
C     09 May 2019: D.Wong Removed all MY_ clauses
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      SUBROUTINE WR_SA ( JDATE, JTIME, TSTEP, NSTEPS )

      USE GRID_CONF
      USE UTILIO_DEFN
      USE SA_DEFN    
      USE RUNTIME_VARS
      USE MIO_MODULE
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! I/O definitions and declarations

C...Arguments Declaration
      INTEGER    JDATE          ! current model date, coded YYYYDDD
      INTEGER    JTIME          ! current model time, coded HHMMSS
      INTEGER    TSTEP(3)       ! format 10000, 0, 0

      INTEGER    NSTEPS         ! Number of time steps of 3D output

C Local variables:
      
      INTEGER         ::   ALLOCSTAT
      LOGICAL, SAVE   ::   FIRSTIME = .TRUE.
      LOGICAL, SAVE   ::   FIRSTWRITE = .TRUE.
      CHARACTER( 16 ) :: PNAME = 'WR_SA'
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 20 ) :: TIME_STAMP
      
      INTEGER   C,R,S,K,L    ! loop induction variables

      REAL, ALLOCATABLE, SAVE :: ISAM_BUFF ( :,:,:,: )

C-----------------------------------------------------------------------

      CALL SUBST_BARRIER

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

         IF ( MYPE .NE. 0 ) THEN
           IF ( .NOT. OPEN3( SA_CONC_1, FSREAD3, PNAME ) ) THEN
             XMSG = 'Could not open ' // SA_CONC_1 // 'file for update'
             CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
           END IF !open3
         END IF ! mype nonzero

         IF ( .NOT. DESC3( SA_CONC_1 ) ) THEN
           XMSG = 'Could not get file description from ' // SA_CONC_1
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( ISAM_ELEV .EQ. -1 ) ISAM_ELEV = NLAYS
         SA_NLAYS = ISAM_ELEV - ISAM_BLEV + 1

         ALLOCATE ( ISAM_BUFF( NCOLS, NROWS, SA_NLAYS, N_SPCTAG ),
     &              STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating ISAM_BUFF'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         ENDIF

      END IF ! firstime

      IF ( FIRSTWRITE ) THEN
         FIRSTWRITE = .FALSE.
! If not writing all layers, change dimension mio_nlays before file creation, 
! then change global attribute NLAYS to match
         call mio_setfile( MET_CRO_3D )
         if ( SA_NLAYS .lt. NLAYS ) mio_nlays = SA_NLAYS
         call mio_fcreate( 'SA_CONC_1_MIO', io_mode )
         if ( mype == 0 ) then
            call mio_set_global_attr( 'SA_CONC_1_MIO', 'NLAYS', SA_NLAYS )
            call mio_set_global_attr( 'SA_CONC_1_MIO', 'VGLVLS', MET_CRO_3D, AISAM_BLEV, AISAM_ELEV+1 )
         end if
      END IF

      call mio_time_format_conversion( JDATE, JTIME, TIME_STAMP )
      
      ISAM_BUFF = 0.0

      DO S = 1, N_SPCTAG
         K = 0
         DO L = ISAM_BLEV, ISAM_ELEV
            K = K + 1
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  ISAM_BUFF( C,R,K,S ) = ISAM( C,R,L,S_SPCTAG(S),T_SPCTAG(S) )
               ENDDO
            ENDDO
         ENDDO

         IF ( .NOT. WRITE3( SA_CONC_1, VNAM_SPCTAG(S), JDATE, JTIME,
     &        ISAM_BUFF(:,:,:,S)) ) THEN
            XMSG = 'Could not write to ' // trim( SA_CONC_1 )
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         call mio_fwrite( 'SA_CONC_1_MIO', VNAM_SPCTAG( S ),
     &                    PNAME, ISAM_BUFF( :,:,:,S ), TIME_STAMP )
      END DO

      return
      end

