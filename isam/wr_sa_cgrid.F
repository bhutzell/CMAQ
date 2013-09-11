
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
c     
c      [2] write tssa concentration data 
c     
C     Revision history
C     created on 28 Oct 02 by Bo Wang: 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      SUBROUTINE WR_SA_CGRID ( JDATE, JTIME, TSTEP, ISAM )

      USE GRID_CONF
      USE UTILIO_DEFN           ! 20120615
      USE SUBST_MODULES         ! stenex
      USE SA_DEFN               ! Mc06

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! I/O definitions and declarations

C...Arguments Declaration
      INTEGER    JDATE          ! current model date, coded YYYYDDD
      INTEGER    JTIME          ! current model time, coded HHMMSS
      INTEGER    TSTEP(2)       ! format 10000, 0, 0
      REAL, POINTER :: ISAM( :,:,:,:,: )

C...End of argument declaration

C Local variables:
      
      INTEGER          ALLOCSTAT, LOGDEV

      CHARACTER( 16 ) :: PNAME = 'WR_SA_CGRID'
      CHARACTER( 96 ) :: XMSG = ' '
      
      INTEGER   C,R,S,K,N, L, ITAG, JSPC    ! loop induction variables

      REAL, ALLOCATABLE :: ISAM_BUFF ( :,:,:,: )

C-----------------------------------------------------------------------


      CALL SUBST_BARRIER


      IF ( .NOT. OPEN3( SA_CGRID_1, FSRDWR3, PNAME ) ) THEN
        XMSG = 'Could not open ' // SA_CGRID_1 // ' file for update - '
     &                                        // 'try to open new'
        CALL M3MESG( XMSG )

        IF ( MYPE .EQ. 0 ) THEN
          ! open sa_conc_1
          IF ( .NOT. OPEN3( SA_CONC_1, FSRDWR3, PNAME ) ) THEN
            XMSG = 'Could not open ' // SA_CONC_1 // ' file for update '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          ENDIF ! sa_conc_1 opened ?
      
          ! Get description of sa_conc_1
          IF ( .NOT. DESC3( SA_CONC_1 ) ) THEN
            XMSG = 'Could not get file description from ' // SA_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          ENDIF ! sa_conc_1 description obtained ?

          SDATE3D = JDATE
          STIME3D = JTIME
          NLAYS3D = NLAYS

          ! Create sa_cgrid_1
          IF ( .NOT. OPEN3( SA_CGRID_1, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not open' // SA_CGRID_1 
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          ENDIF !
        ENDIF ! mype0 ?

      END IF ! sa_cgrid_1 opened ?


      ALLOCATE ( ISAM_BUFF( MY_NCOLS, MY_NROWS, NLAYS, N_SPCTAG ),
     &     STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
        XMSG = 'Failure allocating ISAM_BUFF'
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      ENDIF
      ISAM_BUFF = 0.0
    

      DO S = 1, N_SPCTAG
        ISAM_BUFF( :,:,:,S ) = ISAM( :,:,:,S_SPCTAG(S),T_SPCTAG(S) )
        IF ( .NOT. WRITE3( SA_CGRID_1, VNAME3D(S), JDATE, JTIME,
     &          ISAM_BUFF(:,:,:,S)) ) THEN
           XMSG = 'Could not write to ' 
     &           // SA_CGRID_1 
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
      END DO ! S
      DEALLOCATE( ISAM_BUFF )
      
      return
      end

