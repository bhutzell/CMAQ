cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c      [2] write tssa concentration data 
c     
C     Revision history
C     created on 28 Oct 02 by Bo Wang: 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      SUBROUTINE WR_SA ( JDATE, JTIME, TSTEP, NSTEPS, ISAM )

!      USE HGRD_DEFN             ! horizontal domain specifications
      USE GRID_CONF
      USE UTILIO_DEFN           ! 20120615
      USE SA_LAYERS
      USE SA_DEFN               ! Mc06
      USE SUBST_MODULES         ! stenex

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! I/O definitions and declarations

C...Arguments Declaration
      INTEGER    JDATE          ! current model date, coded YYYYDDD
      INTEGER    JTIME          ! current model time, coded HHMMSS
      INTEGER    TSTEP(3)       ! format 10000, 0, 0
cyqw
      INTEGER    NSTEPS         ! Number of time steps of 3D output
cyqw
      REAL, POINTER :: ISAM( :,:,:,:,: )
!0711 CHARACTER( 16 )       :: FILE_NAME

C...End of argument declaration

C Local variables:
      
      INTEGER          ALLOCSTAT, LOGDEV

      CHARACTER( 16 ) :: PNAME = 'WR_SA'
      CHARACTER( 96 ) :: XMSG = ' '
      
      INTEGER   C,R,S,K,N, L, ITAG, JSPC    ! loop induction variables

      REAL, ALLOCATABLE :: ISAM_BUFF ( :,:,:,: )

C-----------------------------------------------------------------------


      CALL SUBST_BARRIER


Ckrt     print*,'N_SPCTAG, JDATE, JTIME '
Ckrt     print*, N_SPCTAG, JDATE, JTIME  

!0710    IF ( .NOT. OPEN3( FILE_NAME, FSRDWR3, PNAME ) ) THEN
!0710       XMSG = 'Could not open ' // FILE_NAME // ' file for update'
!0710       CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!0710    END IF

!0711    IF ( .NOT. DESC3( FILE_NAME ) ) THEN
!0711       XMSG = 'Could not get file description from ' // FILE_NAME
!0711       CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!0711    END IF

         IF ( .NOT. DESC3( SA_CONC_1 ) ) THEN
            XMSG = 'Could not get file description from ' // SA_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF


         ALLOCATE ( ISAM_BUFF( MY_NCOLS, MY_NROWS, SA_NLAYS, N_SPCTAG ),
     &     STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ISAM_BUFF'
           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         ENDIF
         ISAM_BUFF = 0.0

Ckrt     print*,'S,VNAME3D(S),S_SPCTAG(S),T_SPCTAG(S)'
         DO S = 1, N_SPCTAG
          DO L = ISAM_BLEV, ISAM_ELEV
            ISAM_BUFF( :,:,L,S ) = ISAM( :,:,L,S_SPCTAG(S),T_SPCTAG(S) )
          ENDDO
!0711     IF ( .NOT. WRITE3( FILE_NAME, VNAME3D(S), JDATE, JTIME,
          IF ( .NOT. WRITE3( SA_CONC_1, VNAME3D(S), JDATE, JTIME,
     &     ISAM_BUFF(:,:,:,S)) ) THEN
            XMSG = 'Could not write to ' 
!0711&           // FILE_NAME 
     &           // SA_CONC_1 
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF
         END DO
         DEALLOCATE( ISAM_BUFF )
      
c.....couple ISAM after writing

      return
      end

