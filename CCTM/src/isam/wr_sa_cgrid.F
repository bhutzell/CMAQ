
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
C  Writes full ISAM values to a restart file SA_CGRID_1 which would serve as
C          initial conditions for next day's run
C
C  Called by driver.F
C     Revision history
C     01 Nov 2018: S.Napelenok Updates for cmaq5.3 release 
C     09 May 2019: D.Wong Removed all MY_ clauses
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      SUBROUTINE WR_SA_CGRID ( JDATE, JTIME, TSTEP )

      USE GRID_CONF
      USE UTILIO_DEFN
      USE SA_DEFN
      USE MIO_ASCII
      USE RUNTIME_VARS, ONLY : RUNLEN
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
      INTEGER    TSTEP          ! format 10000

C Local variables:
      
      INTEGER          ALLOCSTAT

      CHARACTER( 16 ) :: PNAME = 'WR_SA_CGRID'
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 20 ) :: TIME_STAMP

      INTEGER   C,R,S,K,N, L, JSPC    ! loop induction variables
      INTEGER, SAVE :: EDATE, ETIME

      REAL, ALLOCATABLE, SAVE :: ISAM_BUFF ( :,:,:,: )
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL, SAVE :: FIRSTWRITE = .TRUE.

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
        
        FIRSTIME = .FALSE.

        IF ( IO_PE_INCLUSIVE ) THEN

            NVARS3D = N_SPCTAG
            NLAYS3D = NLAYS
            VNAME3D( 1:NVARS3D ) = VNAM_SPCTAG
            UNITS3D( 1:NVARS3D ) = VUN_SPCTAG
            VTYPE3D = M3REAL
            VDESC3D = 'tracer conc.'

            ! Store MIO Metadata
            NDIMS3D( 1:NVARS3D ) = 4
            L_TSTEP( 1:NVARS3D ) = .True.
            L_LAY  ( 1:NVARS3D ) = .True.
            L_COL  ( 1:NVARS3D ) = .True.
            L_ROW  ( 1:NVARS3D ) = .True.
            L_VEXT ( 1:NVARS3D ) = .False.
      
            CALL LOAD_MIO_FILE ( SA_CGRID_1, NLAYS3D, 1, NLAYS3D,
     &           VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &           VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &           L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),
     &           L_VEXT(1:NVARS3D) )

          ENDIF ! IO_PE_INCLUSIVE

        CALL SUBST_BARRIER

        ALLOCATE ( ISAM_BUFF( NCOLS, NROWS, NLAYS, N_SPCTAG ),
     &     STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating ISAM_BUFF'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        ENDIF

        RETURN   ! Initialization of MIO File

      ENDIF  !firstime

      IF ( FIRSTWRITE ) THEN
         FIRSTWRITE = .FALSE.
         call mio_setfile( MET_CRO_3D )
         call mio_fcreate( SA_CGRID_1, io_mode )
      END IF

      call mio_time_format_conversion( JDATE, JTIME, TIME_STAMP )

      ISAM_BUFF = 0.0

      DO S = 1, N_SPCTAG
        ISAM_BUFF( :,:,:,S ) = ISAM( :,:,:,S_SPCTAG(S),T_SPCTAG(S) )
        call mio_fwrite( SA_CGRID_1, VNAM_SPCTAG(S),
     &                   PNAME, ISAM_BUFF( :,:,:,S ), TIME_STAMP )
      END DO ! S
      
      return
      end

