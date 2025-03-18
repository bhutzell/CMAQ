
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
      SUBROUTINE SA_OPWDDEP ( SADEPFILE, JDATE, JTIME, TSTEP )

C20140428  Open dry or wet deposition file (SA_DD_1 or SA_WD_1 as SADEPFILE)
C          for ISAM tracers.
C
C          Called by vdiffproc.F (dry) or initscen.F (wet)

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE UTILIO_DEFN         ! 20120712 replaces ioparms, iofdesc, iodecl
      USE SA_DEFN             ! Mc06
      USE MIO_ASCII
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif


      IMPLICIT NONE
 
      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

      CHARACTER( 16 )  SADEPFILE ! Either SA_DD_1 or SA_WD_1
      INTEGER      JDATE      ! current model date, coded YYYYDDD
      INTEGER      JTIME      ! current model time, coded HHMMSS
      INTEGER      TSTEP      ! output time step

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'OPWDDEP'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      V, L     ! loop induction variables
      INTEGER      JSPCTAG  ! loop induction for the Tags

C-----------------------------------------------------------------------

         NVARS3D = N_SPCTAG  
         NLAYS3D =     1

         IF ( SADEPFILE .EQ. SA_DD_1 ) THEN
           XMSG = ' RADM dry '
         ELSEIF ( SADEPFILE .EQ. SA_WD_1 ) THEN
           XMSG = ' wet '
         ENDIF

         DO JSPCTAG = 1, N_SPCTAG
           VTYPE3D( JSPCTAG ) = M3REAL
           VNAME3D( JSPCTAG ) = VNAM_SPCTAG( JSPCTAG )
           UNITS3D( JSPCTAG ) = 'kg/hectare'
           VDESC3D( JSPCTAG ) = 'hourly' // XMSG( 1:LEN_TRIM(XMSG) )
     &                  // 'deposition values'
           !write(*,*) VNAM_SPCTAG( JSPCTAG ),VNAME3D(JSPCTAG), UNITS3D( JSPCTAG ),VDESC3D( JSPCTAG )
         END DO

         ! Store MIO Metadata
         NDIMS3D( 1:NVARS3D ) = 4
         L_TSTEP( 1:NVARS3D ) = .True.
         L_LAY  ( 1:NVARS3D ) = .True.
         L_COL  ( 1:NVARS3D ) = .True.
         L_ROW  ( 1:NVARS3D ) = .True.
         L_VEXT ( 1:NVARS3D ) = .False.
         
         CALL LOAD_MIO_FILE ( SADEPFILE, 0, 0, 0,
     &        VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &        VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &        L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),
     &        L_VEXT(1:NVARS3D) )

      RETURN
      END
