
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
      SUBROUTINE WR_AVG_SA ( JDATE, JTIME, TSTEP )

C Writes hourly averaged ISAM conc to SA_ACONC_1
C
C          Called by driver.F
C
C     01 Nov 2018: S.Napelenok Updates for cmaq5.3 release 
C     09 May 2019: D.Wong Removed all MY_ clauses

C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE UTILIO_DEFN
      USE SA_DEFN
      USE RUNTIME_VARS
      USE MIO_ASCII
      USE MIO_MODULE

#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif


      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C Local variables:

      REAL, ALLOCATABLE :: BUF4( :,:,:,: )
      INTEGER      MDATE        ! modified model date, coded YYYYDDD
      INTEGER      MTIME        ! modified model time, coded HHMMSS

      CHARACTER( 16 ) :: PNAME = 'WR_AVG_SA'
      CHARACTER( 16 ) :: ACONC_END_TIME = 'ACONC_END_TIME'
      CHARACTER( 80 ) :: VARDESC = ' '
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 20 ) :: TIME_STAMP

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL, SAVE :: FIRSTWRITE = .TRUE.
      LOGICAL OK

      INTEGER      L, K, KD, VAR, SPC ! loop counters
      INTEGER      STATUS

      INTEGER, SAVE :: A_NLAYS

      INTEGER TSTEP_RF, NTHIK_RF, NCOLS_RF, NROWS_RF, GDTYP_RF
      REAL( 8 ) :: P_ALP_RF, P_BET_RF, P_GAM_RF
      REAL( 8 ) :: XCENT_RF, YCENT_RF
      REAL( 8 ) :: XORIG_RF, YORIG_RF
      REAL( 8 ) :: XCELL_RF, YCELL_RF
      INTEGER VGTYP_RF
      REAL VGTOP_RF
C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

         IF ( AISAM_ELEV .EQ. -1 ) AISAM_ELEV = NLAYS
         A_NLAYS = AISAM_ELEV - AISAM_BLEV + 1

         IF ( MYPE .EQ. 0 ) THEN

            NVARS3D = N_SPCTAG
            NLAYS3D = A_NLAYS
            VNAME3D = VNAM_SPCTAG
            UNITS3D = VUN_SPCTAG
            VTYPE3D = M3REAL
            VDESC3D = 'tracer conc.'

            ! Store MIO Metadata
            NDIMS3D( 1:NVARS3D ) = 4
            L_TSTEP( 1:NVARS3D ) = .True.
            L_LAY  ( 1:NVARS3D ) = .True.
            L_COL  ( 1:NVARS3D ) = .True.
            L_ROW  ( 1:NVARS3D ) = .True.
            L_VEXT ( 1:NVARS3D ) = .False.
              
            CALL LOAD_MIO_FILE ( SA_ACONC_1, A_NLAYS, AISAM_BLEV, AISAM_ELEV,
     &           VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &           VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &           L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),
     &           L_VEXT(1:NVARS3D) )

         END IF   ! MYPE = 0

         RETURN    ! Initialization of MIO File

      END IF   ! FIRSTIME
    
      MDATE = JDATE; MTIME = JTIME
      IF ( .NOT. END_TIME ) THEN
         CALL NEXTIME( MDATE, MTIME, -TSTEP)
      END IF
      call mio_time_format_conversion( MDATE, MTIME, TIME_STAMP )

      IF ( FIRSTWRITE ) THEN
         FIRSTWRITE = .FALSE.
! If not writing all layers, change dimension mio_nlays before file creation, 
! then change global attribute NLAYS to match
         call mio_setfile( MET_CRO_3D )
         if ( A_NLAYS .lt. NLAYS ) mio_nlays = A_NLAYS
         call mio_fcreate( SA_ACONC_1, mio_new_file )
         if ( mype == 0 ) then
            call mio_set_global_attr( SA_ACONC_1, 'NLAYS', A_NLAYS )
            call mio_set_global_attr( SA_ACONC_1, 'VGLVLS', MET_CRO_3D, AISAM_BLEV, AISAM_ELEV+1 )
         end if
      END IF

      IF ( .NOT. ALLOCATED( BUF4 ) ) ALLOCATE( BUF4( NCOLS, NROWS, A_NLAYS, N_SPCTAG ) )
    
      DO SPC = 1, N_SPCTAG
         L = 0
         DO K = AISAM_BLEV, AISAM_ELEV
          L = L + 1
          BUF4( :,:,L,SPC ) = AISAM( :,:,L, S_SPCTAG(SPC), T_SPCTAG(SPC) )
         ENDDO !
!     write( logdev,* ) ' <>var, A_GC_SPC: ', VAR, A_GC_SPC( SPC )
         call mio_fwrite( SA_ACONC_1, VNAM_SPCTAG( SPC ),
     &                    PNAME, BUF4( :,:,:,SPC ), TIME_STAMP )
      END DO
 
      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &      'Timestep written to', SA_ACONC_1,
     &      'for date and time', MDATE, MTIME

      IF ( ALLOCATED( BUF4 ) ) DEALLOCATE( BUF4 )

      RETURN 
      END
