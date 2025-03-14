
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
      SUBROUTINE WR_CGRID ( CGRID, JDATE, JTIME, TSTEP )

C Save the instantaneous state of CGRID to use for restart/continuation 
C for subsequent simulation. 

C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE MIO_ASCII
      USE RUNTIME_VARS, ONLY : RUNLEN
      use mio_module

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

      REAL, POINTER :: CGRID( :,:,:,: )
      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'WR_CGRID'
      CHARACTER( 96 ) :: XMSG = ' '
      character( 20 ) :: TIME_STAMP

      INTEGER :: K
      INTEGER, SAVE :: EDATE, ETIME

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      LOGICAL, EXTERNAL :: FLUSH3

C-----------------------------------------------------------------------

#ifndef mpas
      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

         IF ( IO_PE_INCLUSIVE ) THEN

            NVARS3D = NSPCSD
            DO K = 1, N_CGRID_SPC
               VTYPE3D( K ) = M3REAL
               VNAME3D( K ) = CGRID_NAME( K )
               UNITS3D( K ) = CGRID_UNITS( K )
               VDESC3D( K ) = 'Instantaneous value of ' // VNAME3D( K ) 
            END DO

            ! Store MIO Metadata
            NDIMS3D( 1:NVARS3D ) = 4
            L_TSTEP( 1:NVARS3D ) = .True.
            L_LAY  ( 1:NVARS3D ) = .True.
            L_COL  ( 1:NVARS3D ) = .True.
            L_ROW  ( 1:NVARS3D ) = .True.
            L_VEXT ( 1:NVARS3D ) = .False.
           
            CALL LOAD_MIO_FILE ( 'S_CGRID', NLAYS3D, 1, NLAYS3D,
     &           VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &           VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &           L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),
     &           L_VEXT(1:NVARS3D) )

         END IF ! IO_PE_INCLUSIVE
         RETURN

      END IF   ! FIRSTIME

      call mio_setfile( INIT_CONC_1 )
      call mio_fcreate( S_CGRID, mio_new_file)
      call mio_time_format_conversion( JDATE, JTIME, TIME_STAMP )
      do K = 1, n_cgrid_spc
         call mio_fwrite( S_CGRID, CGRID_NAME( K ),
     &            PNAME, CGRID( :,:,:,K ), TIME_STAMP )
      end do

      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &      'Timestep written to', S_CGRID,
     &      'for date and time', JDATE, JTIME
#endif

      RETURN 
      END
