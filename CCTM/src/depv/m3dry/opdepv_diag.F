
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
      SUBROUTINE OPDEPV_DIAG ( JDATE, JTIME, TSTEP,
     &                         N_GDEPV_NAMES, GDEPV_NAMES,
     &                         N_ADEPV_NAMES, ADEPV_NAMES )

C 2006-Jun-30 - created by W Hutzell
C 2006-Dec-05 - modified by J.Young
C 2010-Jan-29 - removed unneccesary CLOSE3 statement for CTM_DEPV_DIAG file
C               to eliminate potential MPI race condition (D.Wong)
C 2011-Feb-16 - replaced I/O API include files with UTILIO_DEFN (S.Roselle)
C 2011-May-20 - modified diagnostic file header creation (D.Schwede)
C 2015-Aug    - added a conditional statement to allow only I/O
C                 processors to open CTM_DEPV_DIAG file (D. Wong)

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE DEPVVARS
      USE UTILIO_DEFN
      USE MIO_ASCII

      IMPLICIT NONE
 
      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

      INTEGER, INTENT( IN ) :: JDATE   ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME   ! current model time, coded HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP   ! output time step

C number and names of gas deposition velocities
      INTEGER, INTENT( IN ) :: N_GDEPV_NAMES
      CHARACTER( 16 ), INTENT( IN ) :: GDEPV_NAMES( : )

C number and names of aerosol deposition velocities
      INTEGER, INTENT( IN ) :: N_ADEPV_NAMES
      CHARACTER( 16 ), INTENT( IN ) :: ADEPV_NAMES( : )

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'OPDDEP_DIAG'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      V, N, L     ! loop induction variables

C-----------------------------------------------------------------------

#ifndef mpas
C Try to open existing file for update

            NVARS3D = N_GDEPV_NAMES + N_ADEPV_NAMES

            N = 0

            DO V = 1, N_GDEPV_NAMES
               N = N + 1
               VTYPE3D( V ) = M3REAL
               VNAME3D( V ) = GDEPV_NAMES( V )
               UNITS3D( V ) = 'cm s-1'
               VDESC3D( V ) = 'gas phase dry deposition velocity'
            END DO

            N = N_GDEPV_NAMES

            DO V = 1, N_ADEPV_NAMES
               N = N + 1
               VTYPE3D( N ) = M3REAL
               VNAME3D( N ) = ADEPV_NAMES( V )
               UNITS3D( N ) = 'cm s-1'
               VDESC3D( N ) = 'aerosol phase dry deposition velocity'
            END DO

            ! Store MIO Metadata
            NDIMS3D( 1:NVARS3D ) = 4
            L_TSTEP( 1:NVARS3D ) = .True.
            L_LAY  ( 1:NVARS3D ) = .True. ! even if 2D
            L_COL  ( 1:NVARS3D ) = .True.
            L_ROW  ( 1:NVARS3D ) = .True.
            L_VEXT ( 1:NVARS3D ) = .False.

            CALL LOAD_MIO_FILE ( CTM_DEPV_DIAG, 0, 0, 0,
     &           VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &           VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &           L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),
     &           L_VEXT(1:NVARS3D) )

#endif

      RETURN
      END
