
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


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE UNLOAD_CGRID ( CGRID )

C-----------------------------------------------------------------------
C Function:

C Revision history:
C   David Wong - Mar 17 - initial implmentation
C-----------------------------------------------------------------------

      USE CGRID_SPCS            ! CGRID mechanism species
      USE HGRD_DEFN, ONLY : NCOLS, NROWS
      USE VGRD_DEFN, ONLY : NLAYS
      USE coupler_module, ONLY : coupler_m2a_dr_3d_data, coupler_a2m_nvars

      IMPLICIT NONE

C Arguments:

      REAL, INTENT(IN)  :: CGRID( :,:,:,: )  ! for initial CONC

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER :: C, R, L, V, SPC

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
      END IF

! for GC species
      DO L = 1, NLAYS
         DO C = 1, NCOLS
            DO v = 1, N_GC_SPC
              coupler_m2a_dr_3d_data( c,1,l,v,1 ) = CGRID( C,1,L,V )
            END DO
         END DO
      END DO

! for other species
      DO L = 1, NLAYS
         DO C = 1, NCOLS
            DO v = N_GC_SPC+1, coupler_a2m_nvars
              coupler_m2a_dr_3d_data( c,1,l,v,1 ) = CGRID( C,1,L,V+1 )
            END DO
         END DO
      END DO

      END SUBROUTINE UNLOAD_CGRID
