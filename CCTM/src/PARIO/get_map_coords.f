
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

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine get_global_coords ( local_col, local_row, global_col, global_row )

      ! This subroutine returns the global column and row coordinates
      ! when given the local coordinates on the current processor.

      USE RUNTIME_VARS, ONLY : NPROCS, MYPE
      USE PIOMAPS_MODULE

      implicit none
      
      integer, intent( in ) :: local_col  ! Column on PE
      integer, intent( in ) :: local_row  ! Row on PE
      integer, intent( out) :: global_col ! Column in Full Domain
      integer, intent( out) :: global_row ! Row in Full Domain

      integer i

      if ( nprocs .eq. 1 ) then
          global_col = local_col
          global_row = local_row
      else
          i = mype + 1
          global_col = colsx_pe( 1,i ) - 1 + local_col
          global_row = rowsx_pe( 1,i ) - 1 + local_row
      end if

      end subroutine get_global_coords

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine get_local_coords ( global_col, global_row, local_col, local_row )

      ! This subroutine returns the local column and row coordinates
      ! when given the global coordinates on the current processor.

      USE RUNTIME_VARS, ONLY : NPROCS, MYPE
      USE PIOMAPS_MODULE

      implicit none
      
      integer, intent( out) :: local_col  ! Column on PE
      integer, intent( out) :: local_row  ! Row on PE
      integer, intent( in ) :: global_col ! Column in Full Domain
      integer, intent( in ) :: global_row ! Row in Full Domain

      integer i

      if ( nprocs .eq. 1 ) then
          local_col = global_col
          local_row = global_row
      else
          i = mype + 1
          local_col = global_col - colsx_pe( 1,i ) + 1
          local_row = global_row - rowsx_pe( 1,i ) + 1
      end if

      end subroutine get_local_coords 
