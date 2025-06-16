
!-----------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in    !
!  continuous development by various groups and is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract, and non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of, and distribute copies!
!  of their work in the CMAQ system to the public and to permit others  !
!  to do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!


      MODULE JVAL_PARMS

C...........PARAMETERS and their descriptions
C    MXWL     = maximum number of wavelength bands to process
c    MXWLIN   = maximum number of wavelength bands on input files
C    NJ       = maximum number of vertical levels
C    MXLEV    = maximum number of vertical levels

         IMPLICIT NONE
         INTEGER, PARAMETER :: MXWL   = 200
         INTEGER, PARAMETER :: MXWLIN = 2000
         INTEGER, PARAMETER :: NJ     = 200
         INTEGER, PARAMETER :: MXLEV  = 51

      END MODULE JVAL_PARMS
