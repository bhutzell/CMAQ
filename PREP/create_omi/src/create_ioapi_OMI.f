
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
C $Header: /project/work/rep/arc/CCTM/src/phot/phot_inline/opphot.F,v 1.3 2011/10/21 16:11:28 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CREATE_IOAPI_OMI ( FILE_NAME, JDATE, NLAT, NLON )

C-----------------------------------------------------------------------
C
C  FUNCTION:  ioapi for OMI ozone column observation
C
C  PRECONDITIONS REQUIRED:
C     None
C
C  REVISION  HISTORY:
C       Date   Who          What
C     -------- ----------   -----------------------------------------
C-----------------------------------------------------------------------

      USE GET_ENV_VARS
      USE OUTNCF_FILE_ROUTINES

      IMPLICIT NONE


C...Arguments

      CHARACTER( 16 ), INTENT( IN ) :: FILE_NAME  ! name of file 
      INTEGER,         INTENT( IN ) :: JDATE      ! Start date of file, YYYYDDD
      INTEGER,         INTENT( IN ) :: NLAT       ! # of latitude points
      INTEGER,         INTENT( IN ) :: NLON       ! # of Longitude points

C...Local variables

      CHARACTER( 32 ), SAVE :: PNAME = 'CREATE_IOAPI_OMI'
      CHARACTER( 20 )       :: COMMAND
      CHARACTER( 96 )       :: XMSG = ' '
      INTEGER               :: TSTEP  ! output time step

      INTEGER               :: N, L, JWL, INCR   ! loop variables
      INTEGER               :: SYSTEM            ! exteral function for line commands
      LOGICAL               :: EXISTS             
C-----------------------------------------------------------------------

C...Set output file characteristics 

         CALL VALUE_NAME( FILE_NAME, file_FULL_omi%filename )
         file_FULL_omi%NCOLS = NLON
         file_FULL_omi%NROWS = NLAT
         file_FULL_omi%gdtyp_gd = 1
         file_FULL_omi%p_alp_gd = 0.0D0
         file_FULL_omi%p_bet_gd = 0.0D0
         file_FULL_omi%p_gam_gd = 0.0D0
         file_FULL_omi%XCELL_GD = REAL( 360.0 / REAL( NLON, 4 ),8 )
         file_FULL_omi%YCELL_GD = REAL( 180.0 / REAL( NLAT, 4 ),8 )
         file_FULL_omi%XORIG_GD = -180.0D0
         file_FULL_omi%YORIG_GD =  -90.0D0
         file_FULL_omi%VGTYP_GD = 7
         file_FULL_omi%VGTOP_GD = 5000.0

         file_FULL_omi%VGLVS_GD( 1 ) = 0.0
         file_FULL_omi%VGLVS_GD( 2 ) = 1.0
           
         file_FULL_omi%GDNAME_GD = 'OMI_GLOBE' 

C... Variables, Units and Descriptions for FILE_NAME

         file_FULL_omi%nfld2dxyt = 3
         
         CALL INIT_file2dxyt(file_FULL_omi)

         N = 1
         file_FULL_omi%fldname( N ) = 'OZONE_COLUMN'
         file_FULL_omi%units( N )   = 'DU'
         file_FULL_omi%long_name( N ) = 'Total Ozone Column Density'

         N = N + 1
         file_FULL_omi%fldname( N ) = 'CLOUD_FRACT'
         file_FULL_omi%units( N )   = 'None'
         file_FULL_omi%long_name( N ) = 'Radiative Cloud Fraction'

         N = N + 1
         file_FULL_omi%fldname( N ) = 'O3_MISSING'
         file_FULL_omi%units( N )   = 'None'
         file_FULL_omi%long_name( N ) = 'Whether Ozone Column Density Not Available'

         CALL file_out_ncf (outfile_2dxyt = file_FULL_omi,time_now=omi_start, sdate=0, stime=0 )

      END SUBROUTINE CREATE_IOAPI_OMI
