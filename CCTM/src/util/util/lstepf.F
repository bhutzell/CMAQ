
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
      SUBROUTINE LSTEPF ( FNAME, LDATE, LTIME )

C returns the last data date & time on file FNAME
C specifically used only for input files

      USE UTILIO_DEFN
      use mio_module

      IMPLICIT NONE
      
C Includes:
 
      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:

      CHARACTER( 16 ) :: FNAME
      INTEGER      LDATE        ! last file data date, coded YYYYDDD
      INTEGER      LTIME        ! last file data time, coded HHMMSS

C External Functions:
  
C local variables:

      CHARACTER( 16 ) :: PNAME = 'LSTEPF'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      LSTEP
      integer      floc, nsteps, tstep
C-----------------------------------------------------------------------

C open existing file for readonly access

#ifndef mpas
      floc = mio_search( fname )
      if (floc .le. 0 ) then
         xmsg = 'ERROR: file ' // trim(fname) // ' unknown'
         call m3exit(pname, 0, 0, xmsg, 1)
      end if
      tstep = mio_file_data(floc)%tstep
      nsteps = mio_file_data(floc)%nsteps
      LSTEP = SEC2TIME( ( nsteps - 1 ) * TIME2SEC( tstep ) )
      call mio_get_global_attr( fname, 'SDATE', LDATE )
      CALL NEXTIME( LDATE, LTIME, LSTEP )
#endif
 
      RETURN
      END
