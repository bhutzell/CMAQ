
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

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPENLAYOUT( SDATE, STIME, TSTEP, EMLAYS, VGLVSXG,
     &                       NPTGRPS, MY_NSRC, LNAME )

C-----------------------------------------------------------------------
 
C  Description:
C    Opens the diagnostic layer fractions file(s).
 
C  Preconditions:
 
C  Subroutines and Functions Called:
C       I/O API 

C Revision History:
C   16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   26 Sep 2016 D.Wong: reformatted PLAY_BASE variable as PLAY_BASE_*
C                       rather than PLAY_BASE*_

C-----------------------------------------------------------------------
C Modified from:

C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C             System
C File: @(#)$Id: openlayout.F,v 1.4 2011/10/21 16:11:30 yoj Exp $
C COPYRIGHT (C) 2002, MCNC Environmental Modeling Center
C All Rights Reserved
C See file COPYRIGHT for conditions of use.
C Environmental Modeling Center
C MCNC
C P.O. Box 12889
C Research Triangle Park, NC  27709-2889
C smoke@emc.mcnc.org
C Pathname: $Source: /project/yoj/arc/CCTM/src/plrise/smoke/openlayout.F,v $
C Last updated: $Date: 2011/10/21 16:11:30 $ 
 
C-----------------------------------------------------------------------

      USE RUNTIME_VARS, ONLY : PLAY_BASE
      USE HGRD_DEFN, ONLY: MYPE
      USE UTILIO_DEFN
      USE mio_module
      USE LOGDEV_MOD
      IMPLICIT NONE
 
C Includes:

C Local Parameters:

C Arguments:
      INTEGER,        INTENT( IN )  :: SDATE    ! Julian start date
      INTEGER,        INTENT( IN )  :: STIME    ! start time (HHMMSS)
      INTEGER,        INTENT( IN )  :: TSTEP    ! time step (HHMMSS)
      INTEGER,        INTENT( IN )  :: EMLAYS   ! number of emissions layers
      REAL,           INTENT( IN )  :: VGLVSXG( 0: )      !  vertical coord values
      INTEGER,        INTENT( IN )  :: NPTGRPS  ! number of output files
      INTEGER,        INTENT( IN )  :: MY_NSRC( : ) ! number of point sources
      CHARACTER( * ), INTENT( OUT ) :: LNAME( : )   ! layer fractions logical file name

C Local variables:

      INTEGER J, N
      CHARACTER( 256 ) :: XMSG      !  buffer for M3EXIT() messages
      CHARACTER(  16 ) :: PNAME = 'OPENLAYOUT'   ! procedure name

      CHARACTER(   4 ) :: POSTSTR = '.ncf'
      CHARACTER(  96 ) :: PLAY_EQ
      CHARACTER(  96 ) :: EQNAME
      CHARACTER( 256 ) :: BASE

      INTEGER :: IOS

C-----------------------------------------------------------------------

#ifndef mpas
C Set up and open diagnostic file(s)

      NVARS3D = 2

      VNAME3D = ' '     ! array
      VTYPE3D = 0       ! array
      UNITS3D = ''     ! array
      VDESC3D = ' '     ! array
      VNAME3D( 1 ) = 'LFRAC'
      VTYPE3D( 1 ) = M3REAL
      UNITS3D( 1 ) = '1'
      VDESC3D( 1 ) = 'Fraction of plume emitted into layer'
      VNAME3D( 2 ) = 'ZPLUME'
      VTYPE3D( 2 ) = M3REAL
      UNITS3D( 2 ) = '1'
      VDESC3D( 2 ) = 'plume elevation above ground'

!     UPNAM3D = 'DISTR_GRIDDED'   ! Mandatory!

      DO N = 1, NPTGRPS

         IF ( MY_NSRC( N ) .GT. 0 ) THEN
!           NCOLS3D = MY_NSRC( N )

            PLAY_EQ = TRIM( PLAY_BASE( N ) ) // '_' // CMYPE // POSTSTR
            WRITE( LNAME( N ),'( "PLAY_", I2.2 )' ) N
            IF ( MIO_SETENV ( LNAME( N ), PLAY_EQ ) .NE. 0 ) THEN
               XMSG = 'Could not set environment variable for ' // PLAY_EQ
               CALL M3EXIT( PNAME, SDATE, STIME, XMSG, XSTAT3 )
            END IF
! needs implementation with mio
!            IF ( .NOT. OPEN3( LNAME( N ), FSNEW3, UPNAM3D ) ) THEN
!               XMSG = 'Could not open ' // TRIM( FDESC3D( 1 ) ) // ' file'
!               CALL M3EXIT( PNAME, SDATE, STIME, XMSG, XSTAT1 )
!            END IF

         ELSE

            WRITE( LOGDEV,1009 ) N

         END IF

      END DO
#endif

      RETURN

1009  FORMAT( /5X, 'No sources in this subdomain for file sector', I2
     &        /5X, 'Therefore no plume layer fraction file created' )

      END SUBROUTINE OPENLAYOUT
