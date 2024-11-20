
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
      SUBROUTINE WR_ACONC ( AGRID, JDATE, JTIME, TSTEP )

C Revision History:
C   Jeff - July 01
C   Note: If previous A_CONC exists, check that user hasn't changed what
C         species/layers to save (or domain).
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                        removed deprecated TRIMLEN
C   20 Jul 11 J.Young: added option for ending time timestamp
C   12 Aug 15 D.Wong:  Extracted section of code that deals with creating
C                        A_CONC_1 and put it in opaconc.F
C                      Added a section of code to allow non IO processors
C                        to open A_CONC_1
C   19 May 16 D.Wong: renamed ACONC_END_TIME to AVG_FILE_ENDTIME
C   27 Mar 17 D.Wong: removed redundant parallel_io ifdef construct
C   09 Sep 19 F. Sidi: Replaced L_ACONC_WVEL(depreciated) with W_VEL
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE AVG_CONC              ! integral average CONC
      USE UTILIO_DEFN
      USE RUNTIME_VARS
      USE MIO_MODULE
#ifndef mpas
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

      REAL      :: AGRID( :,:,:,: )
      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C Local variables:

      INTEGER      MDATE        ! modified model date, coded YYYYDDD
      INTEGER      MTIME        ! modified model time, coded HHMMSS

      CHARACTER( 16 ) :: PNAME = 'WR_ACONC'
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 20 ) :: TIME_STAMP

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL OK

      INTEGER      L, K, VAR, SPC ! loop counters
      INTEGER      STATUS

C-----------------------------------------------------------------------

C Change output date/time to starting date/time - e.g. timestamp 1995196:090000
C represents data computed from time 1995196:090000 to 1995196:100000

#ifndef mpas
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

! If not writing all layers, change dimension mio_nlays before file creation, 
! then change global attribute NLAYS to match
         call mio_setfile( MET_CRO_3D )
         if (aconc_subset_layer) mio_nlays = A_NLAYS

         call mio_fcreate( A_CONC_1, mio_new_file )

         if (aconc_subset_layer) then
            if (mype == 0) then
               call mio_set_global_attr ( A_CONC_1, 'NLAYS', A_NLAYS )
               call mio_set_global_attr ( A_CONC_1, 'VGLVLS', MET_CRO_3D, ACONC_BLEV, ACONC_ELEV+1 )
            end if
         end if
      END IF

C Override default beginning time timestamp for ACONC?
      IF ( END_TIME ) THEN   ! ending time timestamp
         MDATE = JDATE; MTIME = JTIME
      ELSE                   ! beginning time timestamp
         MDATE = JDATE; MTIME = JTIME
         CALL NEXTIME ( MDATE, MTIME, -TSTEP )
      END IF

      call mio_setfile( A_CONC_1)
      call mio_time_format_conversion( MDATE, MTIME, TIME_STAMP )

      VAR = 0
    
      DO SPC = 1, N_A_GC_SPC
         VAR = VAR + 1

         call mio_fwrite( A_CONC_1, A_GC_SPC( SPC ), PNAME, 
     &                    AGRID( :,:,:,VAR ), TIME_STAMP )       
      END DO
 
      DO SPC = 1, N_A_AE_SPC
         VAR = VAR + 1
         call mio_fwrite( A_CONC_1, A_AE_SPC( SPC ), PNAME, 
     &                    AGRID( :,:,:,VAR ), TIME_STAMP )       
      END DO
 
      DO SPC = 1, N_A_NR_SPC
         VAR = VAR + 1
         call mio_fwrite( A_CONC_1, A_NR_SPC( SPC ), PNAME, 
     &                    AGRID( :,:,:,VAR ), TIME_STAMP )       
      END DO
 
      ! Write Tracer Species Concentrations to Average Concentration File
      DO SPC = 1, N_A_TR_SPC
         VAR = VAR + 1
         call mio_fwrite( A_CONC_1, A_TR_SPC( SPC ), PNAME, 
     &                    AGRID( :,:,:,VAR ), TIME_STAMP )       
      END DO
 
      ! Write Vertical Velocity to Average Concentration File
      IF ( W_VEL ) THEN
         call mio_fwrite( A_CONC_1, 'W_VEL', PNAME, 
     &                    AVG_WVEL, TIME_STAMP )       
      END IF
 
      ! Write Relative Humidity to Average Concentration File
      IF ( L_ACONC_RH ) THEN
         call mio_fwrite( A_CONC_1, 'RH', PNAME, 
     &                    AVG_RH, TIME_STAMP )       
      END IF
      
      ! Write Temperature to Average Concentration File
      IF ( L_ACONC_TA ) THEN
         call mio_fwrite( A_CONC_1, 'TA', PNAME, 
     &                    AVG_TA, TIME_STAMP )       
      END IF
 

      ! Write Pressure to Average Concentration File
      IF (L_ACONC_PRES ) THEN
         call mio_fwrite( A_CONC_1, 'PRES', PNAME, 
     &                    AVG_PRES, TIME_STAMP )       
      END IF
 

      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &      'Timestep written to', A_CONC_1,
     &      'for date and time', MDATE, MTIME
#endif

      RETURN 
      END
