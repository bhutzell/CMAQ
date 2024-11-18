#ifdef sens

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
      SUBROUTINE WR_ASENS ( JDATE, JTIME, TSTEP )

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
C   26 Nov 19 S.Napelenok: create from WR_ACONC.F for cmaq-ddm3d-v5.3.1
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE AVG_CONC              ! integral average CONC
      USE UTILIO_DEFN
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif

      USE DDM3D_DEFN, ONLY: NP, NPMAX, SPCNAME, SEN_PAR, SAGRID
      USE MIO_MODULE

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C Local variables:

      INTEGER      MDATE        ! modified model date, coded YYYYDDD
      INTEGER      MTIME        ! modified model time, coded HHMMSS

      CHARACTER( 16 ) :: PNAME = 'WR_ASENS'
      CHARACTER( 16 ) :: AVG_FILE_ENDTIME = 'AVG_FILE_ENDTIME'
      CHARACTER( 80 ) :: VARDESC = ' '
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 20 ) :: TIME_STAMP

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL, SAVE :: FIRSTWRITE = .TRUE.
      LOGICAL OK

      INTEGER      L, K, VAR, SPC ! loop counters
      INTEGER      STATUS

      CHARACTER( 16 ), ALLOCATABLE, SAVE :: VARNAME_ASENS( :,: )
      INTEGER, SAVE :: N_ASENS_SPC

      Integer ALLOCSTAT, IDX


C-----------------------------------------------------------------------

C Change output date/time to starting date/time - e.g. timestamp 1995196:090000
C represents data computed from time 1995196:090000 to 1995196:100000

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3 ()

C Override default beginning time timestamp for ACONC?
         VARDESC = 'Flag to Override default beginning time timestamp for ACONC'
         END_TIME = ENVYN( AVG_FILE_ENDTIME, VARDESC, END_TIME, STATUS )
         IF ( STATUS .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         ELSE IF ( STATUS .NE. 0 ) THEN
            WRITE( LOGDEV, '(5X, A)' ) VARDESC
            XMSG = '... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         END IF

         IF ( .NOT. IO_PE_INCLUSIVE ) THEN
            IF ( .NOT. OPEN3( A_SENS_1, FSREAD3, PNAME ) ) THEN
               XMSG = 'Could not open ' // A_SENS_1 // ' file for update'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

c Create a species name list of ASENS file
         N_ASENS_SPC = N_A_GC_SPC + N_A_AE_SPC + N_A_NR_SPC + N_A_TR_SPC

         ALLOCATE( VARNAME_ASENS( NPMAX, N_ASENS_SPC ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating VARNAME_ASENS(NPMAX,N_ASENS_SPC)'
            CALL M3EXIT ( 'PNAME', 0, 0, XMSG, XSTAT2 )
         END IF
         VARNAME_ASENS = ''

         DO NP = 1, NPMAX

           DO SPC = 1, N_A_GC_SPC
             IDX = SPC
             VARNAME_ASENS( NP, IDX ) = TRIM( A_GC_SPC( SPC )( 1:12 ) )
     &                               // '_' // SEN_PAR( NP )
           END DO

           DO SPC = 1, N_A_AE_SPC
             IDX = SPC + N_A_GC_SPC
             VARNAME_ASENS( NP, IDX ) = TRIM( A_AE_SPC( SPC )( 1:12 ) )
     &                               // '_' // SEN_PAR( NP )
           END DO

           DO SPC = 1, N_A_NR_SPC
             IDX = SPC + N_A_GC_SPC + N_A_AE_SPC
             VARNAME_ASENS( NP, IDX ) = TRIM( A_NR_SPC( SPC )( 1:12 ) )
     &                               // '_' // SEN_PAR( NP )
           END DO

           DO SPC = 1, N_A_TR_SPC
             IDX = SPC + N_A_GC_SPC + N_A_AE_SPC + N_A_NR_SPC
             VARNAME_ASENS( NP, IDX ) = TRIM( A_TR_SPC( SPC )( 1:12 ) )
     &                               // '_' // SEN_PAR( NP )
           END DO

         END DO

! If not writing all layers, change dimension mio_nlays before file creation, 
! then change global attribute NLAYS to match
         call mio_setfile( MET_CRO_3D )
         if (aconc_subset_layer) mio_nlays = A_NLAYS

         call mio_fcreate( 'A_SENS_MIO', mio_new_file )

         if (aconc_subset_layer) then
            if (mype == 0) then
               call mio_set_global_attr ( 'A_SENS_MIO', 'NLAYS', A_NLAYS )
               call mio_set_global_attr ( 'A_SENS_MIO', 'VGLVLS', MET_CRO_3D, ACONC_BLEV, ACONC_ELEV+1 )
            end if
         end if

      END IF ! FIRSTIME


      IF ( END_TIME ) THEN   ! ending time timestamp
         MDATE = JDATE; MTIME = JTIME
      ELSE                   ! beginning time timestamp
         MDATE = JDATE; MTIME = JTIME
         CALL NEXTIME ( MDATE, MTIME, -TSTEP )
      END IF

      call mio_time_format_conversion( MDATE, MTIME, TIME_STAMP )

      DO SPC = 1, N_ASENS_SPC
        DO NP = 1, NPMAX
          IF ( .NOT. WRITE3( A_SENS_1, VARNAME_ASENS( NP, SPC ),
     &         MDATE, MTIME, SAGRID( :,:,:,NP,SPC ) ) ) THEN
               XMSG = 'Could not write '// VARNAME_ASENS( NP, SPC ) 
     &              // ' to ' // A_SENS_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
          call mio_fwrite( 'A_SENS_MIO', VARNAME_ASENS( NP, SPC ),
     &                     PNAME, SAGRID( :,:,:,NP,SPC ), TIME_STAMP )
         END DO
      END DO

      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &      'Timestep written to', A_SENS_1,
     &      'for date and time', MDATE, MTIME

      RETURN 
      END


#endif
