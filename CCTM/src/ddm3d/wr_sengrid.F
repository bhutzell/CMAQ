#ifdef sens
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      SUBROUTINE WR_SENGRID ( JDATE, JTIME, TSTEP )

C Save the instantaneous state of SENGRID to use for restart/continuation 
C for subsequent simulation. 

C Revision History:
C      Aug 08 S.L.Napelenok: initial (based on WR_CGRID)
C   09 Nov 12 S.L.Napelenok: update for cmaq 5.0.1
C   14 Oct 15 S.L.Napelenok: update for cmaq 5.1
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID species number and offsets
      USE UTILIO_DEFN
      USE RUNTIME_VARS, ONLY : RUNLEN

#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif

      USE DDM3D_DEFN
      USE MIO_ASCII
      USE MIO_MODULE

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'WR_SENGRID'
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 20 ) :: TIME_STAMP

      INTEGER K, SPC, VAR
      INTEGER ALLOCSTAT

      INTEGER, SAVE :: EDATE, ETIME
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      REAL, SAVE, ALLOCATABLE :: WRBUFF(:,:,:,:)
      CHARACTER(16), SAVE, ALLOCATABLE :: SENNAME( : ) ! for storing sensitivity species names
      INTEGER :: c, r, s, d, i

      LOGICAL, EXTERNAL :: FLUSH3

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

         ALLOCATE ( WRBUFF( NCOLS,NROWS,NLAYS,NSPCSD*NPMAX ), 
     &              SENNAME( NSPCSD*NPMAX ), STAT = ALLOCSTAT  )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'WRBUFF or SENNAME memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         EDATE = JDATE
         ETIME = JTIME
         CALL NEXTIME( EDATE, ETIME, RUNLEN )   ! end date & time

         SDATE3D = EDATE
         STIME3D = ETIME
         TSTEP3D = TSTEP

C Get SENGRID nvars

         NVARS3D = NSPCSD * NPMAX
         NLAYS3D = NLAYS

         FDESC3D = ' '
         FDESC3D( 1 ) = 'Computational grid instantaneous sensitivities'
         FDESC3D( 2 ) = '- for scenario continuation.'

         WRITE( LOGDEV,* ) ' '
         WRITE( LOGDEV,* ) '       State SENGRID File Header Description:'
         DO K = 1, 2
            WRITE( LOGDEV,* ) '    => ',
     &      TRIM( FDESC3D( K ) )
         END DO

C Create the names of the sensitivity outputs

C Gas species

         VAR = 0

         DO SPC = 1, N_GC_SPC
            SPCNAME = GC_SPC( SPC )( 1:12 )
            DO NP = 1, NPMAX
               VAR = VAR + 1
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
               UNITS3D( VAR ) = 'ppmV'
               VDESC3D( VAR ) = 'Sens of ' // TRIM(SPCNAME) // ' to ' // SEN_PAR( NP )
            END DO
         END DO

C RHOJ - should be zero

         SPCNAME = 'RHOJ'
         DO NP = 1, NPMAX
            VAR = VAR + 1
            VTYPE3D( VAR ) = M3REAL
            VNAME3D( VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
            UNITS3D( VAR ) = 'm*Kg/m**3'
            VDESC3D( VAR ) = 'Sens of ' // TRIM(SPCNAME) // ' to ' // SEN_PAR( NP )
         END DO

C Aerosol species

         DO SPC = 1, N_AE_SPC
            SPCNAME = AE_SPC( SPC )( 1:12 )
            DO NP = 1, NPMAX
               VAR = VAR + 1
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
               IF ( VNAME3D( VAR )(1:3) .EQ. 'NUM' ) THEN
                  UNITS3D( VAR ) = 'number/m**3'
               ELSE IF ( VNAME3D( VAR )(1:3) .EQ. 'SRF' ) THEN
                  UNITS3D( VAR ) = 'm**2/m**3'
               ELSE
                  UNITS3D( VAR ) = 'micrograms/m**3'
               END IF
               VDESC3D( VAR ) = 'Sens of ' // TRIM(SPCNAME) // ' to ' // SEN_PAR( NP )
            END DO
         END DO

C NR species

         DO SPC = 1, N_NR_SPC
            SPCNAME = NR_SPC( SPC )( 1:12 )
            DO NP = 1, NPMAX
               VAR = VAR + 1
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
               UNITS3D( VAR ) = 'ppmV'
               VDESC3D( VAR ) = 'Sens of ' // TRIM(SPCNAME) // ' to ' // SEN_PAR( NP )
            END DO
         END DO

c TR species

         DO SPC = 1, N_TR_SPC
            SPCNAME = TR_SPC( SPC )( 1:12 )
            DO NP = 1, NPMAX
               VAR = VAR + 1
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
               UNITS3D( VAR ) = 'ppmV'
               VDESC3D( VAR ) = 'Sens of ' // TRIM(SPCNAME) // ' to ' // SEN_PAR( NP )
            END DO
         END DO

         SENNAME = VNAME3D
           
         IF ( IO_PE_INCLUSIVE ) THEN
            ! Store MIO Metadata
            NDIMS3D( 1:NVARS3D ) = 4
            L_TSTEP( 1:NVARS3D ) = .True.
            L_LAY  ( 1:NVARS3D ) = .True.
            L_COL  ( 1:NVARS3D ) = .True.
            L_ROW  ( 1:NVARS3D ) = .True.
            L_VEXT ( 1:NVARS3D ) = .False.

!           CALL LOAD_MIO_FILE ( 'CTM_SENS_1', NLAYS3D, 1, NLAYS3D,
            CALL LOAD_MIO_FILE ( 'CTM_SENS_MIO', NLAYS3D, 1, NLAYS3D,
     &           VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &           VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &           L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),
     &           L_VEXT(1:NVARS3D) )

c open the file

            IF ( .NOT. OPEN3( CTM_SENS_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not open '
     &              // TRIM( CTM_SENS_1 )  // ' file'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         END IF   
         RETURN   ! MIO Initialization
      END IF   ! FIRSTIME

      i = 0
      do s = 1, NSPCSD
         do d = 1, NPMAX
            i = i + 1
            do k = 1, NLAYS
               do r = 1, NROWS
                  do c = 1, NCOLS
                     WRBUFF(c,r,k,i) = SENGRID(c,r,k,d,s)
                  end do
               end do
            end do
         end do
      end do

#ifdef parallel_io
      IF ( IO_PE_INCLUSIVE ) THEN
         IF ( .NOT. FLUSH3 ( CTM_SENS_1 ) ) THEN
            XMSG = 'Could not flush ' // TRIM(CTM_SENS_1)
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF
      CALL SE_BARRIER
      IF ( .NOT. IO_PE_INCLUSIVE ) THEN
         IF ( .NOT. OPEN3( CTM_SENS_1, FSNONIO, PNAME ) ) THEN
            XMSG = 'Could not open ' // TRIM(CTM_SENS_1)
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF

      IF ( .NOT. WRITE3( CTM_SENS_1, ALLVAR3, JDATE, JTIME, WRBUFF ) ) THEN
         XMSG = 'Could not write CTM_SENS_1'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
#else

      IF ( .NOT. WRITE3( CTM_SENS_1, ALLVAR3, JDATE, JTIME, WRBUFF ) ) THEN
         XMSG = 'Could not write SENGRID'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

#endif
      call mio_setfile( MET_CRO_3D )
      call mio_fcreate( 'CTM_SENS_MIO', io_mode )
      call mio_time_format_conversion( JDATE, JTIME, TIME_STAMP )
      do I = 1, NSPCSD * NPMAX 
         write(logdev,*) 'SENGRID: i, name: ', i, senname(i)
         call mio_fwrite( 'CTM_SENS_MIO', SENNAME( I ),
     &                    PNAME, WRBUFF( :,:,:,I ), TIME_STAMP )
      end do
      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &      'Timestep written to', CTM_SENS_1,
     &      'for date and time', JDATE, JTIME

      RETURN
      END

#endif
