
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
C $Header: /project/yoj/arc/CCTM/src/phot/phot_inline/phot.F,v 1.7 2011/10/21 16:11:28 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PHOT ( MDATE, MTIME, JDATE, JTIME, NDARK, RJ )

C-----------------------------------------------------------------------
C
C Function:  Calculates the photolysis rate constant to be used by the
C     chemical solver.  It calculates these rates at each gridcell using
C     codes adapted from JPROC.  Cloud correction now called within the
C     loops over MY-ROW & MY_COLS
C
C Preconditions: HGRD_INIT() called from PAR_INIT, which is called from
C     DRIVER
C
C Subroutines/Functions called: INIT3, M3EXIT, SUBHFILE, CGRID_MAP,
C     OPPHOT, SOLEFM3, GETZEN2, NEW_OPTICS
C
C Revision History.
C     Started 10/08/2004 with existing PHOT and JPROC coded by
C         Dr. Francis S. Binkowski
C         Carolina Environmental Program
C         University of North Carolina at Chapel Hill
C         email: frank_binkowski@unc.edu
C     August 2005, Sarav Arunachalam, CEP, UNC-CH
C       - Minor revisions while integrating with CMAQ
C       - Error check for NPHOTS added (this version works only for SAPRC-99)
C       - Added creation of new file CTM_RJ_1 to write out RJ values
C         for O3 and NO2 (both clear sky and cloud effects), and
C         ETOT_SFC, TAU_AERO, TAU_TOT and TAUO3_TOP values for 7 wavelengths
C     June 2007, David Wong
C       -- inline with CMAQ
C       - declare RJ as assumed shape array to match with the caller routine
C       - allow PE 0 only to open the output file
C       - output species: NO2_CLOUD and O3_CLOUD with AMISS value when all cells
C         are dark and JTIME_CHK = 0
C       - output species: NO2_CLOUD and O3_CLOUD with AMISS value when CLDATT is
C         0 and JTIME_CHK = 0
C     December 2007, Francis Binkowski
C         code has been modified to call the new on-line version that
C         has the cloud effects built in.  new photolysis routine to
C         replace PHOT in CMAQ
C     January 2008, Shawn Roselle
C       - reformatted for inclusion in CMAQ
C       - added additional 3-d photolysis rate diagnostic file
C       - moved code for opening the diagnostic files to a separate subroutine
C       - moved aerosol pointer evaluation to a FORTRAN module
C       - simplified code for writing the diagnostic file
C       - changed code to call NEW_OPTICS twice, once for clear sky and
C         another time for the cloudy fraction of the grid cell.  RJ's are
C         computed based on the cloud fraction weighting.
C      March 2011, Bill Hutzell
C       - enable wavelength dependent arrays to have an allocatable number
C         of wavelength bins
C       - added data structure and algorithm to compute a surface albedo that
C         depends on time and landuse catagory based on work by John Striecher
C         (AMAD/USEPA)
C       - revised writing to RJ1 file to include surface albedo
C       - moved photolysis and opacity data from CSQY module to an ASCII input
C         file
C       - added routine called LOAD_REF_DATA (inside the PHOT_MOD module) that i
C         reads this input file
C       - added call to a routine called AERO_PHOTDATA that returns opacity data
C         on the aerosol distribution
C       - revised NEW_OPTICS' arguments based on  aerosol redesign in CMAQ 
C         version 5.0
C     March 29, 2011 S.Roselle
C       - Replaced I/O API include files with UTILIO_DEFN
C----------------------------------------------------------------------

C...modules

!     USE GRID_CONF            ! horizontal & vertical domain specifications
      USE CGRID_SPCS           ! CGRID species number and offsets
      USE PCGRID_DEFN          ! get cgrid
      USE UTILIO_DEFN

      USE SUBST_MODULES        ! stenex
      USE AERO_DATA            ! describes aerosol distribution
      USE PHOT_MOD             ! photolysis in-line module
      USE AERO_PHOTDATA        ! arrays and routines for aerosol dimensions and refractive indices
      USE PHOTOLYSIS_ALBEDO    ! surface albedo data and routines

      IMPLICIT NONE

C...include files

      INCLUDE SUBST_FILES_ID   ! file name parameters
!     INCLUDE SUBST_CONST      ! physical constants--moved to PHOT_MOD.
      INCLUDE SUBST_RXCMMN     ! chemical mechanism reactions COMMON

C...arguments

      INTEGER MDATE            ! "centered" Julian date (YYYYDDD)
      INTEGER MTIME            ! "centered" time (HHMMSS)
      INTEGER JDATE            ! current Julian date (YYYYDDD)
      INTEGER JTIME            ! current time (HHMMSS)
!     INTEGER TSTEP            ! current timestep
      INTEGER NDARK            ! Number of level 1 cells in darkness

      REAL RJ( NCOLS, NROWS, NLAYS, NPHOTAB )
!     REAL RJ(:,:,:,:)         ! gridded J-values  (1/min units)

!     REAL CGRID( NCOLS, NROWS, NLAYS, * )  ! Conc array
      REAL, SAVE, POINTER :: CGRID( :,:,:,: ) ! species concentrations

C...parameters

      LOGICAL, PARAMETER :: CLDATT = .TRUE.  ! include cloud attenuation

      REAL, PARAMETER :: DENS_CONV    = ( 1.0E+03 * AVO / MWAIR ) * 1.0E-06  ! convert from kg/m**3 to #/cc
      REAL, PARAMETER :: PPM_MCM3     = 1.0E-06  ! convert from ppm to molecules / cc mol_Spec/mol_Air = ppm * 1E-06
      REAL, PARAMETER :: PRES_CONV    = 1.0 / STDATMPA ! conversion factor Pa to atm
      REAL, PARAMETER :: ZTOA         = 50.0E3   ! height of top of atmosphere [ m ] (=50km)
                                                 ! based a 2005 WRF model Documentation
                                                 
      REAL, PARAMETER :: EPSLON = 1.0E-30     ! Small number
                                                       
C...external functions: none

C...local variables

      LOGICAL, SAVE :: FIRSTIME = .TRUE.  ! Flag for first call to PHOT
      LOGICAL, SAVE :: PHOTDIAG           ! Flag for PHOTDIAG file
      
      LOGICAL, SAVE :: CALL_INIT_ALBEDO = .TRUE.      
      LOGICAL, SAVE :: CALL_GET_ALBEDO  = .TRUE.      

      CHARACTER(   3 ), ALLOCATABLE, SAVE :: WLTXT( : )
      CHARACTER(  16 )                    :: VARNM
      CHARACTER(  16 ), SAVE              :: PNAME = 'PHOT'
      CHARACTER(  16 ), SAVE              :: CTM_PHOTDIAG = 'CTM_PHOTDIAG'

      CHARACTER(  80 ) :: VARDESC  ! environment variable description
      CHARACTER( 240 ) :: XMSG = ' '

      INTEGER,    SAVE :: LOGDEV
      INTEGER,    SAVE :: LGC_O3    ! pointer to O3 in CGRID
      INTEGER,    SAVE :: LGC_NO2   ! pointer to NO2 in CGRID
      INTEGER,    SAVE :: TSTEP     ! current timestep

      INTEGER ESTAT              ! status from environment var check
      INTEGER JTIME_CHK          ! To check for JTIME to write RJ values
      INTEGER IPHOT              ! photolysis rate loop index
      INTEGER ROW
      INTEGER COL
      INTEGER LEV
      INTEGER SPC
      INTEGER L
      INTEGER V, N, MODE
      INTEGER ALLOCSTAT

      INTEGER, SAVE :: GXOFF, GYOFF        ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2
      INTEGER, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

      REAL CURRHR          ! current GMT hour
      REAL JULIAN_DAY      ! time of year [days]
      REAL CURRHR_LST      ! local standard time at each grid cell
      REAL CTOP            ! cloud top in single dimension
      REAL CBASE           ! cloud base in single dimension
      REAL ZLEV            ! height in single dimension
      REAL ZEN             ! cosine of zenith angle
      REAL SINLAT          ! sine of latitude
      REAL COSLAT          ! cosine of latitude
      REAL RSQD            ! square of soldist
      REAL ZSFC            ! surface height (msl) [ m ]
      REAL EQT             ! equation of time
      REAL SOLDIST         ! solar distance [ au ]
      REAL SINDEC          ! sine of the solar declination
      REAL COSDEC          ! cosine of the solar declination
      REAL COSZEN          ! working cosine of the solar zenith angle
      REAL SINZEN          ! working sine of the solar zenith angle
      REAL LATCR           ! local latitude
      REAL LONCR           ! local longitude
      REAL STOZONE         ! stratospheric ozone

      REAL,              SAVE :: JYEAR  ! year
      REAL,              SAVE :: JDSTRT ! current Julian day (DDD)

      REAL, ALLOCATABLE, SAVE :: LAT    ( :,: ) ! north lat in deg (cross pt.)
      REAL, ALLOCATABLE, SAVE :: LON    ( :,: ) ! west long in deg (cross pt.)
      REAL, ALLOCATABLE, SAVE :: HT     ( :,: ) ! ground elevation msl (meters)
      REAL, ALLOCATABLE, SAVE :: SINLATS( :,: ) ! sine of latitude
      REAL, ALLOCATABLE, SAVE :: COSLATS( :,: ) ! cosine of latitude

      REAL, ALLOCATABLE, SAVE :: ETOT_SFC ( : )     ! total downward irradiance at sfc [ Watts / m**2  ]
      REAL, ALLOCATABLE, SAVE :: TAUO3_TOP( : )     ! optical depth of ozone above model domain
      REAL, ALLOCATABLE, SAVE :: TAU_RAY  ( : )     ! Rayleigh optical depth above model domain
      REAL, ALLOCATABLE, SAVE :: TAUC_AERO( :, : )  ! aerosol optical depth at layer bottom
      REAL, ALLOCATABLE, SAVE :: TAU_TOT  ( :, : )  ! total optical depth at layer bottom
      REAL, ALLOCATABLE, SAVE :: TAU_CLOUD( :, : )  ! cloud optical depth at layer bottom

      REAL, ALLOCATABLE, SAVE :: SSA      ( : ) ! aerosol single scattering albedo, column average


      REAL MSCALE          ! combined factor to scale ppm to Molecules / cm**3
                           ! and correct for ambient temperaure and pressure

C FSB new arrays for new on-line cloud version

      REAL LWC    ( NLAYS )   ! cloud liquid water content [ g / m**3 ]
      REAL CLDFRAC( NLAYS )   ! fractional cloud cover
      REAL BLKPRS ( NLAYS )   ! Air pressure in [ Pa ]
      REAL BLKTA  ( NLAYS )   ! Air temperature [ K ]
      REAL BLKDENS( NLAYS )   ! Air density  [ molecules / m**3 ]
      REAL BLKZH  ( NLAYS )   ! layer half-height [ m ]
      REAL BLKO3  ( NLAYS )   ! O3 concentration [ molecules / cm**3 ]
      REAL BLKNO2 ( NLAYS )   ! NO2 concentration [ molecules / cm**3 ]
      REAL BLKZF  ( NLAYS+1 ) ! layer full-height [ m ]
      REAL BLKRJ_CLR( NLAYS, NPHOTAB ) ! photolysis rates
      REAL BLKRJ_CLD( NLAYS, NPHOTAB ) ! photolysis rates

      REAL WBAR   ( NCOLS, NROWS ) ! avg cloud liq water cont (g/m**3)
      REAL CLDT   ( NCOLS, NROWS ) ! cloud top, as K index
      REAL CLDB   ( NCOLS, NROWS ) ! cloud bottom, as K index
      REAL CFRAC  ( NCOLS, NROWS ) ! total fractional cloud coverage
      REAL COSZENS( NCOLS, NROWS ) ! cosines of solar zenith angles

      REAL DENS  ( NCOLS, NROWS, NLAYS ) ! air density [ Kg / m **3 ]
      REAL TA    ( NCOLS, NROWS, NLAYS ) ! Air temperature [ K ]
      REAL PRES  ( NCOLS, NROWS, NLAYS ) ! Air pressure [ Pa ]
      REAL ZM    ( NCOLS, NROWS, NLAYS ) ! layer half height agl [ m ]
      REAL ZFULL ( NCOLS, NROWS, NLAYS ) ! layer full height agl [ m ]

C...Variable for diagnostic outputs

      REAL, ALLOCATABLE, SAVE :: TOC( :, : ) ! total ozone column          
      REAL, ALLOCATABLE, SAVE :: ETOT_SFC_WL ( :,:,: ) ! total downward irradiance at sfc [ Watts / m**2  ]
      REAL, ALLOCATABLE, SAVE :: TAU_AERO_WL ( :,:,: ) ! total aerosol optical depth
      REAL, ALLOCATABLE, SAVE :: TAU_CLOUD_WL( :,:,: ) ! total cloud optical depth
      REAL, ALLOCATABLE, SAVE :: TAU_TOT_WL  ( :,:,: ) ! total optical depth
      REAL, ALLOCATABLE, SAVE :: TAUO3_TOP_WL( :,:,: ) ! optical depth of ozone above model domain
 
      REAL, ALLOCATABLE, SAVE :: AERO_SSA ( :, :, :, : ) ! aerosol single scattering albedo
      REAL, ALLOCATABLE, SAVE :: AERO_ASYM( :, :, :, : ) ! aerosol asymmetry factor
      REAL, ALLOCATABLE, SAVE :: TAU      ( :, :, :, : ) ! optical depth
      REAL, ALLOCATABLE, SAVE :: TAU_AERO ( :, :, :, : ) ! aerosol optical depth
      REAL, ALLOCATABLE, SAVE :: BUFFER   ( :, :, : )    ! IO buffer

C ----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.
         LOGDEV = INIT3()
         STDATE = JDATE
         STTIME = JTIME
         STRTHR = FLOAT( JTIME / 10000 )
         JDSTRT = FLOAT( MOD( JDATE, 1000 ) )
         JYEAR  = FLOAT( JDATE / 1000 )   !   Check this more carefully
         TSTEP  = 010000  ! output timestep for photolysis diagnostic files

         CGRID => PCGRID( 1:MY_NCOLS,1:MY_NROWS,:,: )

C...Get photolysis rate diagnostic file flag

         PHOTDIAG = .FALSE.         ! default
         VARDESC= 'Flag for writing the photolysis rate diagnostic file'
         PHOTDIAG = ENVYN( CTM_PHOTDIAG, VARDESC, PHOTDIAG, ESTAT )
         IF ( ESTAT .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( ESTAT .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         ELSE IF ( ESTAT .EQ. -1 ) THEN
            XMSG =
     &          'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         ELSE IF ( ESTAT .EQ. -2 ) THEN
            XMSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         END IF

C...Get met file offsets

         CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )
         CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )
         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

         CALL LOAD_REF_DATA ( )

C...Get latitudes

         ALLOCATE ( LAT( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating LAT'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'LAT'
         XMSG = 'Could not read LAT from ' // GRID_CRO_2D
!        IF ( .NOT. INTERP3 ( GRID_CRO_2D, VARNM, PNAME,
!    &                        JDATE, JTIME, NCOLS * NROWS, LAT ) )
         IF ( .NOT. INTERPX( GRID_CRO_2D, VARNM, PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2,
     &                       1, 1, JDATE, JTIME, LAT ) ) THEN
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. DESC3( MET_CRO_2D ) ) THEN
            XMSG = 'COULD NOT GET MET_CRO_2D  FILE DESCRIPTION '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

         
C...Allocate array needed to calculation aerosol optical properties

         CALL INIT_AERO_DATA(  )  

C...Initialize Surface albedo method
            
         IF( .NOT. INITIALIZE_ALBEDO( JDATE, JTIME, LOGDEV ) )THEN
              XMSG = 'Failure initializing photolysis surface albedo algorithm'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF 
            

C...Get longitudes

         ALLOCATE ( LON( MY_NCOLS, MY_NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating LON'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'LON'
         XMSG = 'Could not read LON from ' // GRID_CRO_2D
!        IF ( .NOT. INTERP3 ( GRID_CRO_2D, VARNM, PNAME,
!    &                        JDATE, JTIME, NCOLS * NROWS, LON ) )
         IF ( .NOT. INTERPX( GRID_CRO_2D, VARNM, PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2,
     &                       1, 1, JDATE, JTIME, LON ) ) THEN
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...Compute SINLATS and COSLATS for use in getzen2 function

         ALLOCATE ( SINLATS( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating SINLATS'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( COSLATS( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating COSLATS'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         DO COL = 1, MY_NCOLS
            DO ROW = 1, MY_NROWS
               SINLATS( COL, ROW ) = SIN( PI180 * LAT ( COL, ROW ) )
               COSLATS( COL, ROW ) = COS( PI180 * LAT ( COL, ROW ) )
            END DO
         END DO

C...get height of surface

         ALLOCATE ( HT( MY_NCOLS, MY_NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating HT'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'HT'
         XMSG = 'Could not read HT from ' // GRID_CRO_2D
!        IF ( .NOT. INTERP3 ( GRID_CRO_2D, VARNM, PNAME,
!    &                        JDATE, JTIME, NCOLS * NROWS, HT ) )
         IF ( .NOT. INTERPX( GRID_CRO_2D, VARNM, PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2,
     &                       1,1, JDATE, JTIME, HT ) ) THEN
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE( ETOT_SFC ( NWL ) )
         ALLOCATE( TAUO3_TOP( NWL ) )
         ALLOCATE( TAU_RAY  ( NWL ) )
         ALLOCATE( SSA      ( NWL ) )

         ALLOCATE( TAU_CLOUD( NLAYS, NWL ) )
         ALLOCATE( TAUC_AERO( NLAYS, NWL ) )
         ALLOCATE( TAU_TOT  ( NLAYS, NWL ) )


         IF ( PHOTDIAG ) THEN

            ALLOCATE( TOC         ( NCOLS, NROWS ) )
            ALLOCATE( ETOT_SFC_WL ( NCOLS, NROWS, NWL ) )
            ALLOCATE( TAU_AERO_WL ( NCOLS, NROWS, NWL ) )
            ALLOCATE( TAU_CLOUD_WL( NCOLS, NROWS, NWL ) )
            ALLOCATE( TAU_TOT_WL  ( NCOLS, NROWS, NWL ) )
            ALLOCATE( TAUO3_TOP_WL( NCOLS, NROWS, NWL ) )

            DIAG_WVL( 1 ) = 1
            DIAG_WVL( N_DIAG_WVL ) = NWL
            
            ALLOCATE ( AERO_ASYM ( NCOLS, NROWS, NLAYS, N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D AERO_ASYM'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( AERO_SSA ( NCOLS, NROWS, NLAYS, N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D AERO_SSA'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( TAU_AERO ( NCOLS, NROWS, NLAYS, N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D TAU_AERO'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
 
            ALLOCATE ( TAU     ( NCOLS, NROWS, NLAYS, N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D TAU'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( BUFFER  ( NCOLS, NROWS, NLAYS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating IO BUFFER'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

C...open the photolysis rate diagnostic files

            IF ( MYPE .EQ. 0 ) CALL OPPHOT ( JDATE, JTIME, TSTEP )

C...write wavelength data to a character array

            ALLOCATE ( WLTXT( NWL ) )

            DO IWL = 1, NWL
               WRITE( WLTXT( IWL ),'(I3.3)') INT( WAVELENGTH( IWL ) )
            END DO

         END IF  ! photdiag


C...set pointer to species O3 and NO2 in CGRID

         VARNM = 'O3'
         LGC_O3 = INDEX1( VARNM, N_GC_SPC, GC_SPC )
         IF ( LGC_O3 .LE. 0 ) THEN
            XMSG = 'Could not find ' // VARNM // 'in species table'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         VARNM = 'NO2'
         LGC_NO2 = INDEX1( VARNM, N_GC_SPC, GC_SPC )
         IF ( LGC_NO2 .LE. 0 ) THEN
            XMSG = 'Could not find ' // VARNM // 'in species table'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

      END IF  ! firstime


C...calculate the solar information.

      CALL SOLEFM3 ( JYEAR, JDSTRT, EQT, SINDEC, COSDEC, SOLDIST )

      RSQD = SOLDIST * SOLDIST

C...Calculate current hour in GMT and julian day

      CURRHR = STRTHR
     &       + FLOAT( SECSDIFF( STDATE, STTIME, MDATE, MTIME ) )
     &       / 3600.0

      JULIAN_DAY = FLOAT( MOD( JDATE, 1000 ) )

C...Calculate cosines of the zenith angles and compute NDARK

      NDARK = 0
      
      DO ROW = 1, MY_NROWS
         DO COL = 1, MY_NCOLS
  
            SINLAT = SINLATS( COL, ROW )
            COSLAT = COSLATS( COL, ROW )

#ifdef verbose_phot
            if( row .eq. 1 .and. col .eq. 1 )then
                write_cell = .true.
            else
                write_cell = .false.
            end if
            if( write_cell )write(logdev,*)"WRITE_CELL ACTIVATED"
#else
            write_cell = .false.
#endif            
            
C...correct  CURRHR for current *positive* West longitude convention
C...  to obtain LST.

C...this convention on longititude should be reexamined for different domains

            CURRHR_LST = CURRHR + LON( COL, ROW ) / 15.0

            CALL GETZEN2 ( CURRHR_LST , SINLAT, COSLAT,
     &                     SINDEC, COSDEC, EQT, COSZEN )
            COSZENS( COL, ROW ) = COSZEN

            IF(  COSZEN .LE. 0.0 )NDARK = NDARK + 1
            
         END DO
      END DO 

C...set surface albedos
         
      CALL GET_ALBEDO(MDATE, MTIME, LOGDEV, COSZENS, LAT, LON)     
         
C...SA  Write COSZENS array at the end of each hour

      JTIME_CHK = MOD( JTIME, 10000 )

      IF ( PHOTDIAG .AND. JTIME_CHK .EQ. 0 ) THEN

         VARNM = 'COSZENS'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE, JTIME,
     &                      COSZENS ) ) THEN
             XMSG = 'Error writing variable ' // VARNM
             CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      END IF ! JTIME_CHK .EQ. 0

C...If sun below horizon at all cells, zero photolysis rates & exit
C...  (assumes sun below horizon at *all* levels!)

      IF ( SUBST_SUM_CHK( NDARK, 'EQ', GL_NCOLS * GL_NROWS ) ) THEN

         RJ = 0.0

C...write to the log file, CTM_RJ_1 file and return

         WRITE( LOGDEV, 1003 ) JDATE, JTIME
1003     FORMAT( 8X, 'In darkness at ', I8.7, ':', I6.6,
     &           1X, 'GMT - no photolysis')

C...Initialize ETOT_SFC, TAU_AERO, TAU_TOT, TAUO3_TOP to 0.0

         TOC = 0.0

         
C...Write data to output diagnostic file

         IF ( PHOTDIAG .AND. JTIME_CHK .EQ. 0 ) THEN

            ETOT_SFC_WL  = 0.0
            TAU_AERO_WL  = 0.0
            TAU_CLOUD_WL = 0.0
            TAU_TOT_WL   = 0.0
            TAUO3_TOP_WL = 0.0

            VARNM = 'TOC'
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE, JTIME,
     &                         TOC ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'JNO2'
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE, JTIME,
     &                         RJ( :,:,1,LNO2 ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'JO3O1D'
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE, JTIME,
     &                         RJ( :,:,1,LO3O1D ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            DO IWL = 1, NWL

               VARNM = 'ETOT_SFC_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE, JTIME,
     &                            ETOT_SFC_WL( :,:,IWL ) ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'TAU_AERO_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE, JTIME,
     &                            TAU_AERO_WL( :,:,IWL ) ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'TAU_CLOUD_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE, JTIME,
     &                            TAU_CLOUD_WL( :,:,IWL ) ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'TAU_TOT_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE, JTIME,
     &                            TAU_TOT_WL( :,:,IWL ) ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'TAUO3_TOP_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE, JTIME,
     &                            TAUO3_TOP_WL( :,:,IWL ) ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'ALBEDO_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE, JTIME,
     &                            SURFACE_ALBEDO( IWL,:,: ) ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            END DO  ! iwl

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &             'RJ Values written to', CTM_RJ_1,
     &             'for date and time', JDATE, JTIME

            DO IPHOT = 1, NPHOTAB
               IF ( .NOT. WRITE3( CTM_RJ_2, PHOTAB( IPHOT ), JDATE,
     &                            JTIME, RJ( :,:,:,IPHOT ) ) ) THEN
                  XMSG = 'Could not write ' // CTM_RJ_2 // ' file'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            END DO


            BUFFER = 0.0
                                 
            DO L = 1, N_DIAG_WVL

               IWL = DIAG_WVL( L )
               
               VARNM = 'AERO_SSA_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, JDATE, JTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'AERO_ASYM_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, JDATE, JTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'TAU_AERO_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, JDATE, JTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'TAU_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, JDATE, JTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
               
            END DO

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &             'RJ and Optical Data written to', CTM_RJ_2,
     &             'for date and time', JDATE, JTIME

         END IF ! if photdiag .and. jtime_chk .eq. 0

         RETURN
      END IF  ! all cells dark

C...Get heights of each level [m]

      VARNM = 'ZH' ! midlayer height
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                    1,NLAYS, MDATE, MTIME, ZM ) ) THEN
         XMSG = 'Could not read ZH from ' // MET_CRO_3D
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      VARNM = 'ZF' ! full layer height
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                    1,NLAYS, MDATE, MTIME, ZFULL ) ) THEN
         XMSG = 'Could not read ZF from ' // MET_CRO_3D
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C...get air density [kg/m**3]

      VARNM = 'DENS'
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                    1,NLAYS,  MDATE, MTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1)
      END IF

C...get temperature [K]

      VARNM = 'TA'
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                    1,NLAYS, MDATE, MTIME, TA ) ) THEN
         XMSG = 'Could not interpolate '// VARNM // ' from MET_CRO_3D '
         CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

C...pressure [Pa]

      VARNM = 'PRES'
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3,
     &                    1, NLAYS, MDATE, MTIME, PRES ) ) THEN
         XMSG = 'Could not interpolate PRES from ' // MET_CRO_3D
         CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

C...FSB Get the cloud information

C...FSB Currently this is only for the sub-gridscale clouds.
C...  we need to add the profiles from the resolved  cloud
C...  calculations from the meteorological files

C...Read & Interpolate WBAR

      VARNM = 'WBAR'
      XMSG = 'Could not read WBAR from ' // MET_CRO_2D
!     IF ( .NOT. INTERP3 ( MET_CRO_2D, VARNM, PNAME,
!    &                     MDATE, MTIME, NCOLS * NROWS, WBAR ) )
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2,
     &                    1, 1, MDATE, MTIME, WBAR ) ) THEN
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C...Read & Interpolate CLDT

      VARNM = 'CLDT'
      XMSG = 'Could not read CLDT from ' // MET_CRO_2D
!     IF ( .NOT. INTERP3 ( MET_CRO_2D, VARNM, PNAME,
!    &                     MDATE, MTIME, NCOLS * NROWS, CLDT ) )
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2,
     &                    1, 1, MDATE, MTIME, CLDT ) ) THEN
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C...Read & Interpolate CLDB

      VARNM = 'CLDB'
      XMSG = 'Could not read CLDB from ' // MET_CRO_2D
!     IF ( .NOT. INTERP3 ( MET_CRO_2D, VARNM, PNAME,
!    &                     MDATE, MTIME, NCOLS * NROWS, CLDB ) )
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2,
     &                    1, 1, MDATE, MTIME, CLDB ) ) THEN
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C...Read & Interpolate CFRAC

      VARNM = 'CFRAC'
      XMSG = 'Could not read CFRAC from ' // MET_CRO_2D
!     IF ( .NOT. INTERP3 ( MET_CRO_2D, VARNM, PNAME,
!    &                     MDATE, MTIME, NCOLS * NROWS, CFRAC ) )
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2,
     &                    1, 1, MDATE, MTIME, CFRAC ) ) THEN
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C...MAIN loop over all rows and columns
      

      LOOP_ROWS: DO ROW = 1, MY_NROWS
         LOOP_COLS: DO COL = 1, MY_NCOLS

            COSZEN = COSZENS( COL, ROW ) ! local cosine of solar zenith angle

            IF( COSZEN .LE. 0.0 )THEN 
C...the column is dark so set variables to dark values and cycle LOOP_COLS
                RJ( COL, ROW, :, : ) = 0.0
                
                IF ( PHOTDIAG ) THEN
                   ETOT_SFC_WL ( COL, ROW, : ) = 0.0
                   TAUO3_TOP_WL( COL, ROW, : ) = 0.0
                   TAU_AERO_WL ( COL, ROW, : ) = 0.0
                   TAU_CLOUD_WL( COL, ROW, : ) = 0.0
                   TAU_TOT_WL  ( COL, ROW, : ) = 0.0
                   TAU      ( COL, ROW, :, : ) = 0.0
                   TAU_AERO ( COL, ROW, :, : ) = 0.0
                   AERO_SSA ( COL, ROW, :, : ) = 0.0
                   AERO_ASYM( COL, ROW, :, : ) = 0.0
                END IF
                
                CYCLE LOOP_COLS
                
            END IF
            
C...initialize BLKRJ using F90 array operations.

            BLKRJ_CLR = 0.0
            BLKRJ_CLD = 0.0

C...Set height of lowest level to zero

            BLKZF( 1 ) = 0.0

            ZSFC   = HT     ( COL, ROW ) ! surface height [m]
            SINZEN = SQRT( 1.0 - COSZEN * COSZEN ) ! sine of zenith angle

C...FSB now get cloud base, cloud top information

            CTOP  = CLDT( COL, ROW )
            CBASE = CLDB( COL, ROW )

C...fetch local latitude an dlongitude

            LATCR = LAT( COL, ROW )
            LONCR = LON( COL, ROW )

C...loop over vertical layers

            DO L = 1, NLAYS

C...Fetch the grid cell ambient data at each layer.

               BLKTA  ( L )   = TA   ( COL, ROW, L ) ! temperature [K]
               BLKPRS ( L )   = PRES ( COL, ROW, L ) / STDATMPA  ! [atmospheres]
               BLKDENS( L )   = DENS ( COL, ROW, L ) * DENS_CONV ! [molecules / cm**3]
               BLKZH  ( L )   = ZM   ( COL, ROW, L ) ! mid layer height [m]
               BLKZF  ( L+1 ) = ZFULL( COL, ROW, L ) ! full layer height [m]

C...set scale factor for [ppm] -> [molecule / cm**3]
C...  To go from ppm to molecule/cc:
C...  molecule/cc = ppm *  1.0E-06 * DENS (given in molecule/cc)

               MSCALE = BLKDENS( L ) * PPM_MCM3

C...fetch ozone and no2 and convert to [ molecules / cm **3 ]
C...  and adjust the volume for ambient temperature and pressure.

               BLKO3 ( L ) = CGRID( COL, ROW, L, LGC_O3  ) * MSCALE
               BLKNO2( L ) = CGRID( COL, ROW, L, LGC_NO2 ) * MSCALE

               LWC    ( L ) = 0.0
               CLDFRAC( L ) = 0.0
               ZLEV = BLKZF( L )

            END DO ! loop on layers

C..calculate needed aerosol properties in column

            CALL GET_AERO_DATA ( COL, ROW, NLAYS, CGRID )

C...calculate the clear sky photolysis rates for all layers:

#ifdef verbose_phot
            l = 1
            mode = 1
            iwl  = 1
            if(col .eq. 1 .and. row .eq. 1 .and. l .eq. 1 )then
                  write(logdev, 5005)l, mode, iwl, ae_nr_core ( 1, 1, 1 ), ae_ni_core ( 1, 1, 1 ),
     &            ae_nr_shell ( 1, 1, 1 ), ae_ni_shell ( 1, 1, 1 )
5005              format('lay = ',i3,' mode = ',i3,' wlv index = ',i3,' ae_core(nr,ni) = ',
     &            2(es12.4,1x), ' ae_shell(nr,ni) = ', 2(es12.4,1x))
            end if
#endif

            CLDFRAC = 0.0
            LWC     = 0.0

            DO IWL = 1, NWL
               ALB( IWL ) = SURFACE_ALBEDO( IWL, COL, ROW )
            END DO

             CALL NEW_OPTICS ( NLAYS, N_MODE,
     &                        BLKTA, BLKPRS, BLKDENS, BLKZH, BLKZF,
     &                        BLKO3, BLKNO2,
     &                        ZSFC, COSZEN, SINZEN, RSQD,
     &                        LATCR, LONCR, JDATE, LWC, CLDFRAC,
     &                        BLKRJ_CLR, ETOT_SFC,
     &                        TAUC_AERO, TAU_TOT, TAUO3_TOP,
     &                        TAU_RAY, SSA, TAU_CLOUD, STOZONE )

C...load diagnostic file arrays

            IF ( PHOTDIAG  ) THEN
               TOC( COL, ROW ) = STOZONE
               DO IWL = 1, NWL
                  ETOT_SFC_WL ( COL, ROW, IWL ) = ETOT_SFC ( IWL )
                  TAUO3_TOP_WL( COL, ROW, IWL ) = TAUO3_TOP( IWL )
                  TAU_AERO_WL ( COL, ROW, IWL ) = TAUC_AERO( 1, IWL )
                  TAU_TOT_WL  ( COL, ROW, IWL ) = TAU_TOT  ( 1, IWL )
               END DO
               DO L = 1, N_DIAG_WVL
                  IWL = DIAG_WVL( L )
                  DO LEV = 1, NLAYS
                     TAU     ( COL, ROW, LEV, L ) = TAU_TOT  ( LEV, IWL )
                     TAU_AERO( COL, ROW, LEV, L ) = TAUC_AERO( LEV, IWL )
                     IF( AERO_EXTI_COEF( LEV, IWL ) .GT. EPSLON )THEN
                         AERO_SSA ( COL, ROW, LEV, L ) = AERO_SCAT_COEF( LEV, IWL )
     &                                                 / AERO_EXTI_COEF( LEV, IWL )
                         AERO_ASYM( COL, ROW, LEV, L ) = AERO_ASYM_FAC( LEV, IWL )
                     ELSE
                         AERO_SSA ( COL, ROW, LEV, L ) = 1.0
                         AERO_ASYM( COL, ROW, LEV, L ) = 0.0
                     END IF
                  END DO
               END DO
            END IF

C...load cloud information

            IF ( CLDATT .AND. CFRAC( COL,ROW ) .GT. 0.0 ) THEN
               DO L = 1, NLAYS

                  ZLEV = BLKZF( L )

C...FSB check if in cloud, then set LWC and CLDFRAC profiles.

                  IF ( ZLEV .GE. CBASE .AND. ZLEV .LE. CTOP ) THEN
                     LWC    ( L ) = WBAR ( COL, ROW )
                     CLDFRAC( L ) = 1.0
                  END IF ! end cloud check

               END DO ! loop on layers

C...calculate the in-cloud photolysis rates for all layers:


             CALL NEW_OPTICS ( NLAYS, N_MODE,
     &                        BLKTA, BLKPRS, BLKDENS, BLKZH, BLKZF,
     &                        BLKO3, BLKNO2,
     &                        ZSFC, COSZEN, SINZEN, RSQD,
     &                        LATCR, LONCR, JDATE, LWC, CLDFRAC,
     &                        BLKRJ_CLD, ETOT_SFC,
     &                        TAUC_AERO, TAU_TOT, TAUO3_TOP,
     &                        TAU_RAY, SSA, TAU_CLOUD, STOZONE )


C...load diagnostic file arrays
C...compute a cloud-fraction weighted average of ETOT_SFC and TAU_TOT
C...  note that both TAUC_AERO and TAUO3_TOP are the same for clear and
C...  cloudy regions

               MSCALE = MAX( 1.0 - CFRAC( COL, ROW ), 0.0)
               
               IF ( PHOTDIAG ) THEN

                  
                  DO IWL = 1, NWL

                     ETOT_SFC_WL( COL, ROW, IWL ) = MSCALE 
     &                                            * ETOT_SFC_WL( COL, ROW, IWL )
     &                                            +  CFRAC( COL, ROW )
     &                                            * ETOT_SFC( IWL )

                     TAU_TOT_WL( COL, ROW, IWL ) = MSCALE
     &                                           * TAU_TOT_WL( COL, ROW, IWL )
     &                                           + CFRAC( COL, ROW )
     &                                           * TAU_TOT( 1, IWL )

                     TAU_CLOUD_WL ( COL, ROW, IWL ) = CFRAC( COL, ROW )
     &                                              * TAU_CLOUD( 1, IWL )

                  END DO   ! iwl
                  DO L = 1, N_DIAG_WVL
                     IWL = DIAG_WVL( L )
                     DO LEV = 1, NLAYS
                        TAU( COL, ROW, LEV, L ) = MSCALE * TAU( COL, ROW, LEV, L )
     &                                          + CFRAC( COL, ROW ) * TAU_TOT( LEV, IWL )
#ifdef verbose_pdiag
            if(col .eq. 1 .and. row .eq. 1 .and. lev .eq. 1 )then
              write(logdev,6005)MYPE,WLTXT( IWL ),(TAU( COL, ROW, LEV, L )-TAU_TOT_WL( COL, ROW, IWL )),
     &        (TAU_AERO( COL, ROW, LEV, L )-TAU_AERO_WL ( COL, ROW, IWL ))
              write(6,6005)MYPE,WLTXT( IWL ),(TAU( COL, ROW, LEV, L )-TAU_TOT_WL( COL, ROW, IWL )),
     &        (TAU_AERO( COL, ROW, LEV, L )-TAU_AERO_WL ( COL, ROW, IWL ))
            end if
6005        FORMAT('MyPe:',I3,' Lambda = ',A4,' Tau_3D-Tau_2d = ',1PE12.4,' Tau_Aero_3D-Tau_Aero_2d = ',
     &             1PE12.4)
#endif
                     END DO
                  END DO
               END IF  ! photdiag

C...set the photolysis rates based on a weighted average of the clear
C...  and cloudy fractions, and convert from 1/sec to 1/min

               DO L = 1, NLAYS
                  DO IPHOT = 1, NPHOTAB
                     RJ( COL, ROW, L, IPHOT ) = 60.0 *
     &               ( BLKRJ_CLD( L,IPHOT ) * CFRAC( COL,ROW )
     &               + BLKRJ_CLR( L,IPHOT ) * MSCALE )
                  END DO  ! Loop on PHOT
               END DO  ! Loop on layers

            ELSE ! not cldatt or cfrac = 0

C...set the photolysis rates to the clear sky values and convert
C...  from 1/sec to 1/min

               TAU_CLOUD = 0.0

               DO L = 1, NLAYS
                  DO IPHOT = 1, NPHOTAB
                     RJ( COL, ROW, L, IPHOT ) = 60.0 *
     &                                          BLKRJ_CLR( L,IPHOT )
                  END DO  ! Loop on PHOT
               END DO  ! Loop on layers

            END IF  ! cldatt and cfrac

         END DO  LOOP_COLS
      END DO  LOOP_ROWS

C...write diagnostic data to output file at the end of every hour

      IF ( PHOTDIAG .AND. JTIME_CHK .EQ. 0 ) THEN

         VARNM = 'TOC'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE, JTIME, TOC ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'JNO2'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE,
     &                      JTIME, RJ( :,:,1, LNO2 ) ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'JO3O1D'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE,
     &                      JTIME, RJ( :,:,1,LO3O1D ) ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         DO IWL = 1, NWL

            VARNM = 'ETOT_SFC_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE,
     &                         JTIME, ETOT_SFC_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'TAU_AERO_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE,
     &                         JTIME, TAU_AERO_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'TAU_CLOUD_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE,
     &                         JTIME, TAU_CLOUD_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'TAU_TOT_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE,
     &                         JTIME, TAU_TOT_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'TAUO3_TOP_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE,
     &                         JTIME, TAUO3_TOP_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'ALBEDO_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, JDATE, JTIME,
     &                         SURFACE_ALBEDO( IWL,:,: ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         END DO  ! iwl

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &          'RJ Values written to', CTM_RJ_1,
     &          'for date and time', JDATE, JTIME

         DO IPHOT = 1, NPHOTAB
            IF ( .NOT. WRITE3( CTM_RJ_2, PHOTAB( IPHOT ), JDATE,
     &                         JTIME, RJ( :,:,:,IPHOT ) ) ) THEN
               XMSG = 'Could not write ' // CTM_RJ_2 // ' file'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END DO

         DO L = 1, N_DIAG_WVL

            IWL = DIAG_WVL( L )
            
              DO LEV = 1, NLAYS
                DO ROW = 1, MY_NROWS
                  DO COL = 1, MY_NCOLS
                     BUFFER( COL, ROW, LEV ) =  AERO_SSA( COL,ROW,LEV,L )
                  END DO 
                END DO
             END DO
            
            VARNM = 'AERO_SSA_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, JDATE, JTIME, BUFFER ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

             DO LEV = 1, NLAYS
                DO ROW = 1, MY_NROWS
                  DO COL = 1, MY_NCOLS
                     BUFFER( COL, ROW, LEV ) =  AERO_ASYM( COL,ROW,LEV,L )
                  END DO 
                END DO
             END DO

            VARNM = 'AERO_ASYM_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, JDATE, JTIME, BUFFER ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

             DO LEV = 1, NLAYS
                DO ROW = 1, MY_NROWS
                  DO COL = 1, MY_NCOLS
                     BUFFER( COL, ROW, LEV ) =  TAU_AERO( COL,ROW,LEV,L )
                  END DO 
                END DO
             END DO

            VARNM = 'TAU_AERO_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, JDATE, JTIME, BUFFER ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

             DO LEV = 1, NLAYS
                DO ROW = 1, MY_NROWS
                  DO COL = 1, MY_NCOLS
                     BUFFER( COL, ROW, LEV ) =  TAU( COL,ROW,LEV,L )
                  END DO 
                END DO
             END DO

            VARNM = 'TAU_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, JDATE, JTIME, BUFFER ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            
         END DO

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &          'RJ and Optical Data written to', CTM_RJ_2,
     &          'for date and time', JDATE, JTIME

      END IF   ! if photdiag .and. jtime_chk .eq. 0

      RETURN
      END SUBROUTINE PHOT
