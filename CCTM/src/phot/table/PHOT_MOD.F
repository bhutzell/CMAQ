
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

C $Header$

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE PHOT_MOD


      IMPLICIT NONE
 
 
      REAL, ALLOCATABLE  :: RJ    ( :, :, :, : ) ! average grid cell J-values (min-1)
      REAL, ALLOCATABLE  :: RJ_RES( :, :, :, : ) ! resolved cloud J-values (min-1)
      REAL, ALLOCATABLE  :: RJ_SUB( :, :, :, : ) ! subgrid cloud J-values (min-1)
      REAL, ALLOCATABLE  :: CFRAC_2D_AVE( :,: )  ! layer averaged 3D fractional cloud coverage


!      REAL, ALLOCATABLE  :: ETOT_SFC_WL ( :,:,: )  ! total downward irradiance at sfc [ Watts / m**2  ]
      
!***Indices for specific photolysis rates 

      INTEGER :: LNO2            = 0 
      INTEGER :: LO3O1D          = 0
      INTEGER :: LO3O3P          = 0
      INTEGER :: LACETONE        = 0
      INTEGER :: LKETONE         = 0
      INTEGER :: LMGLY_ADJ       = 0
      INTEGER :: LMGLY_ABS       = 0
      INTEGER :: LHCHOR_06       = 0
      INTEGER :: LH2O2           = 0
      INTEGER :: LHNO3           = 0
      INTEGER :: LACETONE_CO     = 0 
      INTEGER :: LACETONE_CH3CO  = 0
      
      ! Variables passed to the PM diagnostic output module, ELMO
      REAL, ALLOCATABLE, SAVE :: ELMO_AOD_550( :,: )   ! total aerosol optical depth at 550 nm (Angstrom Interpolation)
      REAL, ALLOCATABLE, SAVE :: ELMO_EXT_550( :,:,: ) ! aerosol extinction at 550 nm for layer [1/m]

C...........PARAMETERS and their descriptions

      INTEGER, PARAMETER :: JVHT   = 13 ! number of output vert. levels
      INTEGER, PARAMETER :: JVTMAX = 13 ! number of hours output
      INTEGER, PARAMETER :: JVLAT  = 19 ! number of output latitudes

      REAL, ALLOCATABLE, SAVE :: XJVAL( :,:,:,: ) ! file jvalues
      REAL, ALLOCATABLE, SAVE :: XHAJV ( : ) ! hours from noon
      REAL, ALLOCATABLE, SAVE :: XLATJV( : ) ! latitudes of file photolytic rates
      REAL, ALLOCATABLE, SAVE :: XZJV  ( : ) ! vertical heights of file photolytic
      REAL, ALLOCATABLE, SAVE :: ACLD  ( : ) ! ! species dependent cloud albedo factor


      CONTAINS
      
      SUBROUTINE INIT_PHOT_SHARED()

          USE RXNS_DATA            ! chemistry varaibles and data
          USE GRID_CONF            ! horizontal & vertical domain specifications
          USE UTILIO_DEFN
! Purpose initialized arrays shared by other science processes      
          IMPLICIT NONE
!Arguments: None

!Local:          
          INTEGER          :: ALLOCSTAT
          INTEGER          :: JP
          CHARACTER( 240 ) :: XMSG = ' '
          LOGICAL          :: ERROR_FLAG = .FALSE.

          LOGICAL, SAVE    :: INITIALIZED = .FALSE.
          
          IF( INITIALIZED )RETURN
          
          INITIALIZED = .TRUE.

          ALLOCATE( RJ      ( NCOLS, NROWS, NLAYS, NPHOTAB ),
     &              RJ_RES  ( NCOLS, NROWS, NLAYS, NPHOTAB ),
     &              RJ_SUB  ( NCOLS, NROWS, NLAYS, NPHOTAB ), STAT = ALLOCSTAT )
          IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating photolysis rate arrays'
              CALL M3EXIT ( 'INIT_PHOT_SHARED', 0, 0, XMSG, XSTAT1 )
          END IF

!          ALLOCATE( ETOT_SFC_WL( NCOLS,NROWS,NWL ), STAT = ALLOCSTAT )
!          IF ( ALLOCSTAT .NE. 0 ) THEN
!              XMSG = 'Failure allocating irradiance rate array'
!              CALL M3EXIT ( 'INIT_PHOT_SHARED', 0, 0, XMSG, XSTAT1 )
!          END IF

          RJ = 0.0; RJ_RES = 0.0; RJ_SUB = 0.0
!          ETOT_SFC_WL = 0.0

          DO JP = 1, NPHOTAB
             SELECT CASE ( PHOTAB( JP ) )
!***check to see if this photolysis reaction is a special case.
                CASE( 'O3O3P', 'O3O3P_SAPRC99', 'O3O3P_06', 'O3_O3P_IUPAC04', 'O3O3P_NASA06', 'O3_O3P_IUPAC10' )
                    LO3O3P = JP
                CASE( 'NO2', 'NO2_SAPRC99', 'NO2_06', 'NO2_RACM2', 'NO2_IUPAC10' )
                    LNO2 = JP
                CASE( 'O3O1D',  'O3O1D_SAPRC99' , 'O3O1D_06', 'O3_O1D_IUPAC04', 'O3O1D_NASA06', 'O3_O1D_IUPAC10' )
                    LO3O1D = JP
                CASE( 'KETONE', 'KET_RACM2' )
                    LKETONE   = JP
                CASE( 'MGLY_ADJ' )
                    LMGLY_ADJ = JP
                CASE(  'MGLY_ABS' )
                    LMGLY_ABS = JP
                CASE( 'ACETONE', 'CH3COCH3_RACM2', 'ACET_IUPAC10' )
                    LACETONE  = JP
                CASE( 'HCHOR_06', 'HCHO_R_SAPRC99', 'HCHO_RAD_RACM2', 'FORM_R_IUPAC10', 'HCHO_R_MCMv32' )
                    LHCHOR_06 = JP
                CASE( 'ACET_CH3CO_CRI' )
                    LACETONE_CH3CO   = JP
                CASE( 'H2O2', 'H2O2_SAPRC99', 'H2O2_RACM2', 'H2O2_IUPAC10' )
                   LH2O2 = JP
                CASE( 'HNO3', 'HNO3_IUPAC04', 'HNO3_IUPAC10', 'HNO3_RACM2' )
                   LHNO3 = JP
              END SELECT
          END DO
           
!           IF ( LNO2   .EQ. 0 ) THEN
!              XMSG = 'NO2 photolysis not found in the mechanism! '
!              CALL M3WARN ( 'INIT_PHOT_SHARED', 0, 0, XMSG )
!           END IF
!           IF ( LO3O1D .EQ. 0 ) THEN
!              XMSG = 'O(1D) production by photolysis not found in the mechanism! '
!              CALL M3WARN ( 'INIT_PHOT_SHARED', 0, 0, XMSG )
!           END IF
!           IF ( LO3O3P .EQ. 0 ) THEN
!              MSG = 'O(3P) production by photolysis not found in the mechanism! '
!              CALL M3WARN ( 'INIT_PHOT_SHARED', 0, 0, XMSG )
!           END IF
           IF ( LH2O2 .EQ. 0 ) THEN
              XMSG = 'H2O2 photolysis not found in the mechanism! '
              ERROR_FLAG = .TRUE.
              CALL M3WARN ( 'INIT_PHOT_SHARED', 0, 0, XMSG )
           END IF
           IF ( LHNO3 .EQ. 0 ) THEN
              XMSG = 'HNO3 photolysis not found in the mechanism! '
              ERROR_FLAG = .TRUE.
              CALL M3WARN ( 'INIT_PHOT_SHARED', 0, 0, XMSG )
           END IF

          IF( ERROR_FLAG )THEN
            XMSG = 'The above fatal error(s) found initializing'
            CALL M3EXIT ( 'INIT_PHOT_SHARED', 0, 0, XMSG, XSTAT1 )
          END IF

           
          
      END SUBROUTINE INIT_PHOT_SHARED

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


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/calczen.F,v 1.5 2011/10/29 01:03:52 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)calczen.F	1.2 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.calczen.F 04 Jun 1997 10:48:01

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CALCZEN ( LAT, LONG, IDATE, GMT, ZENITH )
        
C*********************************************************************
C
C  this subroutine calculates solar zenith angle for a
C     particulat time and location.  Must specify:
C  INPUT:
c       LAT - latitude in decimal degrees
C       LONG - longitude in decimal degrees
C       IDATE - Date at Greenwich - specify year (yyyy), month (mm),
C               day format is six-digit integer:  yyyymmdd
C       GMT  - Greenwich mean time - decimal military eg.
C               22.75 = 45 min after ten pm gmt
C  OUTPUT
C       Zenith
C
C*********************************************************************

      USE CONST

      IMPLICIT NONE

C...........ARGUMENTS and their descriptions:

      INTEGER      IDATE              ! date (yyyymmdd)

      REAL         LAT                ! latitude (decimal degrees)
      REAL         LONG               ! longitude (decimal degrees)
      REAL         GMT                ! Greenwich mean time (dec.milt)
      REAL         ZENITH             ! zenith angle (degrees)
            
C...........LOCAL VARIABLES and their descriptions:

      INTEGER      IMN( 12 )          ! number of days in each month
      DATA IMN /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      SAVE         IMN

      INTEGER      IIYEAR             ! year (yyyy)
      INTEGER      LEAP               ! # leap years since 1974
      INTEGER      NYEARS             ! years since 1974
      INTEGER      NOLEAP             ! # nonleap years since 1974
      INTEGER      IJD                ! julian days (ddd)
      INTEGER      IN                 ! month pointer (mm-1)
      INTEGER      IMTH               ! month (mm)
      INTEGER      I                  ! month index
      INTEGER      IDAY               ! day (dd)
      INTEGER      JD                 ! julian days since 1974 ref
        
      REAL         LBGMT              ! 
      REAL         LZGMT              ! 
      REAL         ML                 ! geometric mean longitude (deg)
      REAL         RLT                ! latitude (radians)
      REAL         YREF               ! number of days to 1974
      REAL         YR                 ! number of days to yyyy
      REAL         D                  ! jul. days since 1974 + hh frac
      REAL         RML                ! geometric mean long (rad)
      REAL         W                  ! mean longitude of perigee (deg)
      REAL         EC                 ! eccentricity 
      REAL         EPSI               ! mean obliquity of ecliptic (deg)
      REAL         YT                 ! tan**2 (mean obl. of eclipt.)
      REAL         PEPSI              ! mean obliquity of ecliptic (rad)
      REAL         CW                 ! cosine mean long. perigee
      REAL         WR                 ! mean longitude of perigee (rad)
      REAL         SW                 ! sine mean long. perigee
      REAL         SSW                ! sine 2*mean long. perigee
      REAL         EYT                !
      REAL         FEQT               !
      REAL         FEQT1              !
      REAL         FEQT2              !
      REAL         FEQT3              !
      REAL         FEQT4              !
      REAL         FEQT5              !
      REAL         FEQT6              !
      REAL         FEQT7              !
      REAL         REQT               !
      REAL         EQT                !
      REAL         RA                 ! right ascension (degrees)
      REAL         TAB                !
      REAL         RRA                ! right ascension (radians)
      REAL         RDECL              ! declination angle (rad)
      REAL         CSZ                ! cosine (zenith angle)
      REAL         ZPT                ! 
      REAL         ZR                 ! zenith angle (radians)
      
C*********************************************************************
C     begin body of subroutine CALZEN2        

C...convert to radians

      RLT = LAT * PI180

C...parse date

      IIYEAR = IDATE / 10000
      IMTH = ( IDATE - IIYEAR * 10000 ) / 100
      IDAY = IDATE - IIYEAR * 10000 - IMTH * 100

C...identify and correct leap years

      IF ( MOD( IIYEAR, 4 ) .EQ. 0 ) THEN
         IMN( 2 ) = 29
      ELSE
         IMN( 2 ) = 28
      END IF

C...count days from Dec.31,1973 to Jan 1, YEAR, then add to 2,442,047.5

      YREF =  2442047.5
      NYEARS = IIYEAR - 1974
      LEAP = ( NYEARS + 1 ) / 4
      IF ( NYEARS .LE. -1 ) LEAP = ( NYEARS - 2 ) / 4
      NOLEAP = NYEARS - LEAP
      YR = YREF + 365.0 * NOLEAP + 366.0 * LEAP

      IJD = 0
      IN = IMTH - 1
      
      IF ( IN .EQ. 0 ) THEN
        IJD = IDAY
      ELSE IF ( IN .GT. 0 ) THEN      
        DO 101 I = 1, IN
          IJD = IJD + IMN( I )
101     CONTINUE
        IJD = IJD + IDAY
      END IF

C...print julian days current "ijd"

      JD = IJD + ( YR - YREF )
      D = JD + GMT / 24.0

C...calc geom mean longitude

      ML = 279.2801988 + 0.9856473354 * D + 2.267E-13 * D * D
      RML = ML * PI180

C...calc equation of time in sec
C... w = mean long of perigee
C... e = eccentricity
C... epsi = mean obliquity of ecliptic

      W = 282.4932328 + 4.70684E-5 * D + 3.39E-13 * D * D
      WR = W * PI180
      EC = 1.6720041E-2 - 1.1444E-9 * D - 9.4E-17 * D * D
      EPSI = 23.44266511 - 3.5626E-7 * D - 1.23E-15 * D * D
      PEPSI = EPSI * PI180
      YT = ( TAN( PEPSI / 2.0 ) )**2
      CW = COS( WR )
      SW = SIN( WR )
      SSW = SIN( 2.0 * WR )
      EYT = 2.0 * EC * YT
      FEQT1 = SIN( RML ) * ( -EYT * CW - 2.0 * EC * CW )
      FEQT2 = COS( RML ) * ( 2.0 * EC * SW - EYT * SW )
      FEQT3 = SIN( 2.0 * RML ) * ( YT - ( 5.0 * EC**2 / 4.0 ) 
     &      * ( CW**2 - SW**2 ) )
      FEQT4 = COS( 2.0 * RML ) * ( 5.0 * EC**2 * SSW / 4.0 )
      FEQT5 = SIN( 3.0 * RML ) * ( EYT * CW )
      FEQT6 = COS( 3.0 * RML ) * ( -EYT * SW )
      FEQT7 = -SIN( 4.0 * RML ) * ( 0.5 * YT**2 )
      FEQT = FEQT1 + FEQT2 + FEQT3 + FEQT4 + FEQT5 + FEQT6 + FEQT7
      EQT = FEQT * 13751.0

C...convert eq of time from sec to deg

      REQT = EQT / 240.0

C...calc right ascension in rads

      RA = ML - REQT
      RRA = RA * PI180

C...calc declination in rads, deg

      TAB = 0.43360 * SIN( RRA )
      RDECL = ATAN( TAB )

C...calc local hour angle

      LBGMT = 12.0 - EQT / 3600.0 + LONG * 24.0 / 360.0
      LZGMT = 15.0 * ( GMT - LBGMT )
      ZPT = LZGMT * PI180
      CSZ = SIN( RLT ) * SIN( RDECL ) + COS( RLT ) * COS( RDECL )
     &    * COS( ZPT )
      ZR = ACOS( CSZ )
      ZENITH = ZR / PI180
!     print*,'IDATE, IMTH, IN = ',IDATE,IMTH,IMTH-1
     
      RETURN
      END SUBROUTINE CALCZEN 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      FUNCTION CHJ ( ZETA )
      
C**********************************************************************
C
C  Chapman function is used when the solar zenith angle exceeds
C     75 deg., this is the updated RADM2 version (VERSION 89137)
C     interpolates between values given in, e.g., McCartney (1976).
C
C  Edit history:
C
C     04/08/96 - Moved mods from Sasha's new code to this routine
C     01/03/95 - Function has been completely rewritten for  -SF-
C                readability and efficiency
C
C*********************************************************************

      IMPLICIT NONE

C.......ARGUMENTS and descriptions

      REAL          ZETA        ! zenith angle (deg)
      REAL          CHJ         ! chapman function

C.......LOCAL VARIABLES and descriptions
     
      INTEGER       I           ! angle loop index
      
      REAL          RM          ! zenith angle rounded up (deg)
      
      REAL, SAVE ::  Y( 21 )     ! 
      DATA Y /   3.800,  4.055,  4.348,   4.687,   5.083, 
     &           5.551,  6.113,  6.799,   7.650,   8.732,
     &          10.144, 12.051, 14.730,  18.686,  24.905,
     &          35.466, 55.211, 96.753, 197.000, 485.000,
     &         1476.000/

C*********************************************************************
C.......begin body of function CHJ

      I = MAX( INT( ZETA ) + 1, 75 )
      RM = FLOAT( I )

      CHJ = Y( I - 75 ) +
     &    ( Y( I - 74 ) - Y( I - 75 ) ) * ( ZETA - ( RM - 1.0 ) )

      RETURN
      END FUNCTION CHJ
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      INTEGER FUNCTION FILE_UNIT ()

C FILE_UNIT uses an internal INQUIRE call to determine if a unit number is
C currently attached to a file.
C In the call INQUIRE(UNIT = IUN, NAMED = NMD), NMD is .TRUE. if unit (IUN)
C is connected to a file with a name, else it is .FALSE.

      IMPLICIT NONE

      INTEGER, PARAMETER :: LUNOUT = 6
      INTEGER, PARAMETER :: MAXUN = 99
      INTEGER, SAVE :: IUN
      LOGICAL NMD
      LOGICAL, SAVE :: AVAIL( MAXUN ) = .TRUE.   ! array
      INTEGER, SAVE :: IGO = 1

      GO TO ( 10001, 20001 ) IGO
10001 CONTINUE

C set unit numbers that are not available for I/O
      AVAIL( 1 ) = .FALSE.
      AVAIL( 5 ) = .FALSE.
      AVAIL( 6 ) = .FALSE.

C begin loop on unit numbers
      IUN = 10
101   CONTINUE
      IF ( IUN .GT. MAXUN ) GO TO 301
      INQUIRE ( UNIT = IUN, NAMED = NMD )
      IF ( NMD ) THEN
         AVAIL( IUN ) = .FALSE.
         GO TO 201
      ELSE IF ( .NOT. AVAIL( IUN ) ) THEN
         GO TO 201
      END IF
      FILE_UNIT = IUN
      AVAIL( IUN ) = .FALSE.

      IGO = 2
      RETURN
20001 CONTINUE

201   CONTINUE
      IUN = IUN + 1
      GO TO 101

301   CONTINUE
      WRITE( LUNOUT,2001 )
2001  FORMAT( / 1X, '*** ERROR ABORT in FILE_UNIT ***' /
     &       1X, 'No more unit numbers available for I/O' )
      WRITE( LUNOUT,2003 ) ( IUN, AVAIL( IUN ), IUN = 1, MAXUN )
2003  FORMAT( 1X, 'Available unit numbers are: ' /
     &       3( 1X, 20( I2, '-', L1, 2X) / ) )
      CALL M3EXIT( "FILE_UNIT", 0, 0, ":Fatal Error", 0 )
      END FUNCTION FILE_UNIT
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GET_JTABLE( JDATE, JTIME ) 
C*********************************************************************
C
C  This program calculates photolytic rate constants for
C     atmospheric molecules specified by the chemical mechanism
C     reader.  J-values are output in a TABLE format, dimensioned
C     by hour angle, latitudinal band, and vertical height.  The
C     contents of the table have been modulated by climatological
C     profiles of temperture, pressure, and ozone.  In addition,
C     if Total Ozone Column Measurements are available, the ozone
C     profile is scaled to the measured TOC value.
C
C*********************************************************************
C

#ifdef m3box
      USE GET_ENV_VARS
#else
      USE UTILIO_DEFN
#endif
      USE RXNS_DATA
      USE JVAL_PARMS
      USE CONST

      IMPLICIT NONE
      
C arguments:
      INTEGER, INTENT( IN  ) :: JDATE           ! current Julian date (YYYYDDD)
      INTEGER, INTENT( IN  ) :: JTIME           ! Greenwich mean time (HHMMSS)


      REAL,    PARAMETER :: MASS_TO_NUMBER = 1.0E-3 * AVO * MWAIR

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(16) :: PNAME  = 'GET_JTABLE'   !  driver program name
      CHARACTER(16) :: JVFILE = 'XJ_DATA' ! environment logic name of table file with photolysis frequencies
      CHARACTER(80) :: MSG    = '    '    ! buffer for messages to output
      CHARACTER(567) EQNAME               ! actual name of table file
      
      LOGICAL, SAVE :: TOMS_EXIST         = .FALSE.   ! TOMS data existence flag
      LOGICAL, SAVE :: USE_ATMOS_PROFILES = .FALSE.   ! get temperature, density, etc. from observed global profiles

      INTEGER      DAY                ! julian day of year
      INTEGER      IBASE              ! cloud base index
      INTEGER      IDATE              ! date (yyyymmdd)
      INTEGER      YEAR               ! year (yyyy)
      INTEGER      ILAT               ! latitude index
      INTEGER      ITIME              ! hour index
      INTEGER      ITOP               ! cloud top index
      INTEGER      JVUNIT             ! unit number for j-value output 
      INTEGER      JDT                ! julian date
      INTEGER      IWL                ! wavelength index
      INTEGER      ILEV               ! level index
      INTEGER      MONTH              ! month counter
      INTEGER      IHT                ! height index
      INTEGER      NLAYS              ! total # of atm layers
      INTEGER      NLEVS              ! number of levels
      INTEGER      IPHOT              ! reaction index
      INTEGER      NSUBKM             ! cloud sublayers/km
      INTEGER      NSURF              ! ground elev above sea level
      INTEGER      NWL                ! number of wavelength bands
      INTEGER      IOST               ! io status
      INTEGER      STATUS             ! status
      
! indices in latitude array to print rates to standard output 
      INTEGER, PARAMETER :: NLAT_PRINT    = 7
      INTEGER :: ILAT_PRINT( NLAT_PRINT ) = (/ 4, 6, 8, 10, 12, 14, 16 /)
      
      REAL         COSZEN             ! cosine zenith angle
      REAL         DF                 ! actinic flux
      REAL         DJ                 ! jvalue for one wl,lev,react
      REAL         DLAT               ! latitude
      REAL         DLONG              ! longitude for Pittsburgh
      REAL         GAER               ! aerosol asymetry factor
      REAL         GCLD               ! cloud asymetry factor
      REAL         GRAY               ! asymetry fact for Rayleigh scat
      REAL         HAER               ! aerosol scale ht at atm top
      REAL         HAIR               ! air scale height
      REAL         HO3                ! ozone scale height
      REAL         OMAER              ! aerosol single scat albedo
      REAL         OMCLD              ! cloud single scat cross sect
      REAL         OMRAY              ! single scat albedo, Rayleigh
      REAL         UT                 ! time
      REAL         UT0                ! local high noon
      REAL         UTNOON             ! local high noon
      REAL         TEMP_ADJUST        ! factor used to adjust temperature 
      REAL         DENS_ADJUST        ! factor used to adjust number density

      REAL         DOBNEW( JVLAT )    ! total vertical ozone column
      REAL         CLOUD( 48 )        ! cloud optical depth profile
      REAL         O3 ( MXLEV )       ! ozone profile
      REAL         T  ( MXLEV )       ! interpolated temp profile
      REAL         AIR( MXLEV )       ! interpolated air profile
      REAL         AER( MXLEV )       ! aerosol attenuation profile
      REAL         VAER( NJ )         ! aerosol column in layer
      REAL         VAIR( NJ )         ! air column in layer
      REAL         VCLD( NJ )         ! cloud column in layer
      REAL         VO3 ( NJ )         ! ozone column in layer
      REAL         VT  ( NJ )         ! average temp of column
      REAL         ZMID( NJ )         ! altitude of midpoint of layer
      REAL         Z   ( NJ )         ! altitude of each level
      REAL         CVO2( NJ )         ! vertical column O2
      REAL         MIDWL ( MXWL )     ! wavelength band midpoints
      REAL         STWL  ( MXWL )     ! wavelength band starting point
      REAL         ENDWL ( MXWL )     ! wavelength band ending point
      REAL         AAER  ( MXWL )     ! aerosol total vert opt depth
      REAL         ALBEDO( MXWL )     ! ground albedo
      REAL         ARAYL ( MXWL )     ! Rayleigh scat cross section
      REAL         F     ( MXWL )     ! extra-terrestrial radiation
      REAL         O2ABS ( MXWL )     ! O2 absorption cross section
      REAL         O3ABS ( MXWL )     ! O3 absorption cross section
      REAL         XDOBS( 19, 12 )    ! lat-season ozone values
      REAL         ENDIR( NJ, MXWL )  ! direct flux
      REAL         ENDN ( NJ, MXWL )  ! diffuse down-flux
      REAL         ENUP ( NJ, MXWL )  ! diffuse up-flux
      REAL         AO2  ( NJ, MXWL )  ! O2 cross section
      REAL         AO3  ( NJ, MXWL )  ! average O3 cross sect in layer
      REAL         D( NPHOTAB, NJ )    ! j-values
      REAL         QY( MXWL, NPHOTAB ) ! quantum yields
      REAL         CS( MXWL, NPHOTAB ) ! cross sections
      REAL         XT  ( 12, 19, MXLEV ) ! season-lat-vert temp profile (K)
      REAL         XO3 ( 12, 19, MXLEV ) ! season-lat-vert ozone profile (molecules/cm3)
      REAL         XAIR( 12, 19, MXLEV ) ! air concentration (molecules/cm3)
      REAL         QYZ( 100, MXWL, NPHOTAB )! quantum yields T&P corrected
      REAL         CSZ( 100, MXWL, NPHOTAB )! cross section at each level

!     REAL         ZENITH                 ! zenith angle
      REAL, ALLOCATABLE :: ZENITH( :,: ) ! solar zenith angles

      CHARACTER( 3 ) :: MONTHS(1:12) = (/ 
     &      'Jan','Feb','Mar','Apr','May','Jun',
     &      'Jul','Aug','Sep','Oct','Nov','Dec' /)
      CHARACTER(LEN = :), ALLOCATABLE :: SDATE


      LOGICAL, SAVE :: INITIALIZE = .TRUE.

C*********************************************************************
C     begin body of program JPROC

       
      JDT = JDATE

C...check for the existence of TOMS data

      TOMS_EXIST = .FALSE.
!     MSG = 'TOMS data exist (Y/N)'
!     TOMS_EXIST = GET_ENV_FLAG( 'JPROC_TOMSEXIST', MSG, TOMS_EXIST, STATUS )
!     IF ( STATUS .NE. 0 ) WRITE( 6, '(5X, A)' ) MSG

!     IF ( STATUS .EQ. 1 ) THEN
!       MSG = 'Environment variable improperly formatted'
!       WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
!       CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
!     ELSE IF ( STATUS .EQ. -1 ) THEN
!       MSG = 'Environment variable set, but empty ... Using default:'
!       WRITE( 6, '(5X, A, L9)' ) MSG, TOMS_EXIST
!     ELSE IF ( STATUS .EQ. -2 ) THEN
!       MSG = 'Environment variable not set ... Using default:'
!       WRITE( 6, '(5X, A, L9)' ) MSG, TOMS_EXIST
!     END IF

      IF ( .NOT. ( TOMS_EXIST ) ) THEN
        WRITE( 6, 2009 ) 
      END IF

C...convert julian date to year, month, and day
C...  and to yyyymmdd format

#ifdef m3box
      CALL JULIAN_YEAR_MONTH_DAY (JDT,YEAR,MONTH,DAY)
#else
      YEAR = INT( JDT/1000 )
      CALL DAYMON( JDT,MONTH,DAY )
#endif
      IDATE = YEAR * 10000 + MONTH * 100 + DAY

      WRITE( 6, 2005 ) TRIM( MONTHS( MONTH ) ), DAY, YEAR

      IF ( INITIALIZE ) THEN
C...read the extra terrestrial radiation data

          CALL READET ( NWL, STWL, MIDWL, ENDWL, F )

C...read the absorption cross section and quantum yield data

         CALL READCSQY ( NWL, STWL, ENDWL, CS, QY )

C...read the molecular oxygen absorption cross sections

         CALL READO2 ( NWL, STWL, ENDWL, O2ABS )

C...read the ozone absorption cross sections

         CALL READO3 ( NWL, STWL, ENDWL, O3ABS )

C...read the standard atmosphere profiles

         CALL READPROF ( XAIR, AIR, XO3, XDOBS, O3, AER, XT, T )

         INITIALIZE = .TRUE.
    
         ALLOCATE ( ZENITH( JVTMAX, JVLAT ) )

      END IF
C...read the total ozone column data

      IF ( TOMS_EXIST ) CALL READTOMS ( JDT, JVLAT, XLATJV, DOBNEW )

C...specify ground albedo

      CALL SET_ALBEDO ( NWL, MIDWL, ALBEDO )

C...cloud:  specify cloud

      CALL SETCLD ( NLAYS, NLEVS, NSUBKM, IBASE, ITOP, CLOUD,
     &              OMCLD, GCLD )

C...Specify aerosols

      CALL SETAER ( NWL, MIDWL, AAER, OMAER, GAER, HAER )

C...Specify air/Rayleigh parameters

      CALL SETAIR ( NWL, MIDWL, HAIR, OMRAY, GRAY, ARAYL )

C...nsurf:  ground elevation above sea level
C...  Here can specify different altitudes for ground surface.
C...  Currently set up so that change index of surface level:
C...  1 = SEA LEVEL
C...
C...  ***Note that the altitude associated with level index depends
C...     on whether sublayering has been done over this altitude range.
C...     So for example nsurf = 2 means 1 km above sea level
C...     if ibase > 2, but if ibase = 1 then this means 1/36 km above
C...     sea level.

      NSURF = 1

C...ozone:  specify ozone vs. height parameters
c...   HO3 = ozone scale height, used to estimate ozone density column 
C...         upper boundary (50km).
C...   DOBNEW = total vertical ozone column, in milli-cm-atm.  If want 
C...            value, must specify DOBNEW here and turn on subroutine 
C...            further below.  Otherwise: O3 profiles from main data
C...            will be used.

      HO3 = 4.50

C...compute season number
C
C...latitude and longitude:
C...   the altitude profiles of air density, temperature,
C...   and ozone depend on the geographic coordinates.  Also, these are
C...   needed to calculate the zenith angle.
C...     ILAT is index for different latitudes
C...       user must supply the real values of the latitude,
C...       DLAT = funct(ILAT) for each index ILAT.  DLAT is the real
C...       latitude in degrees, e.g.
c...                 90.0  at N-pole
C...                  0.0   at equator
C...                -90.0 at S-pole
C...     All altitude-dependent data are adjusted to the selected
C...     latitude in subroutine INTERP(DLAT,IDATE)

C...set long. for Pittsburgh

      DLONG = 80.0


C...loop for latitude

      LOOP_LAT: DO ILAT = 1, JVLAT
        DLAT = XLATJV( ILAT )

C...   interpolate to working latitude and date
C...   if subroutine INTERP is not called, code will use standard
C...   altitude pr. for ozone, air, temperature, and aerosols

        CALL INTERP ( DLAT, IDATE, XT, XAIR, XO3, XDOBS, HO3, 
     &                   T, AIR, O3 )

! adjust profiles based input temperature and mass density 
!       TEMP_ADJUST = TEMP / T(1)
!       DENS_ADJUST = MASS_TO_NUMBER * DENS / AIR(1)
!       T   = TEMP_ADJUST * T
!       AIR = DENS_ADJUST * AIR
!       O3  = DENS_ADJUST * O3 


C...invoke subroutine O3SCAL is want to set all dobson values to
C...  user-selected value DOBNEW, otherwise WMO monthly average data
C...  are used.

        IF ( TOMS_EXIST ) CALL O3SCAL ( O3, HO3, XLATJV( ILAT ),
     &                                  DOBNEW( ILAT ) )

C...subdivide atmosphere in layers
C...  subroutine SUBGRID computes all altitude dependent quantities on
C...  grid used in radiative transfer calculation.

        CALL SUBGRID ( NWL, STWL, MIDWL, ENDWL, CS, CSZ, QY, QYZ,
     &                 AIR, HAIR, VAIR, CVO2, O3ABS, O3, HO3, VO3,
     &                 AO3, IBASE, ITOP, CLOUD, NSUBKM, VCLD,
     &                 AER, VAER, HAER, T, VT, Z, ZMID,
     &                 NLAYS, NLEVS )

C...time and zenith:  specify times for calculation
C...    note that date was already specified above
C...    UT0 = Universal Time (GMT) of first calculation
C...Select starting time, for example,
C...    UTNOON = 12. + DLONG*24./360.  -> start at local high sun

        UTNOON = 12.0 + DLONG * 24.0 / 360.0
        UT0 = UTNOON
        LOOP_ANGLE: DO ITIME = 1, JVTMAX
          UT = UT0 + XHAJV( ITIME )

          CALL CALCZEN ( DLAT, DLONG, IDATE, UT, ZENITH( ITIME, ILAT ) )

C...initialize J-values

          D = 0.0

C...if nighttime, skip radiative transfer calculation

          IF ( ZENITH( ITIME, ILAT ) .LE. 95.0 ) THEN     ! begin daytime calcs

            ZENITH( ITIME, ILAT ) = MIN( ZENITH( ITIME, ILAT ),89.9 )
            COSZEN = COS( ZENITH( ITIME, ILAT ) * PI180 )
            IF ( ZENITH( ITIME, ILAT ) .GT. 75.0 ) THEN
                 COSZEN = 1.0 / CHJ( ZENITH( ITIME, ILAT ) )
                 ZENITH( ITIME, ILAT ) = ACOS( COSZEN ) 
            END IF

C...subroutine SRBAND computes effective ozone cross sections in the
C...  Schumann-Runge region, using the parameterization of Allen and
C...  Frederick

            CALL SRBAND ( NWL, STWL, MIDWL, ENDWL, COSZEN, NLAYS, AO2,
     &                    CVO2, VT, ZMID, O2ABS )

C...subroutine OPTICS is the driver for the flux calculation.  Output is
C...   ENDIR(ILEV,IWL)  - irradiance of direct solar beam
C...   ENDN (ILEV,IWL)  - irradiance of down-welling diffuse light
C...   ENUP (ILEV,IWL)  - irradiance of up-welling diffuse light
C...   for each level LEV and wavelength bin IWL.

            CALL  OPTICS ( NWL, COSZEN, ENDIR, ENDN, ENUP,
     &                     VAIR, ARAYL, GRAY, OMRAY, AO2, VO3, AO3,
     &                     VCLD, GCLD, OMCLD, VAER, AAER, GAER, OMAER,
     &                     ALBEDO, NLAYS, NLEVS, NSURF )

            
            D = 0.0
            DO IWL = 1, NWL
              DO ILEV = 1, NLEVS

C...compute the actinic flux

                DF = F( IWL ) * ( ENDIR( ILEV, IWL ) 
     &                          + ENDN ( ILEV, IWL )
     &                          + ENUP ( ILEV, IWL ) )
                IF ( ILEV .LT. NSURF ) DF = 0.0

C...compute rate of photolysis (j-values) for each reaction

                DO IPHOT = 1, NPHOTAB
                  DJ = DF * CSZ( ILEV, IWL, IPHOT )
     &                    * QYZ( ILEV, IWL, IPHOT )
                  D( IPHOT, ILEV ) = D( IPHOT, ILEV ) + DJ                  
!                 IF( DJ .GT. 10.0 )THEN
!                     WRITE(6,'(A,A,10(ES12.4,1X))')PHOTAB(IPHOT),': MIDWL,CSZ,QYZ,DF,DJ,D( IPHOT, ILEV ) = ',
!    &                MIDWL(IWL),CSZ( ILEV, IWL, IPHOT ),QYZ( ILEV, IWL, IPHOT ),
!    &                DF,DJ,D( IPHOT, ILEV )
!                 END IF
                END DO

              END DO
            END DO

          END IF     ! end daytime calculations

C...load output array and convert from 1/sec to 1/min

          DO ILEV = 1, NLEVS

            DO IHT = 1, JVHT
              IF ( FLOAT ( ( ILEV - 1 ) * 1000 ) .EQ. XZJV( IHT ) ) THEN

                DO IPHOT = 1, NPHOTAB
                  XJVAL( IPHOT, ITIME, ILAT, IHT ) = D( IPHOT, ILEV )
     &                                             * 60.0
                END DO

              END IF
            END DO

          END DO

        END DO LOOP_ANGLE 

      END DO LOOP_LAT

C...write output file with file header

#ifdef m3box
      CALL VALUE_NAME ( JVFILE, EQNAME )
#else
      CALL NAMEVAL ( JVFILE, EQNAME )
#endif

      OPEN ( NEWUNIT = JVUNIT,
     &       FILE = EQNAME,
     &       FORM = 'FORMATTED',
     &       STATUS = 'UNKNOWN',
     &       IOSTAT = IOST )

C...check for open errors

      IF ( IOST .NE. 0) THEN
        MSG = 'Could not open the JVALUE data file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

      WRITE( 6, 2011 ) JVUNIT, EQNAME

C...Write Julian Date to output ***

      WRITE ( JVUNIT, 2013 ) JDT

      WRITE( JVUNIT, 2015 ) JVHT
      WRITE( JVUNIT, 2017 ) ( XZJV( IHT ), IHT=1, JVHT )

      WRITE( JVUNIT, 2019 ) JVLAT
      WRITE( JVUNIT, 2021 ) ( XLATJV( ILAT ), ILAT=1, JVLAT )

      WRITE( JVUNIT, 2023 ) JVTMAX
      WRITE( JVUNIT, 2025 ) ( XHAJV( ITIME ), ITIME=1, JVTMAX )

      WRITE( JVUNIT, 2027) NPHOTAB

      DO IPHOT = 1, NPHOTAB
        ACLD( IPHOT ) = 1.0
        WRITE( JVUNIT, 2029 ) PHOTAB(IPHOT), ACLD( IPHOT )
      END DO

      DO IHT = 1, JVHT
        DO ILAT = 1, JVLAT
          DO IPHOT = 1, NPHOTAB

            WRITE( JVUNIT, 2031 ) IHT, ILAT, IPHOT, PHOTAB( IPHOT ),
     &     ( ZENITH( ITIME, ILAT ),ITIME = 1, JVTMAX )
            WRITE( JVUNIT, 2033 ) ( XJVAL( IPHOT, ITIME, ILAT, IHT ),
     &                               ITIME = 1, JVTMAX )

          END DO
        END DO
      END DO
      CLOSE( JVUNIT )

      print*,'calculated JTABLE '
C...formats

2001  FORMAT( 5X, A )
2005  FORMAT( 1X, '...Date to be processed: ', A3, 1X, I2, 
     &       ', ', I4, ' starting at 0:00 UT ' )
2009  FORMAT( 1X, '...No TOMS data specified for this run.', /,
     &        '   Climatological O3 column data will be used...', / )
2011  FORMAT( 1X, '...Opening File on UNIT ', I2, /, 1X, A255, / )
2013  FORMAT( 3X, I7, 2X, '(yyyyddd) Julian Date for the file' )
2015  FORMAT( 3X, I2, 2X, 'LEVELS (m)' )
2017  FORMAT( 3X, 30( F7.1, 1X ) )
2019  FORMAT( 3X, I2, 2X, 'LATITUDES (deg)' )
2021  FORMAT( 3X, 30( F5.1, 1X ) )
2023  FORMAT( 3X, I2, 2X, 'HOUR ANGLES (from noon)' )
2025  FORMAT( 3X, 30( F5.1, 1X ) )
2027  FORMAT( 3X, I2, 2X, 'PHOTOLYTIC REACTIONS' )
2029  FORMAT( 6X, '''', A16, ''',', 5X, F3.1 )
2031  FORMAT( 1X, 3( I3, 1X ), A16, " ZENITH = ", 100(1X,F9.4) )
2033  FORMAT( 1X, 1P, 5( E16.7, 2X ) )
2034  FORMAT('At Z = ', F6.2, A )     
2035  FORMAT(A16,10(1X,F12.2))
2036  FORMAT(A16,10(1X,ES12.4))


      END SUBROUTINE GET_JTABLE
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      INTEGER FUNCTION INDEX2 (NAME1, N, NAME2)     

C***********************************************************************
C
C  FUNCTION:
C
C    This routine searches for NAME1 in list NAME2    
C
C  REVISION HISTORY:
C
C    5/88   Modified for ROMNET
C
C  ARGUMENT LIST DESCRIPTION:
C
C    Input arguments:
C
C      NAME1       Character string being searched for
C      N           Length of array to be searched
C      NAME2       Character array to be searched
C
C    Output arguments:
C
C      INDEX1      The position within the NAME2 array that NAME1 
C                  found.  If string was not found, INDEX1 = 0
C
C  LOCAL VARIABLE DESCRIPTION:
C
C      None
C
C***********************************************************************

      IMPLICIT NONE

      INTEGER       N
      INTEGER       I

      CHARACTER*(*) NAME1
      CHARACTER*(*) NAME2(*)          

C...Assume NAME1 is not in list NAME2    

      INDEX2 = 0

      DO I = 1, N
        IF ( INDEX( NAME2( I ), NAME1 ) .EQ. 1 ) THEN
          INDEX2 = I
          RETURN     
        ENDIF
      END DO 

      RETURN
      END FUNCTION INDEX2
      SUBROUTINE INTAVG ( WLIN, CQIN, NWLIN, TYPE,
     &                    WLOUT1, WLOUT2, CQOUT, NWLOUT )
     
C*********************************************************************
C
C  This program computes the integrated average data for the ET
C    extra terrestrial irradiance wavelength intervals
C
C  History:
C    S.Roselle  6/05/95  Program created
C    S.Roselle  7/25/96  Revised subroutine to compute interval
C                        average quantities for point, centered,
C                        beginning, and ending data
C
C*********************************************************************

      USE  JVAL_PARMS

      IMPLICIT NONE      
      

C...........ARGUMENTS and their descriptions

      CHARACTER(1) :: TYPE             ! spectra type

      INTEGER, INTENT( IN ) :: NWLOUT              ! number of intervals ETin
      INTEGER, INTENT( IN ) :: NWLIN               ! number of intervals CQin

      REAL, INTENT( IN )  :: WLIN ( : )     ! wl for CQin
      REAL, INTENT( IN )  :: CQIN( : )      ! quantity (CS or QY) as f(WLIN)
      REAL, INTENT( IN )  :: WLOUT1( : )      ! lower limit on wl int ETin
      REAL, INTENT( IN )  :: WLOUT2( : )      ! upper limit on wl int ETin
      REAL, INTENT( OUT ) :: CQOUT ( : )      ! quantity (CS or QY) as f(WLOUT)

C...........LOCAL VARIABLES and their descriptions:
      
      CHARACTER(16) :: PNAME = 'INTAVG' ! program name
      CHARACTER(80) :: MSG   = '    '   ! message

      INTEGER      I                   ! index var
      INTEGER      J                   ! index var
      INTEGER      MXWLPT              ! pointer
      INTEGER      MNWLPT              ! pointer
     
      REAL         DWLIN               ! wl int for CQin
      REAL         CQA                 ! lower cq value
      REAL         CQB                 ! upper cq value
      REAL         WLIN1( NWLIN  )     ! lower limit on wl int CQin
      REAL         WLIN2( NWLIN  )     ! upper limit on wl int CQin
      
C*********************************************************************
C...begin body of subroutine INTAVG

C...process point data

      IF ( TYPE .EQ. 'P' ) THEN

C...transform the data to the same wavelength intervals as
C...  the irradiance data.

C...loop through wavelength intervals for output arrays

        DO I = 1, NWLOUT

C...find lower limit on input array

          MNWLPT = 1
          DO J = 1, NWLIN
            IF ( WLIN( J ) .LE. WLOUT1( I ) ) MNWLPT = J
          END DO

C...find upper limit on input array

          MXWLPT = NWLIN
          DO J = NWLIN, 1, -1
            IF ( WLIN( J ) .GE. WLOUT2( I ) ) MXWLPT = J
          END DO

C...initialize output arrays

          CQOUT( I ) = 0.0

C...loop through the valid wavelength intervals

          DO J = MNWLPT, MXWLPT 

C...determine weighting fraction for the wavelength interval

            IF ( WLOUT1( I ) .LE. WLIN( J ) ) THEN
            
              IF ( WLOUT2( I ) .EQ. WLIN( J+1 ) ) THEN
                DWLIN = WLIN( J+1 ) - WLIN( J )
                CQA = CQIN( J )
                CQB = CQIN( J+1 )
              ELSE IF ( WLOUT2( I ) .LT. WLIN( J+1 ) ) THEN
                DWLIN = WLOUT2( I ) - WLIN( J )
                CQA = CQIN( J )
                CQB = ( CQIN( J+1 ) - CQIN( J ) )
     &              / ( WLIN( J+1 ) - WLIN( J ) )
     &              * ( WLOUT2( I ) - WLIN( J ) ) + CQIN( J )
              ELSE IF ( WLOUT2( I ) .GT. WLIN( J+1 ) ) THEN
                DWLIN = WLIN( J+1 ) - WLIN( J )
                CQA = CQIN( J )
                CQB = CQIN( J+1 )
              END IF
            
            ELSE IF ( WLOUT1( I ) .GT. WLIN( J ) ) THEN

              IF ( WLOUT2( I ) .EQ. WLIN( J+1 ) ) THEN
                DWLIN = WLIN( J+1 ) - WLOUT1( I )
                CQA = ( CQIN( J+1 ) - CQIN( J ) )
     &              / ( WLIN( J+1 ) - WLIN( J ) )
     &              * ( WLOUT1( I ) - WLIN( J ) ) + CQIN( J )
                CQB = CQIN( J+1 )
              ELSE IF ( WLOUT2( I ) .LT. WLIN( J+1 ) ) THEN
                DWLIN = WLOUT2( I ) - WLOUT1( I )
                CQA = ( CQIN( J+1 ) - CQIN( J ) )
     &              / ( WLIN( J+1 ) - WLIN( J ) )
     &              * ( WLOUT1( I ) - WLIN( J ) ) + CQIN( J )
                CQB = ( CQIN( J+1 ) - CQIN( J ) )
     &              / ( WLIN( J+1 ) - WLIN( J ) )
     &              * ( WLOUT2( I ) - WLIN( J ) ) + CQIN( J )
              ELSE IF ( WLOUT2( I ) .GT. WLIN( J+1 ) ) THEN
                DWLIN = WLIN( J+1 ) - WLOUT1( I )
                CQA = ( CQIN( J+1 ) - CQIN( J ) )
     &              / ( WLIN( J+1 ) - WLIN( J ) )
     &              * ( WLOUT1( I ) - WLIN( J ) ) + CQIN( J )
                CQB = CQIN( J+1 )
              END IF
              
            END IF

C...set wavelength interval fraction

            DWLIN = AMAX1 ( DWLIN / ( WLOUT2( I ) - WLOUT1( I ) ), 0.0 )

C...add weighted quantity to output arrays

            CQOUT( I ) = CQOUT( I ) + 0.5 * ( CQA + CQB ) * DWLIN

          END DO

        END DO

C...process interval data

      ELSE

C...determine wl intervals for CENTERED WLBAND data

        IF ( TYPE .EQ. 'C' ) THEN

          WLIN1( 1 ) = 0.5 * (( 3.0 * WLIN( 1 ) ) -  WLIN( 2 ))
          WLIN2( 1 ) = 0.5 * (WLIN( 1 ) + WLIN( 2 ))

          DO J = 2, NWLIN-1
            WLIN1( J ) = 0.5 * (WLIN( J ) + WLIN ( J-1 ))
            WLIN2( J ) = 0.5 * (WLIN( J ) + WLIN ( J+1 ))
          END DO

C...determine wl intervals for BEGINNING WLBAND data

        ELSE IF ( TYPE .EQ. 'B' ) THEN

          DO J = 1, NWLIN-1
            WLIN1( J ) = WLIN( J )
            WLIN2( J ) = WLIN( J+1 )
          END DO

C...determine wl intervals for ENDING WLBAND data

        ELSE IF ( TYPE .EQ. 'E' ) THEN

          DO J = 2, NWLIN
            WLIN1( J-1 ) = WLIN( J-1 )
            WLIN2( J-1 ) = WLIN( J )
          END DO

C...stop program if wavelength data type not found

        ELSE

          MSG = 'Unrecognized spectra type in CSQY file '
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )

        END IF

C...loop through wavelength intervals for output arrays

        DO I = 1, NWLOUT

C...find lower limit on input array
      
          MNWLPT = 1        
          DO J = 1, NWLIN-1
            IF ( WLIN1( J ) .LE. WLOUT1( I ) ) MNWLPT = J
          END DO

C...find upper limit on input array

          MXWLPT = NWLIN-1
          DO J = NWLIN-1, 1, -1
            IF ( WLIN2( J ) .GE. WLOUT2( I ) ) MXWLPT = J
          END DO

C...initialize output arrays

          CQOUT( I ) = 0.0

C...loop through the valid wavelength intervals

          DO J = MNWLPT, MXWLPT 

C...determine weighting fraction for the wavelength interval

            IF ( WLOUT1( I ) .LE. WLIN1( J ) ) THEN
            
              IF ( WLOUT2( I ) .EQ. WLIN2( J ) ) THEN
                DWLIN = WLIN2( J ) - WLIN1( J )
              ELSE IF ( WLOUT2( I ) .LT. WLIN2( J ) ) THEN
                DWLIN = WLOUT2( I ) - WLIN1( J )
              ELSE IF ( WLOUT2( I ) .GT. WLIN2( J ) ) THEN
                DWLIN = WLIN2( J ) - WLIN1( J )
              END IF
            
            ELSE IF ( WLOUT1( I ) .GT. WLIN1( J ) ) THEN

              IF ( WLOUT2( I ) .EQ. WLIN2( J ) ) THEN
                DWLIN = WLIN2( J ) - WLOUT1( I )
              ELSE IF ( WLOUT2( I ) .LT. WLIN2( J ) ) THEN
                DWLIN = WLOUT2( I ) - WLOUT1( I )
              ELSE IF ( WLOUT2( I ) .GT. WLIN2( J ) ) THEN
                DWLIN = WLIN2( J ) - WLOUT1( I )
              END IF
              
            END IF

C...set wavelength interval fraction

            DWLIN = AMAX1 ( DWLIN / ( WLOUT2( I ) - WLOUT1( I ) ), 0.0 )

C...add weighted quantity to output arrays

            CQOUT( I ) = CQOUT( I ) + CQIN( J ) * DWLIN

          END DO

        END DO

      END IF
     
      RETURN
      END SUBROUTINE INTAVG
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE INTERP ( DLAT, IDATE, XT, XAIR, XO3, XDOBS, HO3, 
     &                    T, AIR, O3 )

C*********************************************************************
C
C     FUNCTION:  Interpolates data for T, AIR, and O3 to the selected
C     day of the year (from seasonal averages or monthly averages)
C     and to the selected latitude (data at 90N -> 90S by 10 deg incre
C     Note:  latitude dependent data is indexed 1 through 19:
C                 (1 = 90N, 10 = Equator, 19 = 90S).
C            seasonal data is indexed 1 through 4:
c                 1 = winter, 2 = spring, 3 = summer, 4 = fall
C            monthly data is indexed 1=Jan, 2=Feb,...,12=Dec.
c     Output of this subroutine is: Molecular # density, Ozone, and
C     temperature interpolated to date and latitude of interest:
C     AIR(I), T(I), O3.  Also, the O3 data can also be re-scaled to
C     a different value of total ozone (DOBSON).
C     
C     PRECONDITIONS REQUIRED:  
C     
C     REVISION  HISTORY:
C     5/12/95  s.roselle  modified old radm version for new coding std
C
C*********************************************************************

      USE  JVAL_PARMS

      IMPLICIT NONE

C...........ARGUMENTS and their descriptions

      INTEGER, INTENT( IN ) :: IDATE          ! date (yyyymmdd)
      REAL, INTENT( IN )    :: DLAT          ! latitude interval
      REAL, INTENT( IN )    :: XT  ( :,:,: ) ! season-lat-vert temp profile
      REAL, INTENT( IN )    :: XAIR( :,:,: ) ! season-lat-vert air profile
      REAL, INTENT( IN )    :: XO3 ( :,:,: ) ! season-lat-vert ozone profile
      REAL, INTENT( IN )    :: XDOBS( :,: )  ! lat-season ozone values
      REAL, INTENT( IN )    :: HO3           ! ozone scale height

      REAL, INTENT( OUT ) :: T  ( : )      ! interpolated temp profile
      REAL, INTENT( OUT ) :: AIR( : )      ! interpolated air profile
      REAL, INTENT( OUT ) :: O3 ( : )      ! interpolated ozone profile

C...........LOCAL VARIABLES and their descriptions:

      INTEGER      IYEAR             ! year (yyyy)
      INTEGER      IDAY              ! day (dd)
      INTEGER      IMONTH            ! month (mm)
      INTEGER      ILEV              ! level index variable
      INTEGER      IMON              ! month index variable
      INTEGER      NS1               ! season number
      INTEGER      JDAYS             ! julian day
      INTEGER      NS2               ! next season number
      INTEGER      NM2               ! next month number
      INTEGER      NM1               ! month number
      INTEGER      L1                ! latitude
      INTEGER      L2                ! next latitude
      INTEGER      JDAY              ! julian date
      INTEGER      JDAYM             ! julian date

! julian start day for each month    
      INTEGER, SAVE :: IJMN( 12 ) = (/   0,  31,  59,  90, 120, 151,
     &                                 181, 212, 243, 273, 304, 334 /)
! julian day midpoints of seasons     
      INTEGER, SAVE :: MIDS( 5 )  = (/ 35, 126, 218, 310, 400 /)
! julian day midpoints by month 
      INTEGER, SAVE :: MIDM( 13 ) = (/  15,  46,  74, 105, 135, 166, 196, 
     &                                 227, 258, 288, 319, 349, 380 / )

      REAL         FS2               ! seasonal weighting factor 1
      REAL         FS1               ! seasonal weighting factor 2
      REAL         FM2               ! monthly weighting factor 1
      REAL         FM1               ! monthly weighting factor 2
      REAL         AL1               !
      REAL         FL1               ! latitude weighting factor 1
      REAL         FL2               ! latitude weighting factor 2
      REAL         A1                ! seasonally interpolated air 1
      REAL         A2                ! seasonally interpolated air 2
      REAL         T1                ! seasonally interpolated temp 1
      REAL         T2                ! seasonally interpolated temp 2
      REAL         O31               ! seasonally interpolated ozone 1
      REAL         O32               ! seasonally interpolated ozone 2
      REAL         D1                ! monthly interpolated dobson 1
      REAL         D2                ! monthly interpolated dobson 2
      REAL         DOBSON            ! lat & monthly interp. dobson
      REAL         DOBSREF           ! interp. O3 profile dobson unit

C*********************************************************************
C     begin body of subroutine INTERP2

C.......assumed julian date of seasonally averaged data, MIDS:
C.......    1 - winter -  35 - 4 feb
C.......    2 - spring - 126 - 6 may
C.......    3 - summer - 218 - 6 aug
C.......    4 - fall   - 310 - 6 nov
C.......    5 - new winter - 35+365 - 4 feb
C.......assumed julian date for monthly averaged data, MIDM:
C.......    15th day of each month

C.......parse date

      IYEAR = INT( IDATE / 10000 )
      IMONTH = INT( (IDATE - IYEAR * 10000) / 100 )
      IDAY   = IDATE - IYEAR * 10000 - IMONTH * 100
      
C.......compute Julian day, adjusting for leap years

      JDAY = IJMN( IMONTH ) + IDAY
      IF ( (MOD( IYEAR, 4 ) .EQ. 0) .AND. (IMONTH .GT. 2) )
     &      JDAY = JDAY + 1

C.......compute season interpolation parameters
C.......  calculate nearest previous season data set index

      NS1 = 4
      IF ( JDAY .GE.  35 ) NS1 = 1
      IF ( JDAY .GE. 126 ) NS1 = 2
      IF ( JDAY .GE. 218 ) NS1 = 3
      IF ( JDAY .GE. 310 ) NS1 = 4

C.......adjust for cyclic year

      JDAYS = JDAY
      IF ( JDAY .LT. 35 ) JDAYS = JDAY + 365
      NS2 = NS1 + 1
      FS2 = FLOAT( JDAYS - MIDS( NS1 ) ) /
     &      FLOAT( MIDS( NS2 ) - MIDS( NS1 ) )
      FS1 = 1.0 - FS2
      IF ( NS2 .EQ. 5 ) NS2 = 1

C.......compute monthly interpolation parameters

      DO IMON = 1, 13
        NM2 = IMON
        IF ( JDAY .LT. MIDM( IMON ) ) EXIT
      END DO

C.......adjust for cyclic year

      IF ( NM2 .EQ. 1 ) NM2 = 13
      NM1 = NM2 - 1
      JDAYM = JDAY
      IF ( JDAYM .LT. 15 ) JDAYM = JDAY + 365
      FM2 = FLOAT( JDAYM - MIDM( NM1 ) ) / 
     &      FLOAT( MIDM( NM2 ) - MIDM( NM1 ) )
      FM1 = 1.0 - FM2
      IF ( NM2 .EQ. 13 ) NM2 = 1

C.......compute latitude interpolation parameters

      AL1 = 10.0 - DLAT / 10.0
      L1  = INT( AL1 )
      L2  = L1 + 1
      IF ( DLAT .EQ. -90.0 ) L2  = L1 - 1
      FL2 = AL1 - FLOAT( L1 )
      FL1 = 1.0 - FL2

C.......interpolate

      DO ILEV = 1, MXLEV
        A1  = FS1 * XAIR( NS1, L1, ILEV ) +
     &        FS2 * XAIR( NS2, L1, ILEV )
        A2  = FS1 * XAIR( NS1, L2, ILEV ) +
     &        FS2 * XAIR( NS2, L2, ILEV )
        T1  = FS1 * XT  ( NS1, L1, ILEV ) +
     &        FS2 * XT  ( NS2, L1, ILEV )
        T2  = FS1 * XT  ( NS1, L2, ILEV ) +
     &        FS2 * XT  ( NS2, L2, ILEV )
        O31 = FS1 * XO3 ( NS1, L1, ILEV ) +
     &        FS2 * XO3 ( NS2, L1, ILEV )
        O32 = FS1 * XO3 ( NS1, L2, ILEV ) +
     &        FS2 * XO3 ( NS2, L2, ILEV )
        AIR( ILEV ) = FL1 * A1   + FL2 * A2
        T  ( ILEV ) = FL1 * T1   + FL2 * T2
        O3 ( ILEV ) = FL1 * O31  + FL2 * O32
      END DO

C.......interpolate dobson values:

      D1 = FM1 * XDOBS( L1, NM1 ) + FM2 * XDOBS( L1, NM2 )
      D2 = FM1 * XDOBS( L2, NM1 ) + FM2 * XDOBS( L2, NM2 )
      DOBSON = FL1 * D1 + FL2 * D2

C.......adjustment of O3 profiles to monthly DOBSON data from WMO 1981

      DOBSREF = O3( MXLEV ) * 1.0E5 * HO3

      DO ILEV = 1, MXLEV
        DOBSREF = DOBSREF + O3( ILEV ) * 1.0E5
      END DO

      DOBSREF = DOBSREF / 2.687E16
      
      DO ILEV = 1, MXLEV
        O3( ILEV ) = O3( ILEV ) * DOBSON / DOBSREF
      END DO

      RETURN
      END SUBROUTINE INTERP
       SUBROUTINE O3SCAL ( O3, HO3, XLAT, DOBNEW )

C*********************************************************************
C
C  adjustment of O3 profiles to a user-selected dobson value.
C   select value of DOBNEW in main program
C   if don't want to use, don't call this subroutine
C
C*********************************************************************

      USE  JVAL_PARMS

      IMPLICIT NONE
            
C...........ARGUMENTS and their descriptions

      REAL, INTENT( INOUT ) :: O3( : )  ! ozone profile
      REAL, INTENT( IN    ) :: HO3      ! ozone scale height
      REAL, INTENT( IN    ) :: XLAT     ! latitudinal band
      REAL, INTENT( IN    ) :: DOBNEW   ! new dobson unit

C...........LOCAL VARIABLES and their descriptions:

      LOGICAL, SAVE :: FIRSTTIME = .TRUE.
     
      INTEGER      I                  ! level index
     
      REAL         DOBSREF            ! reference dobson unit
      REAL         SCALE_FACTOR
               
C*********************************************************************
C     begin body of subroutine O3SCAL2

      IF ( FIRSTTIME ) THEN
        FIRSTTIME = .FALSE.
        WRITE ( 6, 2001 )
      END IF

C...convert O3 value into DU

      DOBSREF = O3( MXLEV ) * 1.0E5 * HO3
      DO I = 1, MXLEV
        DOBSREF = DOBSREF + O3( I ) * 1.0E5
      END DO
      DOBSREF = DOBSREF / 2.687E16

C...apply scale factor to O3 profile if a non-zero TOC value provided

      IF ( DOBNEW .GT. 0.0 ) THEN
        SCALE_FACTOR = DOBNEW / DOBSREF
        WRITE( 6, 2003 ) XLAT, DOBSREF, DOBNEW, SCALE_FACTOR
        DO I = 1, MXLEV
          O3( I ) = O3( I ) * SCALE_FACTOR
        END DO
      ELSE
        WRITE( 6, 2005 ) XLAT, DOBSREF
      END IF

C...formats

2001  FORMAT( 1X, 'Rescaling O3 profile based on TOC data' )
2003  FORMAT( 7X, 'Latitude:', 1X, F5.1, ', oldDOBS=', F9.4, ', newDOBS=',
     &        F9.4, ', scale factor=', F7.4 )
2005  FORMAT( 7X, 'Latitude:', 1X, F5.1, ', DOBS=', F9.4, ', not rescaled' )

      RETURN
      END SUBROUTINE O3SCAL
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPTICS ( NWL, COSZEN, ENDIR, ENDN, ENUP,
     &                    VAIR, ARAYL, GRAY, OMRAY, AO2, VO3, AO3,
     &                    VCLD, GCLD, OMCLD, VAER, AAER, GAER, OMAER,
     &                    ALBEDO, NLAYER, NLEVEL, NSURF )

C*********************************************************************
C
C This subroutine prepares the data needed for the flux calculation,
C   then calls the scattering subroutine DELTED. It returns values of
C   the flux FLUX(LEV,IWL) for altitude LEV-1, wavelength IWL.
C It calculates the optical depths (vertical)
C   for each layer, from the vertical profiles of O2, O3,
C   air, cloud, and aerosol, and from the associated "cross sections
C
C*********************************************************************

      USE  JVAL_PARMS

      IMPLICIT NONE

C...........ARGUMENTS and their descriptions

      INTEGER, INTENT( IN ) :: NLAYER             ! total # of atm layers
      INTEGER, INTENT( IN ) :: NLEVEL             ! number of levels
      INTEGER, INTENT( IN ) :: NSURF              ! ground elev above sea level
      INTEGER, INTENT( IN ) :: NWL                ! number of wl bands
      
      REAL, INTENT( IN ) :: COSZEN         ! cosine zenith angle
      REAL, INTENT( IN ) :: GAER           ! aerosol asymetry factor
      REAL, INTENT( IN ) :: GCLD           ! cloud asymetry factor
      REAL, INTENT( IN ) :: GRAY           ! asymetry fact for Rayleigh scat
      REAL, INTENT( IN ) :: OMAER          ! aerosol single scat albedo
      REAL, INTENT( IN ) :: OMCLD          ! cloud single scat cross sect
      REAL, INTENT( IN ) :: OMRAY          ! single scat albedo, Rayleigh
      REAL, INTENT( IN ) :: VAER( : )      ! aerosol column in layer
      REAL, INTENT( IN ) :: VAIR( : )      ! air column in layer
      REAL, INTENT( IN ) :: VCLD( : )      ! cloud column in layer
      REAL, INTENT( IN ) :: VO3 ( : )      ! ozone column in layer
      REAL, INTENT( IN ) :: AAER  ( : )    ! aerosol total vert opt depth
      REAL, INTENT( IN ) :: ALBEDO( : )    ! ground albedo
      REAL, INTENT( IN ) :: ARAYL ( : )    ! Rayleigh scat cross section
      REAL, INTENT( IN ) :: AO2  ( :,: )   ! O2 cross section
      REAL, INTENT( IN ) :: AO3  ( :,: )   ! average O3 cross sect in layer

      REAL, INTENT( OUT ) :: ENDIR( :,: )   ! direct flux
      REAL, INTENT( OUT ) :: ENDN ( :,: )   ! diffuse down-flux
      REAL, INTENT( OUT ) :: ENUP ( :,: )   ! diffuse up-flux

C...........LOCAL VARIABLES and their descriptions:

      INTEGER      II                 ! layer index
      INTEGER      IWL                ! wavelength index
      INTEGER      LAY                ! layer index
      INTEGER      LEV                ! level index
      INTEGER      NZ                 ! number of levels above sfc

      REAL         DTABS              ! sum of O2 & O3 absorption
      REAL         DTAER              ! aerosol scattering
      REAL         DTAIR              ! air scattering
      REAL         DTCLD              ! cloud column
      REAL         DTO2               ! O2 absorption
      REAL         DTO3               ! ozone absorption
      REAL         DTSCAT             ! total scattering 
      REAL         DTAU( NJ )         ! optical depth of layer
      REAL         EDIR( NJ )         ! irradiance of direct solar beam
      REAL         EDN ( NJ )         ! irradiance of down-welling diffuse light
      REAL         EUP ( NJ )         ! irradiance of up-welling diffuse light
      REAL         FDIR( NJ )         ! direct actinic flux
      REAL         FDN ( NJ )         ! downward actinic flux
      REAL         FUP ( NJ )         ! upward actinic flux
      REAL         G   ( NJ )         ! asymmetry factor for layer
      REAL         OM  ( NJ )         ! single-scat albedo of layer

C*********************************************************************
C     begin body of subroutine RTLINK

C...loop over wavelengths

      DO 301 IWL = 1, NWL

C...calculate optical depths for all layers (including cloud sublayers)'

        DO LAY = NSURF, NLAYER
          II = NLAYER + 1 - LAY
          DTAIR = VAIR( LAY ) * ARAYL( IWL )
          DTO2  = 0.2095 * VAIR( LAY ) * AO2( LAY, IWL )
          DTO3  = VO3( LAY ) * AO3( LAY, IWL )
          DTCLD = VCLD( LAY )
          DTAER = VAER( LAY ) * AAER( IWL )

          DTSCAT = DTAIR + DTCLD * OMCLD + DTAER * OMAER
          DTABS  = DTO2 + DTO3 + DTCLD * ( 1.0 - OMCLD )
     &           + DTAER * ( 1.0 - OMAER )
          DTABS  = AMAX1( DTABS,  1.0E-36 )
          DTSCAT = AMAX1( DTSCAT, 1.0E-36 )

          DTAU( II ) = DTABS + DTSCAT
          OM  ( II ) = DTSCAT / ( DTSCAT + DTABS )
          G   ( II ) = ( GCLD * DTCLD * OMCLD
     &               + GAER * DTAER * OMAER )
     &               / DTSCAT
        END DO

C...initialize fluxes and other DELTED parameters

        NZ = NLEVEL - NSURF + 1

        DO II = 1, NJ
          FDIR( II ) = 0.0
          FUP ( II ) = 0.0
          FDN ( II ) = 0.0
          EDIR( II ) = 0.0
          EUP ( II ) = 0.0
          EDN ( II ) = 0.0
        END DO

        CALL TWOSTR ( NZ, COSZEN, ALBEDO( IWL ), DTAU, OM, G,
     &                FDIR, FUP, FDN, EDIR, EUP, EDN )

C...return to upright grid

        DO II = 1, NLEVEL - NSURF + 1
          LEV = NLEVEL + 1 - II
          ENDIR( LEV, IWL ) = FDIR( II )
          ENDN ( LEV, IWL ) = FDN ( II )
          ENUP ( LEV, IWL ) = FUP ( II )
        END DO

301   CONTINUE

      RETURN
      END SUBROUTINE OPTICS
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PNTAVG ( WLIN, CQIN,  NWLIN,
     &                    WLOUT1, WLOUT2, CQOUT, NWLOUT )
     
C*********************************************************************
C
C  This program computes the integrated average data for the ET
C    extra terrestrial irradiance wavelength intervals
C
C  History:
C    S.Roselle  6/5/95  Program created
C
C*********************************************************************

      USE  JVAL_PARMS

      IMPLICIT NONE      
      
C...........ARGUMENTS and their descriptions

      INTEGER, INTENT( IN ) :: NWLOUT              ! number of intervals ETin
      INTEGER, INTENT( IN ) :: NWLIN               ! number of intervals CQin

      REAL, INTENT( IN ) :: WLIN( : )     ! wl of CQin
      REAL, INTENT( IN ) :: CQIN( : )     ! quantity (CS or QY) as f(WLIN)
      REAL, INTENT( IN ) :: WLOUT1( : )  ! lower limit on wl int ETin
      REAL, INTENT( IN ) :: WLOUT2( : )  ! upper limit on wl int ETin

      REAL, INTENT( OUT ) :: CQOUT ( : )  ! quantity (CS or QY) as f(WLOUT)

C...........LOCAL VARIABLES and their descriptions:
      
      INTEGER      I                   ! index var
      INTEGER      J                   ! index var
      INTEGER      MXWLPT              ! pointer
      INTEGER      MNWLPT              ! pointer
     
      REAL         DWLIN               ! wl int for CQin
      REAL         CQA                 ! lower cq value
      REAL         CQB                 ! upper cq value

C*********************************************************************
C...begin body of subroutine INTAVG

C...loop through wavelength intervals for output arrays

      DO I = 1, NWLOUT

C...find lower limit on input array

        MNWLPT = 1
        DO J = 1, NWLIN
          IF ( WLIN( J ) .LE. WLOUT1( I ) ) MNWLPT = J
        END DO

C...find upper limit on input array

        MXWLPT = NWLIN
        DO J = NWLIN, 1, -1
          IF ( WLIN( J ) .GE. WLOUT2( I ) ) MXWLPT = J
        END DO

C...initialize output arrays

        CQOUT( I ) = 0.0

C...loop through the valid wavelength intervals

        DO J = MNWLPT, MXWLPT 

C...determine weighting fraction for the wavelength interval

          IF ( WLOUT1( I ) .LE. WLIN( J ) ) THEN
            
            IF ( WLOUT2( I ) .EQ. WLIN( J+1 ) ) THEN
              DWLIN = WLIN( J+1 ) - WLIN( J )
              CQA = CQIN( J )
              CQB = CQIN( J+1 )
            ELSE IF ( WLOUT2( I ) .LT. WLIN( J+1 ) ) THEN
              DWLIN = WLOUT2( I ) - WLIN( J )
              CQA = CQIN( J )
              CQB = ( CQIN( J+1 ) - CQIN( J ) )
     &            / ( WLIN( J+1 ) - WLIN( J ) )
     &            * ( WLOUT2( I ) - WLIN( J ) ) + CQIN( J )
            ELSE IF ( WLOUT2( I ) .GT. WLIN( J+1 ) ) THEN
              DWLIN = WLIN( J+1 ) - WLIN( J )
              CQA = CQIN( J )
              CQB = CQIN( J+1 )
            END IF
            
          ELSE IF ( WLOUT1( I ) .GT. WLIN( J ) ) THEN

            IF ( WLOUT2( I ) .EQ. WLIN( J+1 ) ) THEN
              DWLIN = WLIN( J+1 ) - WLOUT1( I )
              CQA = ( CQIN( J+1 ) - CQIN( J ) )
     &            / ( WLIN( J+1 ) - WLIN( J ) )
     &            * ( WLOUT1( I ) - WLIN( J ) ) + CQIN( J )
              CQB = CQIN( J+1 )
            ELSE IF ( WLOUT2( I ) .LT. WLIN( J+1 ) ) THEN
              DWLIN = WLOUT2( I ) - WLOUT1( I )
              CQA = ( CQIN( J+1 ) - CQIN( J ) )
     &            / ( WLIN( J+1 ) - WLIN( J ) )
     &            * ( WLOUT1( I ) - WLIN( J ) ) + CQIN( J )
              CQB = ( CQIN( J+1 ) - CQIN( J ) )
     &            / ( WLIN( J+1 ) - WLIN( J ) )
     &            * ( WLOUT2( I ) - WLIN( J ) ) + CQIN( J )
            ELSE IF ( WLOUT2( I ) .GT. WLIN( J+1 ) ) THEN
              DWLIN = WLIN( J+1 ) - WLOUT1( I )
              CQA = ( CQIN( J+1 ) - CQIN( J ) )
     &            / ( WLIN( J+1 ) - WLIN( J ) )
     &            * ( WLOUT1( I ) - WLIN( J ) ) + CQIN( J )
              CQB = CQIN( J+1 )
            END IF
              
          END IF

C...set wavelength interval fraction

          DWLIN = AMAX1 ( DWLIN / ( WLOUT2( I ) - WLOUT1( I ) ), 0.0 )

C...add weighted quantity to output arrays

          CQOUT( I ) = CQOUT( I ) + 0.5 * ( CQA + CQB ) * DWLIN

        END DO

      END DO
     
      RETURN
      END SUBROUTINE PNTAVG
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE READCSQY ( NWL, STWL, ENDWL, CS, QY )
         
C*********************************************************************
C
C  the subroutine readcsqy reads the absorption cross section/quantum
C     yield file(s).  The input data are
C
C     CS(nwl,NPHOTAB)        - absorption cross sections for NR species.
C     QY(nwl,NPHOTAB)        - quantum yields
C
C    S.Roselle  1/30/96  Subroutine created, uses generalized method
C                        for reading CS/QY data that ties in with the
C                        chemistry mechanism reader.
C    S.Roselle  7/25/96  Revised subroutine to call INTAVG, passing
C                        data type (e.g., point, centered,
C                        beginning, and ending data)
C
C*********************************************************************

#ifdef m3box
      USE GET_ENV_VARS
#endif
      USE RXNS_DATA
      USE  JVAL_PARMS


      IMPLICIT NONE


C...........ARGUMENTS and their descriptions
      
      REAL, INTENT( IN ) :: STWL ( : )       ! wavelength band lower limit
      REAL, INTENT( IN ) :: ENDWL( : )       ! wavelength band upper limit

      REAL, INTENT( OUT ) :: CS( :,: )  ! output absorp. cross sections
      REAL, INTENT( OUT ) :: QY( :,: )  ! output quantum yields

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(1)   :: TYPE               ! cs/qy spectra type
      CHARACTER(16)  :: PNAME = 'READCSQY' ! program name
      CHARACTER(16)  :: CQDIR = 'CSQY'     ! directory for CSQY data
      CHARACTER(16)  :: PHOTID             ! reaction id's
      CHARACTER(80)  :: MSG   = '    '     ! message
      CHARACTER(255) :: CQFILE = ''        ! input filename buffer
      CHARACTER(255) :: EQNAME = ''

      INTEGER      IWL                 ! wavelength index
      INTEGER      NWL                 ! # of wlbands
      INTEGER      NWLIN               ! # of wlbands (infile)
      INTEGER      IPHOT               ! reaction index
      INTEGER      CQUNIT              ! cross section/qy io unit
      INTEGER      IOST                ! io status

      REAL         FACTOR              ! multiplying factor for CS
      REAL         WLIN ( MXWLIN )     ! wl for input cs/qy data
      REAL         XCSIN ( MXWLIN )     ! raw absorption cross sections
      REAL         QYIN ( MXWLIN )     ! raw quantum yields
      REAL         CSOUT( MXWL )       ! integrated absorp. cross sect.
      REAL         QYOUT( MXWL )       ! integrated quantum yields
  

C*********************************************************************
C     begin body of subroutine READCSQY

C...loop over the number of reactions, reading each file

      DO 801 IPHOT = 1, NPHOTAB

C...open input file

#ifdef m3box
        CALL VALUE_NAME ( CQDIR, EQNAME )
#else
        CALL NAMEVAL ( CQDIR, EQNAME )
#endif
        CQFILE = TRIM( EQNAME ) // '/' // TRIM( PHOTAB( IPHOT ) )

        OPEN( NEWUNIT = CQUNIT,
     &        FILE = CQFILE,
     &        STATUS = 'OLD',
     &        IOSTAT = IOST )

C...check for open errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Could not open ' // PHOTAB( IPHOT ) // ' data file:'
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          WRITE(6,'(A)') TRIM( CQFILE )
          CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
        END IF

        WRITE( 6, 2001 ) CQUNIT, CQFILE

C...read photolysis subgroup id

        READ( CQUNIT, 1001, IOSTAT = IOST ) PHOTID

C...check for read errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Errors occurred while reading PHOTID for ' //
     &           PHOTAB( IPHOT )
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
        END IF

C...get type of data (e.g. centered, beginning, ending, or point wavelen

101     CONTINUE

        READ( CQUNIT, 1003, IOSTAT = IOST ) TYPE

C...check for read errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Errors occurred while reading TYPE for ' //
     &           PHOTAB( IPHOT )
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
        END IF

        IF ( TYPE .EQ. '!' ) GO TO 101

C...read the factor to multiply cross sectionS by

        READ( CQUNIT, 1005, IOSTAT = IOST ) FACTOR

C...check for read errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Errors occurred while reading FACTOR for ' //
     &           PHOTAB( IPHOT )
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
        END IF

C...reinitialize arrays

        DO IWL = 1, MXWL
          WLIN( IWL ) = 0.0
          XCSIN( IWL ) = 0.0
          QYIN( IWL ) = 0.0
        END DO

C...loop over the number of wavelengths and continue reading

        IWL = 0
201     CONTINUE

          IWL = IWL + 1
          READ( CQUNIT, *, IOSTAT = IOST ) WLIN( IWL ), XCSIN( IWL ),
     &                                     QYIN( IWL )
          XCSIN( IWL ) = XCSIN( IWL ) * FACTOR

C...check for read errors

          IF ( IOST .GT. 0) THEN
            MSG = 'Errors occurred while reading WL,CS,QY for ' //
     &             PHOTAB( IPHOT )
            WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
            CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
          END IF

C...end loop if we reach EOF, otherwise continue looping

        IF ( IOST .EQ. 0 ) GO TO 201

C...adjust loop counter index index and close file

        NWLIN = IWL - 1
        CLOSE( CQUNIT )

        WRITE( 6, 2003 ) NWLIN

C...transform the cs data to the same wavelength intervals as
C...  the irradiance data.

        CALL INTAVG ( WLIN, XCSIN, NWLIN, TYPE,
     &                STWL, ENDWL, CSOUT, NWL )

C...transform the qy data to the same wavelength intervals as
C...  the irradiance data.

        CALL INTAVG ( WLIN, QYIN, NWLIN, TYPE,
     &                STWL, ENDWL, QYOUT, NWL )

C...load output arrays with integrated data
        
        DO IWL = 1, NWL
          CS( IWL, IPHOT ) = CSOUT( IWL )
          QY( IWL, IPHOT ) = QYOUT( IWL )
        END DO

801   CONTINUE

C...formats

1001  FORMAT( A16 )
1003  FORMAT( A1 )
1005  FORMAT( /, 4X, F10.1 )

2001  FORMAT( 1X, '...Opening File on UNIT ', I2, /, 1X, A255 )
2003  FORMAT( 1X, '...Data for ', I4, ' wavelengths read from file',
     &        // )

      RETURN
      END SUBROUTINE READCSQY
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE READET ( NWL, STWL, MIDWL, ENDWL, F )
         
C*********************************************************************
C
C  the subroutine readet reads the extra terrestrial radiation file.
C    The input data are:
C
C     NWL                   - number of wavelength bands
C     STWL(mxwl)            - array of nominal starting wavelengths of
C                             spectral interval
C     MIDWL(mxwl)           - array of nominal center wavelengths of
C                             spectral interval
C     ENDWL(mxwl)           - array of nominal ending wavelengths of
C                             spectral interval
C     F(mxwl)               - extraterrestrial solar irradiance
C
C*********************************************************************

#ifdef m3box
      USE GET_ENV_VARS
#endif
      USE  JVAL_PARMS


      IMPLICIT NONE

C...........ARGUMENTS and their descriptions
      
      REAL         ENDWL( MXWL )       ! wavelength band upper limit
      REAL         F    ( MXWL )       ! extra-terrestrial radiation
      REAL         MIDWL( MXWL )       ! wavelength midpoints
      REAL         STWL ( MXWL )       ! wavelength band lower limit

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(1)   :: TYPE                ! cs/qy spectra type
      CHARACTER(16)  :: ETFILE = 'ET'       ! ET i/o logical name
      CHARACTER(16)  :: PNAME  = 'READET'   ! program name
      CHARACTER(80)  :: MSG    = '    '     ! message
      CHARACTER(255) :: EQNAME              ! full name of ET file

      INTEGER      ETUNIT              ! extraterrestrial rad io unit
      INTEGER      IOST                ! io status
      INTEGER      IWL                 ! wavelength index
      INTEGER      NWL                 ! # of wlbands (infile)
      INTEGER      NWLIN               ! # of wlbands (infile)

      REAL         FACTOR              ! multiplying factor for F
      REAL         WLIN( MXWLIN )      ! wl for input ET data

C*********************************************************************
C     begin body of subroutine READET

C...open and read the wavelength bands and extraterrestrial radiation

#ifdef m3box
      CALL VALUE_NAME ( ETFILE, EQNAME )
#else
      CALL NAMEVAL ( ETFILE, EQNAME )
#endif


      OPEN( NEWUNIT = ETUNIT,
     &      FILE = EQNAME,
     &      STATUS = 'OLD',
     &      IOSTAT = IOST )

C...check for open errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Could not open the ET data file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

      WRITE( 6, 2001 ) ETUNIT, EQNAME

C...get type of data (e.g. centered, beginning, ending wavelength

101   CONTINUE

      READ( ETUNIT, 1003, IOSTAT = IOST ) TYPE

C...check for read errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Errors occurred while reading TYPE from ET file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

      IF ( TYPE .EQ. '!' ) GO TO 101

C...read the factor to multiply irradiance by

      READ( ETUNIT, 1005, IOSTAT = IOST ) FACTOR

C...check for read errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Errors occurred while reading FACTOR from ET file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

C...initialize arrays

      DO IWL = 1, MXWL
        WLIN( IWL ) = 0.0
        F   ( IWL ) = 0.0
      END DO

C...loop over the number of wavelengths and continue reading

      IWL = 0
201   CONTINUE

C...read the wavelength band data

        IWL = IWL + 1
        READ( ETUNIT, *, IOSTAT = IOST ) WLIN( IWL ), F( IWL )
        F( IWL ) = F( IWL ) * FACTOR

C...check for read errors

        IF ( IOST .GT. 0 ) THEN
          MSG = 'Errors occurred while reading WL,F from ET file'
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
        END IF

C...end loop if we reach EOF, otherwise continue looping

      IF ( IOST .EQ. 0 ) GO TO 201

C...adjust loop counter index index and close file

      NWLIN = IWL - 1
      CLOSE( ETUNIT )

      WRITE( 6, 2003 ) NWLIN

C...determine wl intervals for CENTERED WLBAND data

      IF ( TYPE .EQ. 'C' ) THEN

        NWL = NWLIN
        MIDWL( 1 ) = WLIN( 1 )
        STWL ( 1 ) = 0.5 * ( ( 3.0 * WLIN( 1 ) ) -  WLIN( 2 ) )
        ENDWL( 1 ) = 0.5 * ( WLIN( 1 ) + WLIN( 2 ) )
          
        DO IWL = 2, NWLIN-1
          MIDWL( IWL ) = WLIN( IWL )
          STWL ( IWL ) = 0.5 * ( WLIN( IWL ) + WLIN ( IWL - 1 ) )
          ENDWL( IWL ) = 0.5 * ( WLIN( IWL ) + WLIN ( IWL + 1 ) )
        END DO

        MIDWL( NWL ) = WLIN( NWLIN )
        STWL ( NWL ) = 0.5 * ( WLIN( NWLIN - 1 ) + WLIN( NWLIN ) )
        ENDWL( NWL ) = 0.5 * ( ( 3.0 * WLIN( NWLIN ) )
     &               - WLIN( NWLIN - 1 ) )

C...determine wl intervals for BEGINNING WLBAND data

      ELSE IF ( TYPE .EQ. 'B' ) THEN

        NWL = NWLIN - 1

        DO IWL = 1, NWLIN - 1
          STWL ( IWL ) = WLIN( IWL )
          MIDWL( IWL ) = 0.5 * ( WLIN( IWL ) + WLIN( IWL + 1 ) )
          ENDWL( IWL ) = WLIN( IWL + 1 )
        END DO

C...determine wl intervals for ENDING WLBAND data

      ELSE IF ( TYPE .EQ. 'E' ) THEN

        NWL = NWLIN - 1

        DO IWL = 2, NWLIN
          STWL ( IWL - 1 ) = WLIN( IWL - 1 )
          MIDWL( IWL - 1 ) = 0.5 * ( WLIN( IWL - 1 ) + WLIN( IWL ) )
          ENDWL( IWL - 1 ) = WLIN( IWL )
        END DO

C...stop program if wavelength data type not found

      ELSE

        MSG = 'Unrecognized spectra type in ' // TRIM( EQNAME )
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )

      END IF

C...formats

1001  FORMAT( A16 )
1003  FORMAT( A1 )
1005  FORMAT( /, 4X, F10.1 )

2001  FORMAT( 1X, '...Opening File on UNIT ', I2, /, 1X, A255, / )
2003  FORMAT( 1X, '...Data for ', I4, ' wavelengths read from file',
     &        // )

      RETURN
      END SUBROUTINE READET
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE READO2 ( NWL, STWL, ENDWL, O2ABS )
         
C*********************************************************************
C
C  the subroutine reads the absorption cross section
C     The input data are
C
C     O2ABS - absorption cross sections for molecular oxygen
C
C*********************************************************************

#ifdef m3box
      USE GET_ENV_VARS
#endif
      USE  JVAL_PARMS


      IMPLICIT NONE

C...........ARGUMENTS and their descriptions
      
      INTEGER      NWL                 ! # of wlbands
      REAL         ENDWL( MXWL )       ! wavelength band upper limit
      REAL         O2ABS( MXWL )       ! output absorp. cross sections
      REAL         STWL ( MXWL )       ! wavelength band lower limit

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(1)   :: TYPE              ! cs spectra type
      CHARACTER(16)  :: O2FILE = 'O2ABS'  ! input filename buffer
      CHARACTER(16)  :: PHOTID            ! reaction id's
      CHARACTER(16)  :: PNAME  = 'READO2' ! program name
      CHARACTER(80)  :: MSG    = '    '   ! message
      CHARACTER(255) :: EQNAME

      INTEGER      IOST                ! i/o status
      INTEGER      IWL                 ! wavelength index
      INTEGER      NWLIN               ! # of wlbands (infile)
      INTEGER      O2UNIT              ! cross section io unit

      REAL         FACTOR              ! multiplying factor for CS
      REAL         CSOUT( MXWL )       ! integrated absorp. cross sect.
      REAL         XCSIN( MXWLIN )      ! raw absorption cross sections
      REAL         WLIN( MXWLIN )      ! wl for input cs/qy data

C*********************************************************************
C     begin body of subroutine

C...get a unit number for CSQY files

#ifdef m3box
      CALL VALUE_NAME ( O2FILE, EQNAME )
#else
      CALL NAMEVAL ( O2FILE, EQNAME )
#endif

C...open input file

      OPEN( NEWUNIT = O2UNIT,
     &      FILE = EQNAME,
     &      STATUS = 'OLD',
     &      IOSTAT = IOST )

C...check for open errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Could not open the O2ABS data file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

      WRITE( 6, 2001 ) O2UNIT, EQNAME

C...read photolysis subgroup id

      READ( O2UNIT, 1001, IOSTAT = IOST ) PHOTID

C...check for read errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Errors occurred while reading PHOTID from O2ABS file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

C...get type of data (e.g. centered, beginning, ending, or point wavelen

101   CONTINUE

      READ( O2UNIT, 1003, IOSTAT = IOST ) TYPE

C...check for read errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Errors occurred while reading TYPE from O2ABS file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

      IF ( TYPE .EQ. '!' ) GO TO 101

C...read the factor to multiply cross sectionS by

      READ( O2UNIT, 1005, IOSTAT = IOST ) FACTOR

C...check for read errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Errors occurred while reading FACTOR from O2ABS file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

C...reinitialize arrays

      DO IWL = 1, MXWL
        WLIN( IWL ) = 0.0
        XCSIN( IWL ) = 0.0
      END DO

C...loop over the number of wavelengths and continue reading

      IWL = 0
201   CONTINUE

        IWL = IWL + 1
        READ( O2UNIT, *, IOSTAT = IOST ) WLIN( IWL ), XCSIN( IWL )
        XCSIN( IWL ) = XCSIN( IWL ) * FACTOR

C...check for read errors

        IF ( IOST .GT. 0 ) THEN
          MSG = 'Errors occurred while reading WL,CS from O2ABS file'
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
        END IF

C...end loop if we reach EOF, otherwise continue looping

      IF ( IOST .EQ. 0 ) GO TO 201

C...adjust loop counter index index and close file

      NWLIN = IWL - 1
      CLOSE( O2UNIT )

      WRITE( 6, 2003 ) NWLIN

C...transform the cs data to the same wavelength intervals as
C...  the irradiance data.

      CALL INTAVG ( WLIN, XCSIN, NWLIN, TYPE,
     &              STWL, ENDWL, CSOUT, NWL )

C...load output arrays with integrated data
        
      DO IWL = 1, NWL
        O2ABS( IWL ) = CSOUT( IWL )
      END DO

C...formats

1001  FORMAT( A16 )
1003  FORMAT( A1 )
1005  FORMAT( /, 4X, F10.1 )

2001  FORMAT( 1X, '...Opening File on UNIT ', I2, /, 1X, A255 )
2003  FORMAT( 1X, '...Data for ', I4, ' wavelengths read from file',
     &        // )

      RETURN
      END SUBROUTINE READO2 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE READO3 ( NWL, STWL, ENDWL, O3ABS )
         
C*********************************************************************
C
C  the subroutine reads the absorption cross section
C     The input data are
C
C     O3ABS - absorption cross sections for molecular oxygen
C
C*********************************************************************

#ifdef m3box
      USE GET_ENV_VARS
#endif
      USE JVAL_PARMS

      IMPLICIT NONE

C...........ARGUMENTS and their descriptions
      
      REAL         ENDWL( MXWL )       ! wavelength band upper limit
      REAL         O3ABS( MXWL )       ! output absorp. cross sections
      REAL         STWL ( MXWL )       ! wavelength band lower limit

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(1)   :: TYPE               ! cs/qy spectra type
      CHARACTER(16)  :: O3FILE = 'O3ABS'   ! input filename buffer
      CHARACTER(16)  :: PHOTID             ! reaction id's
      CHARACTER(16)  :: PNAME  = 'READO3'  ! program name
      CHARACTER(80)  :: MSG = '    '       ! message
      CHARACTER(255) :: EQNAME

      INTEGER      IOST                ! i/o status
      INTEGER      IWL                 ! wavelength index
      INTEGER      NWL                 ! # of wlbands
      INTEGER      NWLIN               ! # of wlbands (infile)
      INTEGER      O3UNIT              ! cross section io unit

      REAL         FACTOR              ! multiplying factor for CS
      REAL         CSOUT( MXWL )       ! integrated absorp. cross sect.
      REAL         WLIN( MXWLIN )      ! wl for input cs/qy data
      REAL         XCSIN( MXWLIN )      ! raw absorption cross sections

C*********************************************************************
C     begin body of subroutine

C...get a unit number for CSQY files

#ifdef m3box
      CALL VALUE_NAME ( O3FILE, EQNAME )
#else
      CALL NAMEVAL ( O3FILE, EQNAME )
#endif


C...open input file

      OPEN( NEWUNIT = O3UNIT,
     &      FILE = EQNAME,
     &      STATUS = 'OLD',
     &      IOSTAT = IOST )

C...check for open errors

      IF ( IOST .NE. 0) THEN
        MSG = 'Could not open the O3ABS data file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

      WRITE( 6, 2001 ) O3UNIT, EQNAME

C...read photolysis subgroup id

      READ( O3UNIT, 1001, IOSTAT = IOST ) PHOTID

C...check for read errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Errors occurred while reading PHOTID from O3ABS file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

C...get type of data (e.g. centered, beginning, ending, or point wavelen

101   CONTINUE

      READ( O3UNIT, 1003, IOSTAT = IOST ) TYPE

C...check for read errors

      IF ( IOST .NE. 0) THEN
        MSG = 'Errors occurred while reading TYPE from O3ABS file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

      IF ( TYPE .EQ. '!' ) GO TO 101

C...read the factor to multiply cross sectionS by

      READ( O3UNIT, 1005, IOSTAT = IOST ) FACTOR

C...check for read errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Errors occurred while reading FACTOR from O3ABS file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

C...reinitialize arrays

      DO IWL = 1, MXWL
        WLIN( IWL ) = 0.0
        XCSIN( IWL ) = 0.0
      END DO

C...loop over the number of wavelengths and continue reading

      IWL = 0
201   CONTINUE

        IWL = IWL + 1
        READ( O3UNIT, *, IOSTAT = IOST ) WLIN( IWL ), XCSIN( IWL )
        XCSIN( IWL ) = XCSIN( IWL ) * FACTOR

C...check for read errors

        IF ( IOST .GT. 0 ) THEN
          MSG = 'Errors occurred while reading WL,CS from O3ABS file'
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
        END IF

C...end loop if we reach EOF, otherwise continue looping

      IF ( IOST .EQ. 0 ) GO TO 201

C...adjust loop counter index index and close file

      NWLIN = IWL - 1
      CLOSE( O3UNIT )

      WRITE( 6, 2003 ) NWLIN

C...transform the cs data to the same wavelength intervals as
C...  the irradiance data.

        CALL INTAVG ( WLIN, XCSIN, NWLIN, TYPE,
     &                STWL, ENDWL, CSOUT, NWL )

C...load output arrays with integrated data
        
      DO IWL = 1, NWL
        O3ABS( IWL ) = CSOUT( IWL )
      END DO


C...formats

1001  FORMAT( A16 )
1003  FORMAT( A1 )
1005  FORMAT( /, 4X, F10.1 )

2001  FORMAT( 1X, '...Opening File on UNIT ', I2, /, 1X, A255 )
2003  FORMAT( 1X, '...Data for ', I4, ' wavelengths read from file',
     &        // )

      RETURN
      END SUBROUTINE READO3
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE READPROF ( XAIR, AIR, XO3, XDOBS, O3, AER, XT, T )
         
C*********************************************************************
C
C  the subroutine readprof reads the atmospheric profiles file.
C    The input data are:
C
C     XO3 (nsea,nlat,nlev) - ozone profiles
C     XT  (nsea,nlat,nlev) - temperature profiles
C     XAIR(nsea,nlat,nlev) - air concentration profile
C     XDOBS(nlat,MONTH)    - average dobson values
C
C*********************************************************************

#ifdef m3box
      USE GET_ENV_VARS
#endif
      USE JVAL_PARMS


      IMPLICIT NONE
      
C...........ARGUMENTS and their descriptions
      
      REAL         O3 ( MXLEV )        ! ozone profile
      REAL         AER( MXLEV )        ! aerosol attenuation profile
      REAL         T  ( MXLEV )        ! interpolated temp profile
      REAL         AIR( MXLEV )        ! interpolated air profile
      REAL         XDOBS( 19, 12 )     ! lat-season ozone values
      REAL         XO3 ( 12, 19, MXLEV ) ! season-lat-vert ozone profile
      REAL         XT  ( 12, 19, MXLEV ) ! season-lat-vert temp profile
      REAL         XAIR( 12, 19, MXLEV ) ! air concentration

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(16)  :: PNAME  = 'READPROF'  ! program name
      CHARACTER(16)  :: PFFILE = 'PROFILES'  ! profiles i/o logical name
      CHARACTER(80)  :: MSG    = '    '      ! message
      CHARACTER(255) :: EQNAME               ! full name of profile file

      INTEGER      IMON                ! month index
      INTEGER      ILAT                ! latitude index
      INTEGER      ILEV                ! level index
      INTEGER      ISEA                ! season index
      INTEGER      IOST                ! io status
      INTEGER      PFUNIT              ! profiles io unit

C*********************************************************************
C     begin body of subroutine READPROF

C...read 4 seasons of 19xMXLEV fields of O3, T and M (air pressure in
C...  molecules/cm**3).  T and M data is from Louis (Ph.D. thesis 1974
C...  U.of Colorado).  O3 profiles are from Isaksen et al.
C...  diabatic 2D model

#ifdef m3box
      CALL VALUE_NAME ( PFFILE, EQNAME )
#else
      CALL NAMEVAL ( PFFILE, EQNAME )
#endif

      OPEN( NEWUNIT = PFUNIT,
     &      FILE = EQNAME,
     &      STATUS = 'OLD',
     &      IOSTAT = IOST )

C...check for open errors

      IF ( IOST .NE. 0) THEN
        MSG = 'Could not open the PROFILES data file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

      WRITE( 6, 2001 ) PFUNIT, EQNAME

      DO 70 ISEA = 1, 4

C...read ozone profiles

        DO ILAT = 1, 19
          READ( PFUNIT, 1001, IOSTAT = IOST )
     &        ( XO3 ( ISEA, ILAT, ILEV ), ILEV = 1, MXLEV )

C...check for read errors

          IF ( IOST .GT. 0) THEN
            MSG = 'Errors occurred while reading XO3 in PROFILES file'
            WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
            CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
         END IF

        END DO

C...read temperature profiles

        DO ILAT = 1, 19
          READ( PFUNIT, 1001, IOSTAT = IOST )
     &        ( XT  ( ISEA, ILAT, ILEV ), ILEV = 1, MXLEV )

C...check for read errors

          IF ( IOST .GT. 0) THEN
            MSG = 'Errors occurred while reading XT in PROFILES file'
            WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
            CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
          END IF
        END DO

C...read pressure/air concentration profiles

        DO ILAT = 1, 19
          READ( PFUNIT, 1001, IOSTAT = IOST )
     &        ( XAIR( ISEA, ILAT, ILEV ), ILEV = 1, MXLEV )

C...check for read errors

          IF ( IOST .GT. 0) THEN
            MSG = 'Errors occurred while reading XAIR in PROFILES file'
            WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
            CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
          END IF

        END DO
70    END DO
C
C...Read dobson values (12 months, 19 latitudes):
C... Source = WMO(1981) originally from Deutch 1971.
C... These values are used if subroutine O3SCAL is invoked in main progr
C
      DO IMON = 1, 12
        READ( PFUNIT, *, IOSTAT = IOST ) ( XDOBS( ILAT, IMON ),
     &                                            ILAT = 1, 19 )

C...check for read errors

        IF ( IOST .GT. 0) THEN
          MSG = 'Errors occurred while reading XDOBS in PROFILES file'
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
        END IF

      END DO
C
C...read standard profiles: air, temp, and ozone are from
C...  Nicolet et al. 1982 Planet.Space Sci. v30 p935.  Aerosols from
C...  Elterman 1968 AFCRL-68-0153 report (Air Force Cambridge Labs.)
C...  These standard profiles will be used if subroutine INTERP(DLAT,I
C...  is not invoked.  If it is invoked (in the main program), local
C...  profiles will be interpolated to latitude and date, then used.
C...  In either case, aerosol profiles are currently from here.
C
      DO ILEV = 1, MXLEV
        READ( PFUNIT, *, IOSTAT = IOST ) T ( ILEV ), AIR( ILEV ),
     &                                   O3( ILEV ), AER( ILEV )

C...check for read errors

        IF ( IOST .GT. 0) THEN
          MSG ='Errors occurred while reading STD PROF in PROFILES file'
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
        END IF

      END DO

      CLOSE( PFUNIT )

C...formats

1001  FORMAT( 8E10.3 )
2001  FORMAT( 1X, '...Opening File on UNIT ', I2, /, 1X, A255, / )

      RETURN
      END SUBROUTINE READPROF
      SUBROUTINE READTOMS ( JDT, JVLAT, XLATJV, DOBNEW )

C*********************************************************************
C
C  This routine reads the daily gridded data for real-time satellite
C     total ozone column (TOC) data.  variables lat and lon contain
C     the latitudes and longitudes of the center of each of the grid
C     cells in the array ozone.                                    
C
C  HISTORY:
C     10/7/2009 S.Roselle  Updated to read TOMS datasets with different
C                          lat/lon grid sizes (e.g. OMI); expanded
C                          latitudinal band averaging to full width
C                          of TOMS dataset
C     04/11/96  S.Roselle  Modified program to conform with Models3
C                          coding standards and to average latitudinal
C                          bands of TOMS values
C     04/10/96  Program received from NASA via web/ftp
C
C*********************************************************************

#ifdef m3box
      USE GET_ENV_VARS
#endif
      USE JVAL_PARMS

      IMPLICIT NONE

C...........ARGUMENTS and their descriptions

      INTEGER      JDT                ! julian date (yyyyddd)
      INTEGER      JVLAT              ! number of output latitudes
      REAL         XLATJV( JVLAT )    ! latitudes
      REAL         DOBNEW( JVLAT )    ! average TOC values

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(16)  :: TMFILE = 'TOMS'     ! TOMS i/o logical name
      CHARACTER(16)  :: PNAME  = 'READTOMS'
      CHARACTER(80)  :: MSG = '    '
      CHARACTER(255) :: EQNAME              ! full name of TOMS file

      INTEGER      ILAT               ! latitude index
      INTEGER      JLAT               ! latitude index
      INTEGER      ILON               ! longitude index
      INTEGER      NLAT               ! number of input latitudes
      INTEGER      NLON               ! number of input longitudes
      INTEGER      DDD                ! julian day
      INTEGER      YYYY               ! year
      INTEGER      TJDATE             ! julian date for toms data
      INTEGER      IOST               ! io status
      INTEGER      TMUNIT             ! unit number for TOMS file
      INTEGER      ALLOCSTAT          ! allocate status
      INTEGER      COUNT( JVLAT )     ! # points included in TOC average
      INTEGER, ALLOCATABLE :: OZONE( :,: )

      REAL         STLAT, STLON       ! starting point lat,lon
      REAL         ENDLAT, ENDLON     ! ending point lat,lon
      REAL         DLAT
      REAL         DLON
      REAL, ALLOCATABLE :: LAT( : )
      REAL, ALLOCATABLE :: LON( : )



C*********************************************************************
C     begin body of program READTOMS
                                                              
C...calculate latitudes and longitudes                   

C...open the input file

#ifdef m3box
      CALL VALUE_NAME ( TMFILE, EQNAME )
#else
      CALL NAMEVAL ( TMFILE, EQNAME )
#endif


      OPEN ( NEWUNIT = TMUNIT,
     &       FILE = EQNAME,
     &       FORM = 'FORMATTED',
     &       STATUS = 'OLD',
     &       IOSTAT = IOST )

C...check for open errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Could not open the TOMS data file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

      WRITE( 6, 2001 ) TMUNIT, EQNAME

C...read in the header lines

      READ( TMUNIT, 1001 ) DDD, YYYY
      TJDATE = YYYY * 1000 + DDD

C...check to see if julian date of file matches julian date requested
C...  and warn user if they do not match

      IF ( TJDATE .NE. JDT ) THEN
        MSG = 'Julian date of TOMS file does not match requested date '
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
      END IF
      
C...read longitude info and allocate array

      READ( TMUNIT, 1003 ) NLON, STLON, ENDLON, DLON

      ALLOCATE ( LON( NLON ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
        MSG = 'Failure allocating LON'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

      DO ILON = 1, NLON
        LON( ILON ) = -STLON + ( ILON - 1 ) * DLON
      END DO

C...read latitude info and allocate array

      READ( TMUNIT, 1003 ) NLAT, STLAT, ENDLAT, DLAT

      ALLOCATE ( LAT( NLAT ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
        MSG = 'Failure allocating LAT'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

      DO ILAT = 1, NLAT
        LAT( ILAT ) = -STLAT + ( ILAT - 1 ) * DLAT
      END DO

C...allocate and read in the data into the array ozone

      ALLOCATE ( OZONE( NLON, NLAT ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
        MSG = 'Failure allocating OZONE'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
      END IF

      DO ILAT = 1, NLAT
        READ( TMUNIT, 1005, IOSTAT = IOST ) ( OZONE( ILON, ILAT ),
     &                                               ILON = 1, NLON )

        IF ( IOST .NE. 0) THEN
          MSG = 'Errors occurred while reading TOMS file'
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          CALL M3EXIT( PNAME, 0, 0, ":Fatal Error", 0 )
        END IF

      END DO

C...close the input file

      CLOSE( TMUNIT )

C...process/print the ozone data

      WRITE( 6, 2003 )

      DO JLAT = 1, JVLAT

        COUNT ( JLAT ) = 0
        DOBNEW( JLAT ) = 0.0

        DO ILAT = 1, NLAT

          IF ( ( LAT( ILAT ) .GT. ( XLATJV( JLAT ) - 5.0 ) ) .AND.
     &         ( LAT( ILAT ) .LT. ( XLATJV( JLAT ) + 5.0 ) ) ) THEN

            DO ILON = 1, NLON
               IF ( OZONE( ILON, ILAT ) .GT. 0 ) THEN
                  COUNT ( JLAT ) = COUNT( JLAT ) + 1
                  DOBNEW( JLAT ) = DOBNEW( JLAT )
     &                           + FLOAT( OZONE( ILON, ILAT ) )
               END IF
            END DO

          END IF

        END DO

        IF ( COUNT( JLAT ) .GT. 0 ) THEN
          DOBNEW( JLAT ) = DOBNEW( JLAT ) / FLOAT( COUNT( JLAT ) )
          WRITE( 6, 2005 ) XLATJV( JLAT ), COUNT( JLAT ), DOBNEW( JLAT )
        ELSE
        	WRITE( 6, 2007 ) XLATJV( JLAT )
        END IF
      END DO

C...format statements

1001  FORMAT( 6X, I3, 9X, I4 )
1003  FORMAT( 13X, I4, 18X, F7.3, 6X, F7.3, 5X, F4.2 )
1005  FORMAT( 1X, 25I3 )
2001  FORMAT( 1X, '...Opening File on UNIT ', I2, /, 1X, A255 )
2003  FORMAT( 1X, 'Processing Total Ozone Column Data' )
2005  FORMAT( 7X, 'Latitude:', 1X, F5.1, ', N=', I5, ', Mean TOC=', F9.4 )
2007  FORMAT( 7X, 'Latitude:', 1X, F5.1, ', TOC=0.0' )

      RETURN
      END SUBROUTINE READTOMS
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SETAER ( NWL, MIDWL, AAER, OMAER, GAER, HAER )

C*********************************************************************
C
C  Specify aerosols
C
C    AAER(IWL) = aerosol total vertical optical depth variation with
C                wavelength.  Estimated from Elterman (1968)
C    AER(I)    = attenuation (per km) profile from Elterman (1968).
C                Given in data statement in beginning of code,
C                for 340 nm (IWL=60)
C                Same vertical shape at all wavelengths.
C                Normalized later (in subroutine SUBGRID) to total
C                vertical dep this wavelength.
C    OMAER     = aerosol single scattering albedo.  Use 0.99 for now.
C    GAER      = aerosol asymetry factor.  Use 0.61 (Hansen and
C                Travis 1974) (these are assuming particles of
C                about 0.1 micron radius index of refraction of
C                about 1.65 + 0.002i.
C    HAER      = the aerosol scale height at top of atmosphere
C                use equal to air (8.05 km)
C
C*********************************************************************

      USE  JVAL_PARMS
      IMPLICIT NONE

C...........ARGUMENTS and their descriptions

      INTEGER      NWL                ! number of wavelength bands

      REAL         MIDWL ( MXWL )     ! wavelength band midpoints
      REAL         AAER  ( MXWL )     ! aerosol total vert opt depth
      REAL         OMAER              ! aerosol single scat albedo
      REAL         GAER               ! aerosol asymetry factor
      REAL         HAER               ! aerosol scale ht at atm top

C...........LOCAL VARIABLES and their descriptions:

      INTEGER      IWL                ! wavelength index

C*********************************************************************
C     begin body of subroutine SETAER

      DO IWL = 1, NWL
        AAER( IWL ) = 0.379 * ( 340.0 / MIDWL( IWL ) )
      END DO

      OMAER = 0.990
      GAER  = 0.610
      HAER  = 8.05

      RETURN
      END SUBROUTINE SETAER
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SETAIR ( NWL, MIDWL, HAIR, OMRAY, GRAY, ARAYL )

C*********************************************************************
C
C  Specify air/Rayleigh parameters
C    HAIR      = air scale height, used to estimate ozone density
C                   column a upper boundary (50km).
C    OMRAY     = single scattering albedo, Rayleigh.  Use 1.00
C    GRAY      =  asymetry factor for Rayleigh scattering.  Use 0.0
C    ARAYL(KL) = Rayleigh scattering cross section, from
C                Frohlich and Shaw, Appl.Opt. v.11, p.1773 (1980).
C                overrides tabulation of JDATA.BASE
C
C*********************************************************************

      USE  JVAL_PARMS
      IMPLICIT NONE


C...........ARGUMENTS and their descriptions

      INTEGER      NWL                ! number of wavelength bands

      REAL         HAIR               ! air scale height
      REAL         OMRAY              ! single scat albedo, Rayleigh
      REAL         GRAY               ! asymetry fact for Rayleigh scat
      REAL         ARAYL ( MXWL )     ! Rayleigh scat cross section
      REAL         MIDWL ( MXWL )     ! wavelength band midpoints

C...........LOCAL VARIABLES and their descriptions:

      INTEGER      IWL                ! wavelength index

      REAL         XX                 ! intermediate var
      REAL         WMICRON            ! wavelength in microns

C*********************************************************************
C     begin body of subroutine SETAIR

      HAIR  = 8.05
      OMRAY = 1.0
      GRAY  = 0.0

      DO IWL = 1, NWL
        WMICRON = MIDWL( IWL ) / 1.0E3
        XX = 3.916 + 0.074 * WMICRON + 0.050 / WMICRON
        ARAYL( IWL ) = 3.90E-28 / WMICRON**XX
      END DO

      RETURN
      END SUBROUTINE SETAIR 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SET_ALBEDO ( NWL, MIDWL, ALBEDO )

C*********************************************************************
C
C  Set the albedo of the surface. 
C    Use best estimate albedo of Demerjian et al.,
C    Adv.Env.Sci.Tech.,v.10,p.369, (1980)
C
C*********************************************************************

      USE  JVAL_PARMS

      IMPLICIT NONE

C...........ARGUMENTS and their descriptions

      INTEGER      NWL                ! number of wavelength bands

      REAL         MIDWL ( MXWL )     ! wavelength band midpoints
      REAL         ALBEDO( MXWL )     ! ground albedo

C...........LOCAL VARIABLES and their descriptions:

      INTEGER      IWL                ! wavelength index

C*********************************************************************
C     begin body of subroutine SET_ALBEDO

      DO IWL = 1, NWL

        IF ( MIDWL( IWL ) .LT. 400.0 ) THEN
          ALBEDO( IWL ) = 0.05
        ELSE IF (( MIDWL( IWL ) .GE. 400.0 ) .AND.
     &           ( MIDWL( IWL ) .LT. 450.0 )) THEN
          ALBEDO( IWL ) = 0.06
        ELSE IF (( MIDWL( IWL ) .GE. 450.0 ) .AND.
     &           ( MIDWL( IWL ) .LT. 500.0 )) THEN
          ALBEDO( IWL ) = 0.08
        ELSE IF (( MIDWL( IWL ) .GE. 500.0 ) .AND.
     &           ( MIDWL( IWL ) .LT. 550.0 )) THEN
          ALBEDO( IWL ) = 0.10
        ELSE IF (( MIDWL( IWL ) .GE. 550.0 ) .AND.
     &           ( MIDWL( IWL ) .LT. 600.0 )) THEN
          ALBEDO( IWL ) = 0.11
        ELSE IF (( MIDWL( IWL ) .GE. 600.0 ) .AND.
     &           ( MIDWL( IWL ) .LT. 640.0 )) THEN
          ALBEDO( IWL ) = 0.12
        ELSE IF (( MIDWL( IWL ) .GE. 640.0 ) .AND.
     &           ( MIDWL( IWL ) .LT. 660.0 )) THEN
          ALBEDO( IWL ) = 0.135
        ELSE IF ( MIDWL( IWL ) .GE. 660.0 ) THEN
          ALBEDO( IWL ) = 0.15
        END IF

      END DO

      RETURN
      END SUBROUTINE SET_ALBEDO
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SETCLD ( NLAYS, NLEVS, NSUBKM, IBASE, ITOP, CLOUD,
     &                    OMCLD, GCLD )

C*********************************************************************
C
C   CLOUD MUST BE SPECIFIED EVEN FOR CLEAR PHOTOLYSIS RATES
C   A FINE GRID IS REQUIRED BY THE DELTA-EDDINGTON SOLVER
C   DEPTH = cloud optical depth
C   IBASE = cloud base index (height/km  +  1)
C   ITOP =  cloud top index (height/km  +  1)
C           NSUBKM = cloud sublayers/km
C           NSBTOT = total number of cloud sublayers (36 or less)
C           NLAYS = total number of atmospheric layers
C   CLOUD(K) = cloud optical depth profile.  Relative optical depth
C           cloud sublayer.  Assume constant cloud profile for now
C           Re- normalize to total optical depth
C           OMCLD = single scattering cross sect.  Use 0.9995
C           GCLD = asymetry factor.  Use 0.875
C
C*********************************************************************

      USE  JVAL_PARMS
      IMPLICIT NONE

C...........ARGUMENTS and their descriptions

      INTEGER      NLAYS              ! total # of atm layers
      INTEGER      NLEVS              ! number of levels
      INTEGER      NSUBKM             ! cloud sublayers/km
      INTEGER      IBASE              ! cloud base index
      INTEGER      ITOP               ! cloud top index

      REAL         CLOUD( 48 )        ! cloud optical depth profile
      REAL         OMCLD              ! cloud single scat cross sect
      REAL         GCLD               ! cloud asymetry factor

C...........LOCAL VARIABLES and their descriptions:

      INTEGER      K                  ! cloud sublayer index
      INTEGER      NSBTOT             ! total # of cloud sublayers 

      REAL         CTOT               ! total cloud column
      REAL         DEPTH              ! optical cloud depth

C*********************************************************************
C     begin body of subroutine SETCLD

      DEPTH = 0.0

      IBASE = 20
      ITOP  = 30

      NSUBKM = 36 / ( ITOP - IBASE )
      NSBTOT = NSUBKM * ( ITOP - IBASE )
      NLAYS  = MXLEV + ( NSUBKM - 1 ) * ( ITOP - IBASE )
      NLEVS  = NLAYS + 1

      DO K = 1, NSBTOT
        CLOUD( K ) = 1.0
      END DO

C...normalize cloud profile

      CTOT = 0

      DO K = 1, NSBTOT
        CTOT = CTOT + CLOUD( K )
      END DO

      DO K = 1, NSBTOT
        CLOUD( K ) = DEPTH / CTOT
      END DO

      OMCLD = 1.000
      GCLD  = 0.860

      RETURN
      END SUBROUTINE SETCLD
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SRBAND ( NWL, STWL, MIDWL, ENDWL, COSZEN, NLAYS, AO2,
     &                    CVO2, VT, ZMID, O2ABS )
       
C*********************************************************************
C
C  this subroutine calculates effective O2 cross sections in the
C    Schumann-Runge band, using the formulae from Allen and Frederick,
C    J.Atmos.Sci., v39, p2066 (1982).  The coefficients are those
C    modified by Wuebbles (LLL report, 1982).
C
C*********************************************************************

      USE  JVAL_PARMS

      IMPLICIT NONE

C...........PARAMETERS and their descriptions

      INTEGER, PARAMETER :: NWLO2 = 11  ! # of wl bands for O2 SR data

C...........ARGUMENTS and their descriptions

      INTEGER, INTENT( IN ) :: NWL                ! number of wavelength bands
      INTEGER, INTENT( IN ) :: NLAYS              ! total # of atm layers

      REAL, INTENT( IN ) :: COSZEN      ! cosine zenith angle
      REAL, INTENT( IN ) :: STWL( : )   ! wavelength bands starting point
      REAL, INTENT( IN ) :: MIDWL( : )  ! wavelength bands midpoint
      REAL, INTENT( IN ) :: ENDWL( : )  ! wavelength bands ending point
      REAL, INTENT( IN ) :: CVO2( : )   ! vertical column O2
      REAL, INTENT( IN ) :: VT  ( : )   ! average temp of column
      REAL, INTENT( IN ) :: ZMID( : )   ! altitude of midpoint of layer
      REAL, INTENT( IN ) :: O2ABS( : )  ! O2 cross sections

      REAL, INTENT( OUT ) :: AO2( :,: )    ! layered O2 cross sections

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(1), SAVE :: TYPE = 'B' ! cs spectra type (B=beginning wl)

      INTEGER      IWL                ! wavelength index
      INTEGER      IWLO2              ! wavelength index for ref O2 data
      INTEGER      ILAY               ! layer index
      INTEGER      N20                ! layer at 20 km

      REAL         E10                ! e**10
      REAL         X1                 ! 
      REAL         X2                 ! x1**2
      REAL         X3                 ! x1**3
      REAL         X4                 ! x1**4
      REAL         X5                 ! x1**5
      REAL         X6                 ! x1**6
      REAL         X7                 ! x1**7
      REAL         X8                 ! x1**8
      REAL         AO20               !
      REAL         AO20LG             !
      REAL         Y1                 ! 
      REAL         Y2                 ! y1**2
      REAL         Y3                 ! y1**3
      REAL         Y4                 ! y1**4
      REAL         C                  !
      REAL         CLOG               !
      REAL         ZENDEP             !
      REAL         AO2W1( NWLO2 )     ! O2 SR data on ref WL bands
      REAL         AO2W2( MXWL )      ! O2 SR data on ET wl bands
      REAL         WLO2L( NWLO2+1 )   ! lower wl's for O2 SR data
      DATA         WLO2L / 185.185, 186.916, 188.679, 190.476, 192.308,
     &                     194.175, 196.078, 198.020, 200.000, 202.020,
     &                     204.082, 206.186/
      SAVE         WLO2L

      REAL         SRA1( NWLO2 )      ! Schumann-Runge coef a1
      DATA SRA1 / -2.158311E+01, -2.184813E+01, -2.200507E+01,
     &            -2.205527E+01, -2.205261E+01, -2.228000E+01,
     &            -2.275796E+01, -2.297610E+01, -2.506084E+01,
     &            -2.313436E+01, -2.312205E+01 /
      SAVE         SRA1

      REAL         SRA2( NWLO2 )      ! Schumann-Runge coef a2
      DATA SRA2 / -4.164652E-01, -4.753880E-01, -4.628729E-01,
     &            -4.400848E-01, -5.707936E-01, -3.960759E-01,
     &            -2.054719E-01, -5.823677E-02,  3.442774E-02,
     &             1.177283E-04,  0.000000E+00 /
      SAVE         SRA2

      REAL         SRA3( NWLO2 )      ! Schumann-Runge coef a3
      DATA SRA3 /  5.266362E-02,  4.519945E-02, -5.022541E-02,
     &            -5.687308E-03, -3.330207E-02, -2.995798E-02,
     &            -1.094205E-02, -1.007612E-01, -2.212047E-04,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA3

      REAL         SRA4( NWLO2 )      ! Schumann-Runge coef a4
      DATA SRA4 /  1.655877E-02,  3.228313E-02,  2.545036E-02,
     &             3.712279E-02,  5.959032E-02,  4.918104E-02,
     &             2.079595E-02,  2.404666E-02,  6.186041E-07,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA4

      REAL         SRA5( NWLO2 )      ! Schumann-Runge coef a5
      DATA SRA5 /  0.000000E+00,  3.079373E-03,  5.791406E-02,
     &             6.025527E-03,  1.510540E-02,  9.269080E-03,
     &             3.769638E-03,  4.761876E-02, -6.284394E-10,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA5

      REAL         SRA6( NWLO2 )      ! Schumann-Runge coef a6
      DATA SRA6 /  0.000000E+00,  0.000000E+00,  1.179966E-02,
     &             0.000000E+00,  1.000376E-03, -1.173411E-03,
     &             0.000000E+00,  4.169606E-03,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA6

      REAL         SRA7( NWLO2 )      ! Schumann-Runge coef a7
      DATA SRA7 /  0.000000E+00,  0.000000E+00, -8.296876E-03,
     &             0.000000E+00,  0.000000E+00, -2.599386E-04,
     &             0.000000E+00, -7.126663E-03,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA7

      REAL         SRA8( NWLO2 )      ! Schumann-Runge coef a8
      DATA SRA8 /  0.000000E+00,  0.000000E+00, -3.238368E-03,
     &             0.000000E+00,  0.000000E+00,  0.000000E+00,
     &             0.000000E+00, -2.263652E-03,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA8

      REAL         SRA9( NWLO2 )      ! Schumann-Runge coef a9
      DATA SRA9 /  0.000000E+00,  0.000000E+00, -3.069686E-04,
     &             0.000000E+00,  0.000000E+00,  0.000000E+00,
     &             0.000000E+00, -1.971653E-04,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA9

      REAL         SRB1( NWLO2 )      ! Schumann-Runge coef b1
      DATA SRB1 / -2.431640E+03, -3.701955E+01, -1.086239E+03,
     &            -1.213108E+03, -8.334575E+01, -2.139117E+02,
     &            -3.281301E+02,  3.033416E+03, -2.535815E+00,
     &            -4.474937E+00, -2.996639E+00 /
      SAVE         SRB1

      REAL         SRB2( NWLO2 )      ! Schumann-Runge coef b2
      DATA SRB2 /  4.729722E+02,  3.623290E+00,  1.981847E+02,
     &             2.277459E+02,  7.944254E+00,  2.612729E+01,
     &             4.307004E+01, -5.978911E+02,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRB2

      REAL         SRB3( NWLO2 )      ! Schumann-Runge coef b3
      DATA SRB3 / -3.452121E+01, -8.929223E-02, -1.359057E+01,
     &            -1.612207E+01, -1.898894E-01, -1.036749E+00,
     &            -1.870019E+00,  4.370384E+01,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRB3

      REAL         SRB4( NWLO2 )      ! Schumann-Runge coef b4
      DATA SRB4 /  1.120677E+00,  0.000000E+00,  4.155845E-01,
     &             5.101389E-01,  0.000000E+00,  1.317695E-02,
     &             2.674331E-02, -1.406715E+00,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRB4

      REAL         SRB5( NWLO2 )      ! Schumann-Runge coef b5
      DATA SRB5 / -1.365618E-02,  0.000000E+00, -4.788462E-03,
     &            -6.090518E-03,  0.000000E+00,  0.000000E+00,
     &             0.000000E+00,  1.683967E-02,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRB5

C*********************************************************************
C     begin body of subroutine SRBAND

C...initialize cross sections:
           
      DO ILAY = 1, NLAYS
        DO IWL = 1, NWL
          AO2( ILAY, IWL ) = O2ABS( IWL )
        END DO
      END DO

C...correct as needed
C... use formula for 20-50 km.
C... below 20 km, use 20 km value
C... find layer near 20 km

      DO ILAY = 1, NLAYS
        IF ( ZMID( ILAY ) .GT. 20.0 ) THEN
          N20 = ILAY
          EXIT
        END IF
      END DO

301   CONTINUE

      E10 = ALOG( 10.0 )
      DO ILAY = N20, NLAYS
      
        DO IWLO2 = 1, NWLO2
        
          X1 = ALOG( 4.696E-23 * CVO2( ILAY ) / 0.2095 ) / E10
          IF ( WLO2L( IWLO2 ) .GE. 200.0 ) X1 = VT( ILAY )
          X2 = X1 * X1
          X3 = X2 * X1
          X4 = X3 * X1
          X5 = X4 * X1
          X6 = X5 * X1
          X7 = X6 * X1
          X8 = X7 * X1
          AO20LG = SRA1( IWLO2 )      + SRA2( IWLO2 ) * X1
     &           + SRA3( IWLO2 ) * X2 + SRA4( IWLO2 ) * X3
     &           + SRA5( IWLO2 ) * X4 + SRA6( IWLO2 ) * X5
     &           + SRA7( IWLO2 ) * X6 + SRA8( IWLO2 ) * X7
     &           + SRA9( IWLO2 ) * X8
          AO20 = 10.0**AO20LG

          Y1 = ALOG( CVO2( ILAY ) ) / E10
          Y2 = Y1 * Y1
          Y3 = Y2 * Y1
          Y4 = Y3 * Y1
          CLOG = SRB1( IWLO2 )      + SRB2( IWLO2 ) * Y1
     &         + SRB3( IWLO2 ) * Y2 + SRB4( IWLO2 ) * Y3
     &         + SRB5( IWLO2 ) * Y4
          C = 10.0**CLOG
          ZENDEP = COSZEN**C
          
          AO2W1( IWLO2 ) = AO20 * ZENDEP
          
        END DO

C...now transfrom the computed O2 data from their
C...  reference wavelength bands to the same wavelength bands
C...  as the extraterrestrial irradiance data

        CALL INTAVG ( WLO2L, AO2W1, NWLO2+1, TYPE,
     &                STWL,  ENDWL, AO2W2, NWL )

        DO IWL = 1, NWL
          IF ( ENDWL( IWL ) .LE. 205.0 ) THEN
            AO2( ILAY, IWL ) = AO2W2( IWL )
          END IF
        END DO

      END DO

C...assign values below 20 km

      DO ILAY = 1, N20 - 1
        DO IWL = 1, NWL
          AO2( ILAY, IWL ) = AO2( N20, IWL )
        END DO
      END DO

      RETURN
      END SUBROUTINE SRBAND
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SUBGRID ( NWL, STWL, MIDWL, ENDWL, CS, CSZ, QY, QYZ,
     &                     AIR, HAIR, VAIR, CVO2, O3ABS, O3, HO3, VO3,
     &                     AO3, IBASE, ITOP, CLOUD, NSUBKM, VCLD,
     &                     AER, VAER, HAER, T, VT, Z, ZMID,
     &                     NLAYS, NLEVS )
        
C*********************************************************************
C
C  This subroutine computes all altitude dependent quantities over the
C       sub-divided grid
C  The following quantities are computed at each LEVEL (altitude)
C       Z   (ILEV) = altitude (km) of each level
C       ZAIR(ILEV) = air concentration at each level
C       ZT  (ILEV) = temperature at each level
C       CSZ(ILEV,IWL,IPHOT) = abs. cross sect at each altitude (T,P corrected)
C       QYZ(ILEV,IWL,IPHOT) = quantum yield at each altitude (T,P corrected)
C
C  The following quantities are computed at each LAYER (thickness)
C       ZMID(ILAY)     = altitude of modpoint of layer
C       VAIR(ILAY)     = air column in layer (vertical)
C       VO3 (ILAY)     = ozone column   "       "
C       VAER(ILAY)     = aerosol "      "       '
C       VCLD(ILAY)     = cloud   "      "       "
C       VT  (ILAY)     = average temperature of column
C       AO3 (ILAY,IWL) = average O3 cross sect in layer (with ave. layer
C
C*********************************************************************

      USE RXNS_DATA
      USE JVAL_PARMS

      IMPLICIT NONE

C...........PARAMETERS and their descriptions
      
      INTEGER, PARAMETER :: NWLO3 = 29  ! number of wl bands for O3 T cor

C...........ARGUMENTS and their descriptions

      INTEGER, INTENT( IN ) :: NWL                ! number of wavelength bands
      INTEGER, INTENT( IN ) :: NSUBKM             ! cloud sublayers/km
      INTEGER, INTENT( IN ) :: IBASE              ! cloud base index
      INTEGER, INTENT( IN ) :: ITOP               ! cloud top index
      INTEGER, INTENT( IN ) :: NLAYS              ! total # of atm layers
      INTEGER, INTENT( IN ) :: NLEVS              ! number of levels

      REAL, INTENT( IN ) :: HAIR             ! air scale height
      REAL, INTENT( IN ) :: HO3              ! ozone scale height
      REAL, INTENT( IN ) :: HAER             ! aerosol scale ht at atm top
      REAL, INTENT( IN ) :: CLOUD( : )       ! cloud optical depth profile
      REAL, INTENT( IN ) :: T  ( : )         ! interpolated temp profile
      REAL, INTENT( IN ) :: AER( : )         ! aerosol attenuation profile
      REAL, INTENT( IN ) :: O3 ( : )        ! ozone profile
      REAL, INTENT( IN ) :: AIR( : )        ! interpolated air profile
      REAL, INTENT( IN ) :: STWL( : )       ! wavelength band starting point
      REAL, INTENT( IN ) :: MIDWL( : )      ! wavelength band midpoints
      REAL, INTENT( IN ) :: ENDWL( : )      ! wavelength band ending point
      REAL, INTENT( IN ) :: O3ABS( : )      ! O3 absorption cross section
      REAL, INTENT( INOUT )  :: VT  ( : )    ! average temp of column
      REAL, INTENT( INOUT )  :: Z   ( : )    ! altitude of each level
      REAL, INTENT( INOUT )  :: ZMID( : )    ! altitude of midpoint of layer
      REAL, INTENT( INOUT )  :: VAER( : )    ! aerosol column in layer
      REAL, INTENT( INOUT )  :: VCLD( : )    ! cloud column in layer
      REAL, INTENT( INOUT )  :: VO3 ( : )    ! ozone column in layer
      REAL, INTENT( INOUT )  :: VAIR( : )    ! air column in layer
      REAL, INTENT( INOUT )  :: CVO2( : )    ! vertical column O2
      REAL, INTENT( INOUT )  :: AO3( :,: )   ! average O3 cross sect in layer
      REAL, INTENT( INOUT )  :: CS( :,: )    ! cross sections
      REAL, INTENT( INOUT )  :: QY( :,: )    ! quantum yields
      REAL, INTENT( INOUT )  :: CSZ( :,:,: ) ! cross section at each level
      REAL, INTENT( INOUT )  :: QYZ( :,:,: ) ! quantum yields T&P corrected

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(8), SAVE :: SRO1D = '        ' ! source for o1d data
      CHARACTER(1), SAVE :: TYPE = 'B' ! cs spectra type (B=beginning wl)

      LOGICAL, SAVE :: FIRSTIME = .TRUE. ! Flag for first call to SUBGRID

      INTEGER      ILEV               ! level index
      INTEGER      I                  ! index
      INTEGER      K                  ! level index
      INTEGER      ILAY               ! layer index
      INTEGER      IWL                ! wavelength index
      INTEGER      II                 ! layer index
      INTEGER      IPHOT              ! reaction index
      INTEGER, SAVE :: IO3O1D             ! O3O1D reaction index 
      INTEGER, SAVE :: IO3O3P             ! O3O3P reaction index
      INTEGER, SAVE :: IHCHOM             ! HCHOM reaction index
      INTEGER, SAVE :: IHCHOR             ! HCHOR reaction index
      INTEGER, SAVE :: IALD               ! ALD reaction index
      INTEGER, SAVE :: IACETONE           ! ACETONE reaction index
      INTEGER, SAVE :: IKETONE            ! KETONE reaction index
      INTEGER, SAVE :: IGLYF              ! GLYF reaction index
      INTEGER, SAVE :: IGLYM              ! GLYM reaction index
      INTEGER, SAVE :: IGLY_R             ! GLY_R reaction index
      INTEGER, SAVE :: IMGLY              ! MGLY reaction index
      INTEGER      IWLO3              ! index for wl bands for O3 t cor

      REAL         DZ                 ! atmosphere depth
      REAL         DZCLD              ! cloud thickness
      REAL         HLOCAL             ! local air scale
      REAL         HLOCA1             ! local air scale
      REAL         HLOCA2             ! local aerosol scale
      REAL         X0                 ! height fraction
      REAL         X1                 ! height above fraction
      REAL         AKT                ! temp corrected form diff
      REAL         AK300              ! diff in qy(2) for form
      REAL         PHI20              ! qy for form react 2 diff
      REAL         SUM                ! total aerosol opt depth
      REAL         DZ1                ! x0 - dz
      REAL         DZ2                ! x0 + dz
      REAL         TDIFFX             ! temp diff for o3 c-s
      REAL         PHI1               ! qy for form react 1
      REAL         PHI2               ! qy for form react 2
      REAL         C                  ! coef 3 for o3 qy
      REAL         A                  ! coef 1 for o3 qy
      REAL         B                  ! coef 2 for o3 qy
      REAL         TN1                ! temp diff for o3 qy nasa94
      REAL         TR1                ! temp diff for o3 qy radm
      REAL         XL                 ! wavelength
      REAL         XL0                ! wavelength
      REAL         DXL                ! wl delta from 305nm
      REAL         A0                 ! 1st coef for O3 qy
      REAL         A1                 ! 2nd coef for O3 qy
      REAL         A2                 ! 3rd coef for O3 qy
      REAL         A3                 ! 4th coef for O3 qy
      REAL         A4                 ! 5th coef for O3 qy
      REAL         A5                 ! 6th coef for O3 qy
      REAL         A6                 ! 7th coef for O3 qy

C...coefficients for JPL06-2 and IUPAC04 O3O1D QY temperature adjustment

      REAL, SAVE :: jA1 = 0.8036        
      REAL, SAVE :: jA2 = 8.9061        
      REAL, SAVE :: jA3 = 0.1192        
      REAL, SAVE :: jX1 = 304.225 ! nm  
      REAL, SAVE :: jX2 = 314.957 ! nm  
      REAL, SAVE :: jX3 = 310.737 ! nm  
      REAL, SAVE :: jw1 = 5.576 ! nm    
      REAL, SAVE :: jw2 = 6.601 ! nm    
      REAL, SAVE :: jw3 = 2.187 ! nm    
      REAL, SAVE :: jv1 = 0.0 ! cm-1    
      REAL, SAVE :: jv2 = 825.518 ! cm-1
      REAL, SAVE :: jc = 0.0765        
      REAL, SAVE :: jR = 0.695 ! cm-1/K
      REAL          jq1, jq2, jq112, jq212
      REAL          jTEMP, jT300

C...

      REAL         ZAIR( NJ )         ! air concentration profile
      REAL         ZT  ( NJ )         ! temperature profile

      REAL         CSO3W1( NWLO3 )    ! temp O3 CS on t coef wl's
      REAL         CSO3W2( MXWL )     ! temp O3 CS on ET's wls
! lower wl's for O3 c-s temp coef.
      REAL, SAVE :: WLO3L( NWLO3+1 ) = (/
     &       263.158,   266.667,   270.270,   273.973,   277.778,
     &       281.690,   285.714,   289.855,   294.118,   298.500,
     &       302.500,   303.500,   304.500,   305.500,   306.500,
     &       307.500,   308.500,   309.500,   310.500,   311.500,
     &       312.500,   313.500,   314.500,   317.500,   322.500,
     &       327.500,   332.500,   337.500,   342.500,   347.500 /)

      REAL, SAVE ::  SO3TX( NWLO3, 3 )  ! O3 c-s temperature cooef.
      DATA ( SO3TX( IWLO3, 1 ), IWLO3 = 1, NWLO3 ) /
     &       9.630E+00, 8.320E+00, 6.880E+00, 5.370E+00, 3.960E+00,
     &       2.710E+00, 1.750E+00, 1.060E+00, 5.960E-01, 3.330E-01,
     &       2.400E-01, 2.100E-01, 1.800E-01, 1.600E-01, 1.400E-01,
     &       1.200E-01, 1.050E-01, 9.000E-02, 8.000E-02, 7.000E-02,
     &       6.000E-02, 5.500E-02, 4.000E-02, 2.190E-02, 1.010E-02,
     &       5.080E-03, 2.120E-03, 8.290E-04, 2.940E-04 /

      DATA ( SO3TX( IWLO3, 2 ), IWLO3 = 1, NWLO3 ) /
     &       1.190E-03, 3.640E-04, 2.460E-04, 1.030E-03, 1.690E-03,
     &       1.450E-03, 8.940E-04, 7.830E-04, 4.940E-04, 3.550E-04,
     &       2.950E-04, 2.750E-04, 2.500E-04, 2.300E-04, 2.080E-04,
     &       1.860E-04, 1.640E-04, 1.450E-04, 1.280E-04, 1.121E-04,
     &       1.000E-04, 9.200E-05, 7.500E-05, 4.830E-05, 3.430E-05,
     &       1.820E-05, 8.850E-06, 4.270E-06, 5.300E-06 /

      DATA ( SO3TX( IWLO3, 3 ), IWLO3 = 1, NWLO3 ) /
     &      -1.740E-05, 2.470E-06, 1.170E-05, 1.260E-06,-6.860E-06,
     &      -2.890E-06, 3.590E-06, 2.000E-06, 3.660E-06, 2.600E-06,
     &       2.170E-06, 1.950E-06, 1.380E-06, 1.650E-06, 1.550E-06,
     &       1.460E-06, 1.340E-06, 1.210E-06, 1.130E-06, 1.060E-06,
     &       9.400E-07, 8.700E-07, 7.500E-07, 5.200E-07, 2.660E-07,
     &       1.630E-07, 1.260E-07, 8.710E-08, 3.500E-08 /


C*********************************************************************
C     begin body of subroutine SUBGRID2

      IF ( FIRSTIME ) THEN

C... Setup indices for reactions

        IO3O1D  = INDEX2( 'O3O1D',   NPHOTAB, PHOTAB )
        IF ( IO3O1D .EQ. 0 ) IO3O1D  = INDEX2( 'O3_O1D', NPHOTAB, PHOTAB )

        IO3O3P  = INDEX2( 'O3O3P',   NPHOTAB, PHOTAB )
        IF ( IO3O3P .EQ. 0 ) IO3O3P  = INDEX2( 'O3_O3P', NPHOTAB, PHOTAB )

        IHCHOM  = INDEX2( 'HCHOM', NPHOTAB, PHOTAB )
        IF ( IHCHOM .EQ. 0 ) IHCHOM  = INDEX2( 'HCHO_M', NPHOTAB, PHOTAB )
        IF ( IHCHOM .EQ. 0 ) IHCHOM  = INDEX2( 'FORM_M', NPHOTAB, PHOTAB )

        IHCHOR  = INDEX2( 'HCHOR', NPHOTAB, PHOTAB )
        IF ( IHCHOR .EQ. 0 ) IHCHOR  = INDEX2( 'HCHO_R', NPHOTAB, PHOTAB )
        IF ( IHCHOR .EQ. 0 ) IHCHOR  = INDEX2( 'FORM_R', NPHOTAB, PHOTAB )

        IALD    = INDEX2( 'ALD',     NPHOTAB, PHOTAB )
        IF (IALD .EQ. 0) IALD = INDEX2( 'CCHO',     NPHOTAB, PHOTAB )

        IACETONE= INDEX2( 'ACET_', NPHOTAB, PHOTAB )
        IF (IACETONE .EQ. 0) IACETONE = INDEX2( 'CH3COCH3',     NPHOTAB, PHOTAB )

        IKETONE = INDEX2( 'KET',  NPHOTAB, PHOTAB )

        IGLYF   = INDEX2( 'GLYF',  NPHOTAB, PHOTAB )

        IGLYM   = INDEX2( 'GLY_07M',  NPHOTAB, PHOTAB )
        IF( IGLYM .EQ. 0 ) IGLYM   = INDEX2( 'GLYH2',  NPHOTAB, PHOTAB )

        IGLY_R  = INDEX2( 'GLY_07R',   NPHOTAB, PHOTAB )
        IF(  IGLY_R .EQ. 0 ) IGLY_R  = INDEX2( 'GLYHX',   NPHOTAB, PHOTAB )
  
       IMGLY   = INDEX2( 'MGLY',    NPHOTAB, PHOTAB )
        
        write(6,*) 'Photolysis pressure/temperature dependencies:'
        write(6,*) '0: disabled; >0 enabled'
        write(6,*) ''
        write(6,*) 'O3 O3P', IO3O3P
        write(6,*) 'O3 O1D', IO3O1D
        write(6,*) 'formaldehyde 2*HO2:', IHCHOR
        write(6,*) 'formaldehyde H2', IHCHOM
        write(6,*) 'Acetaldehyde:', IALD
        write(6,*) 'Acetone:', IACETONE
        write(6,*) 'Ketone:', IKETONE
        write(6,*) 'Glyoxal formaldehyde:', IGLYF
        write(6,*) 'Glyoxal molecular', IGLYM
        write(6,*) 'Glyoxal radical', IGLY_R
        write(6,*) 'Methyl glyoxal', IMGLY

C...check to see if O1D is from IUPAC, NASA94, or RADM data

        IF ( IO3O1D .GT. 0 ) THEN
          IF ( INDEX( PHOTAB( IO3O1D ), 'IUPAC04' ) .GT. 0 ) THEN
            SRO1D = 'IUPAC04'
          ELSE IF ( INDEX( PHOTAB( IO3O1D ), 'IUPAC10' ) .GT. 0 ) THEN
            SRO1D = 'IUPAC10'
          ELSE IF ( INDEX( PHOTAB( IO3O1D ), 'NASA94' ) .GT. 0 ) THEN
            SRO1D = 'NASA94'
          ELSE IF ( INDEX( PHOTAB( IO3O1D ), 'NASA06' ) .GT. 0 ) THEN
            SRO1D = 'NASA06'
          ELSE IF ( INDEX( PHOTAB( IO3O1D ), 'JPL06-2' ) .GT. 0 ) THEN
            SRO1D = 'JPL06-2'
          ELSE IF ( INDEX( PHOTAB( IO3O1D ), 'RADM' ) .GT. 0 ) THEN
            SRO1D = 'RADM'
          ELSE
            SRO1D = 'JPL06-2'
          END IF
        END IF

        FIRSTIME = .FALSE.
        
      END IF
      
      DZ = 1.0E+05
      DZCLD = 1.0E+05 / FLOAT( NSUBKM )

C...levels

      ILEV = 0
      DO I = 1, IBASE - 1
        ILEV = ILEV + 1
        Z   ( ILEV ) = FLOAT( I - 1 )
        ZAIR( ILEV ) = AIR( I )
        ZT  ( ILEV ) = T( I )
      END DO

      DO I = IBASE, ITOP-1
        HLOCAL = 1.0 / ALOG( AIR( I ) / AIR( I + 1 ) )
        DO K = 1, NSUBKM
          X0 = FLOAT( K - 1 ) * DZCLD / DZ
          X1 = 1.0 - X0
          ILEV = ILEV + 1
          Z   ( ILEV ) = FLOAT( I - 1 ) + X0
          ZAIR( ILEV ) = AIR( I ) * EXP( -X0 / HLOCAL )
          ZT  ( ILEV ) = X0 * T( I + 1 ) + X1 * T( I )
        END DO
      END DO

      DO I = ITOP, 51
        ILEV = ILEV + 1
        Z   ( ILEV ) = FLOAT( I - 1 )
        ZAIR( ILEV ) = AIR( I )
        ZT  ( ILEV ) = T( I )
      END DO

      Z   ( NLEVS ) = 51.0
      ZAIR( NLEVS ) = AIR( 51 ) * EXP( -1.0 / HAIR )
      ZT  ( NLEVS ) = T( 51 ) + ( T( 51 ) - T( 50 ) )

C...assign default yields

      DO ILEV = 1, NLEVS
        DO IWL = 1, NWL
          DO IPHOT = 1, NPHOTAB
            QYZ( ILEV, IWL, IPHOT ) = QY( IWL, IPHOT )
            CSZ( ILEV, IWL, IPHOT ) = CS( IWL, IPHOT )
          END DO
        END DO
      END DO

C...correct absorption cross sections for T and P

      IF ( ( IO3O1D .GT. 0 ) .AND. ( IO3O3P .GT. 0 ) ) THEN
        DO ILEV = 1, NLEVS   ! level loop

C...compute O3 cross sections for wavelength bands corresponding
C...  to the reference data for the temperature coefficients

          TDIFFX = ZT( ILEV ) - 230.0
          DO IWLO3 = 1, NWLO3        ! wavelength loop
            CSO3W1( IWLO3 ) = 1.0E-18 * ( SO3TX( IWLO3, 1 )
     &                      + SO3TX( IWLO3, 2 ) * TDIFFX
     &                      + SO3TX( IWLO3, 3 ) * TDIFFX * TDIFFX )
          END DO   ! wavelength loop

C...now transfrom the computed O3 cross sections from their
C...  reference wavelength bands to the same wavelength bands
C...  as the extraterrestrial irradiance data

          CALL INTAVG ( WLO3L, CSO3W1, NWLO3+1, TYPE,
     &                  STWL, ENDWL, CSO3W2, NWL )

C...replace the O3 cross sections data only within the wavelengths
C...  which are sensitive to temperature changes

          DO IWL = 1, NWL            ! wavelength loop
            IF ( ( STWL( IWL ) .GE. 263.158 ) .AND. 
     &           ( ENDWL( IWL ) .LE. 347.5 ) ) THEN
              CSZ( ILEV, IWL, IO3O1D ) = CSO3W2( IWL )
              CSZ( ILEV, IWL, IO3O3P ) = CSZ( ILEV, IWL, IO3O1D )
            END IF
          END DO    ! wavelength loop
        END DO    ! level loop

      END IF

C...Adjust quantum yields for temperature dependencies

C...O3O1D:  jpl06-2 and iupac04
        
      IF ( ( SRO1D .EQ. 'IUPAC10' ) .OR. ( SRO1D .EQ. 'IUPAC04' ) 
     &                 .OR. ( SRO1D .EQ. 'JPL06-2' ) .OR. SRO1D .EQ. 'NASA06' ) THEN

        DO ILEV = 1, NLEVS   ! level loop
          jTEMP = MIN( 320.0, MAX( 200.0, ZT( ILEV ) ) )
          jq1 = EXP( -jv1 / ( jR * jTEMP ) )
          jq2 = EXP( -jv2 / ( jR * jTEMP ) )
          jq112 = jq1 / ( jq1 + jq2 )
          jq212 = jq2 / ( jq1 + jq2 )
          jT300 = jTEMP / 300

          DO IWL = 1, NWL   ! wavelength loop
            XL = MIDWL( IWL )
            IF ( ( XL .GE. 305.0 ) .AND. ( XL .LE. 328.0 ) ) THEN
              QYZ( ILEV, IWL, IO3O1D ) = jq112 * jA1
     &                                 * EXP( -( ( jX1 - XL ) / jw1 )**4 )
     &                                 + jq212 * jA2 * jT300**2
     &                                 * EXP( -( ( jX2 - XL ) / jw2 )**2 )
     &                                 + jA3 * jT300**1.5
     &                                 * EXP( -( ( jX3 - XL ) / jw3 )**2 )
     &                                 + jC
            END IF
          END DO   ! wavelength loop

        END DO   ! level loop
      END IF   ! jpl06-2 and iupac04
        
C...O3O1D:  radm

      IF ( SRO1D .EQ. 'RADM' ) THEN
        DO ILEV = 1, NLEVS        ! level loop

          TR1 = ZT( ILEV ) - 230.0
          A = 0.9 * ( 0.369
     &              + 2.85E-4 * TR1
     &              + 1.28E-5 * TR1 * TR1
     &              + 2.57E-8 * TR1 * TR1 * TR1 )
          B =       - 0.575
     &              + 5.59E-3  * TR1
     &              - 1.439E-5 * TR1 * TR1
     &              - 3.27E-8  * TR1 * TR1 * TR1
          C = 0.9 * ( 0.518
     &              + 9.87E-4 * TR1
     &              - 3.94E-5 * TR1 * TR1
     &              + 3.91E-7 * TR1 * TR1 * TR1 )
          XL0 =       308.20
     &              + 4.4871E-2 * TR1
     &              + 6.9380E-5 * TR1 * TR1
     &              - 2.5452E-6 * TR1 * TR1 * TR1

          DO IWL = 1, NWL        ! wavelength loop
            XL = MIDWL( IWL )
            QYZ( ILEV, IWL, IO3O1D ) = A * ATAN( B * ( XL - XL0 ) ) + C
            IF ( QYZ( ILEV, IWL, IO3O1D ) .LT. 0.0 ) THEN
              QYZ( ILEV, IWL, IO3O1D ) = 0.0
            ELSE IF ( QYZ( ILEV, IWL, IO3O1D ) .GT. 0.9 ) THEN
              QYZ( ILEV, IWL, IO3O1D ) = 0.9
            END IF
          END DO   ! wavelength loop

        END DO   ! level loop
      END IF  ! radm o3o1d

C...O3O1D:  nasa94
        
      IF ( SRO1D .EQ. 'NASA94' ) THEN
        DO ILEV = 1, NLEVS        ! level loop

          TN1 = 298.0 - ZT( ILEV )
          A0 =  0.94932   - 1.7039E-4 * TN1 + 1.4072E-6 * TN1 * TN1
          A1 = -2.4052E-2 + 1.0479E-3 * TN1 - 1.0655E-5 * TN1 * TN1
          A2 =  1.8771E-2 - 3.6401E-4 * TN1 - 1.8587E-5 * TN1 * TN1
          A3 = -1.454E-2  - 4.7787E-5 * TN1 + 8.1277E-6 * TN1 * TN1
          A4 =  2.3287E-3 + 1.9891E-5 * TN1 - 1.1801E-6 * TN1 * TN1
          A5 = -1.4471E-4 - 1.7188E-6 * TN1 + 7.2661E-8 * TN1 * TN1
          A6 =  3.183E-6  + 4.6209E-8 * TN1 - 1.6266E-9 * TN1 * TN1

          DO IWL = 1, NWL        ! wavelength loop
            XL = MIDWL( IWL )
            IF ( XL .LT. 290.0 ) THEN
              QYZ( ILEV, IWL, IO3O1D ) = 0.9
            ELSE IF ( ( XL .GE. 290.0 ) .AND. ( XL .LT. 305.0 ) ) THEN
              QYZ( ILEV, IWL, IO3O1D ) = 0.95
            ELSE IF ( ( XL .GE. 305.0 ) .AND. ( XL .LE. 320.0 ) ) THEN
              DXL = MIDWL( IWL ) - 305.0
              QYZ( ILEV, IWL, IO3O1D ) = A0          + A1 * DXL
     &                                 + A2 * DXL**2 + A3 * DXL**3
     &                                 + A4 * DXL**4 + A5 * DXL**5
     &                                 + A6 * DXL**6
              IF ( QYZ( ILEV, IWL, IO3O1D ) .LT. 0.02 ) THEN
                QYZ( ILEV, IWL, IO3O1D ) = 0.0
              END IF
            ELSE IF ( XL .GT. 320.0 ) THEN
              QYZ( ILEV, IWL, IO3O1D ) = 0.0
            END IF
          END DO   ! wavelength loop

        END DO   ! level loop
      END IF   ! nasa94 o3o1d
        
C...O3O3P
C...  The yields for O3->O(3P) are calculated as:  (1.- singlet D yield)

      IF ( ( IO3O3P .GT. 0 ) .AND. ( IO3O1D .GT. 0 ) ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            QYZ( ILEV, IWL, IO3O3P ) = 1.0 - QYZ( ILEV, IWL, IO3O1D )
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...CH2O formaldehyde:
C...  the CH2O yield recalculated only for wavelengths longer than 329 nm

      IF ( ( IHCHOM .GT. 0 ) .AND. ( IHCHOR .GT. 0 ) ) THEN
          
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            IF ( ( XL .GE. 330.0 ) .AND.
     &           ( QYZ( ILEV, IWL, IHCHOM ) .GT. 0.0 ) ) THEN
              PHI1 = QYZ( ILEV, IWL, IHCHOR )
              PHI2 = QYZ( ILEV, IWL, IHCHOM )
              PHI20 = 1.0 - PHI1
              AK300 = ( ( 1.0 / PHI2 ) - ( 1.0 / PHI20 ) ) / 2.54E+19
              AKT = AK300 * ( 1.0 + 61.69 * ( 1.0 - ZT( ILEV ) / 300.0 )
     &            * ( XL / 329.0 - 1.0 ) )
              QYZ( ILEV, IWL, IHCHOM ) = 1.0 / ( ( 1.0 / PHI20 )
     &                                 + ZAIR( ILEV ) * AKT )
            END IF
            
            IF ( QYZ( ILEV, IWL, IHCHOM ) .GT. 1.0 ) THEN
              QYZ( ILEV, IWL, IHCHOM ) = 1.0
            ELSE IF ( QYZ( ILEV, IWL, IHCHOM ) .LT. 0.0 ) THEN
              QYZ( ILEV, IWL, IHCHOM ) = 0.0
            END IF
          END DO   ! wavelength loop
        END DO   ! level loop

      END IF

C...CH3CHO and the dicarbonyls yields are calculated
C...  from the NTP yield by linear adjustment to 1/yield.  
C...Ketones yield is calculated from fit equations


C...CH3CHO acetaldehyde:

      IF ( IALD .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            IF ( QY( IWL, IALD ) .NE. 0.0 ) THEN
              QYZ( ILEV, IWL, IALD ) = 1.0 /
     &                               ( 1.0 + (1.0 / QY( IWL, IALD )
     &                               - 1.0 ) * ZAIR( ILEV ) / 2.465E19 )
            END IF
          END DO   ! wavelength loop
        END DO   ! level loop

      END IF

C...ACETONE + HV -> CH3CO. + CH3.

      IF ( IACETONE .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            QYZ( ILEV, IWL, IACETONE ) = 0.0766 + 0.09415
     &                                 * EXP( -ZAIR( ILEV ) / 3.222E18 )
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...CH3COC2H5 + hv -> ACO3 + ETH

      IF ( IKETONE .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            QYZ( ILEV, IWL, IKETONE ) = 0.0766 + 0.09415
     &                                * EXP( -ZAIR( ILEV ) / 3.222E18 )
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...HCOCHO  glyoxal PROCESS A:

      IF ( IGLYF .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            IF ( QY( IWL, IGLYF ) .NE. 0.0 ) THEN
              QYZ( ILEV, IWL, IGLYF ) = 1.0 / ( 1.0
     &                                + (1.0 / QY( IWL, IGLYF ) - 1.0 )
     &                                * ZAIR( ILEV ) / 2.465E19 )
            END IF
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...HCOCHO  glyoxal PROCESS B:

      IF ( IGLYM .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            IF ( QY( IWL, IGLYM ) .NE. 0.0 ) THEN
              QYZ( ILEV, IWL, IGLYM ) = 1.0 / ( 1.0 
     &                                + (1.0 / QY( IWL, IGLYM ) - 1.0 )
     &                                * ZAIR( ILEV ) / 2.465E19 )
            END IF
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...Glyoxal + hv = 2 HCO

      IF ( IGLY_R .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            IF ( QY( IWL, IGLY_R ) .NE. 0.0 ) THEN
              QYZ( ILEV, IWL, IGLY_R ) = 1.0 / ( 1.0 
     &                                + (1.0 / QY( IWL, IGLY_R ) - 1.0 )
     &                                * ZAIR( ILEV ) / 2.465E19 )
            END IF
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...CH3COCHO  methylglyoxal:

      IF ( IMGLY .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            IF ( QY( IWL, IMGLY ) .NE. 0.0) THEN
              QYZ( ILEV, IWL, IMGLY ) = 1.0 / ( 1.0 
     &                                + ( 1.0 / QY( IWL, IMGLY ) - 1.0 )
     &                                * ZAIR( ILEV ) / 2.465E19 )
            END IF
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...layers

      ILAY = 0
      DO I = 1, IBASE-1
        ILAY = ILAY + 1
        ZMID( ILAY ) = FLOAT( I - 1 ) + 0.5
        VAIR( ILAY ) = DZ * ( AIR( I + 1 ) - AIR( I ) )
     &               / ALOG( AIR( I + 1 ) / AIR( I ) )
        VO3 ( ILAY ) = DZ * ( O3( I + 1 ) + O3( I ) ) / 2.0
        VCLD( ILAY ) = 0.0
        VAER( ILAY ) = ( AER( I + 1 ) - AER( I ) )
     &               / ALOG( AER( I + 1 ) / AER( I ) )
        VT  ( ILAY ) = ( T( I + 1 ) + T( I ) ) / 2.0
      END DO

      DO I = IBASE, ITOP - 1
        HLOCA1 = 1.0 / ALOG( AIR( I ) / AIR( I + 1 ) )
        HLOCA2 = 1.0 / ALOG( AER( I ) / AER( I + 1 ) )
        DO K = 1, NSUBKM
          X0 = ( FLOAT( K - 1 ) + 0.5 ) * DZCLD / DZ
          X1 = 1.0 - X0
          ILAY = ILAY + 1
          ZMID( ILAY ) = FLOAT( I ) + X0
          DZ1 = X0 - 0.5 * DZCLD / DZ
          DZ2 = X0 + 0.5 * DZCLD / DZ
          VAIR( ILAY ) = AIR( I ) * HLOCA1 * 1.0E5
     &                 * ( EXP( -DZ1 / HLOCA1 )
     &                   - EXP( -DZ2 / HLOCA1 ) )
          VO3 ( ILAY ) = DZCLD * ( X0 * O3( I + 1 ) + X1 * O3( I ) )
          VCLD( ILAY ) = CLOUD( NSUBKM * ( I - IBASE ) + K )
          VAER( ILAY ) = AER( I ) * HLOCA2
     &                 * ( EXP( -DZ1 / HLOCA2 )
     &                   - EXP( -DZ2 / HLOCA2 ) )
          VT( ILAY ) = X0 * T( I + 1 ) + X1 * T( I )
        END DO
      END DO

      DO I = ITOP, 50
        ILAY = ILAY + 1
        ZMID( ILAY ) = FLOAT( I - 1 ) + 0.5
        VAIR( ILAY ) = DZ * ( AIR( I + 1 ) - AIR( I ) )
     &                 / ALOG( AIR( I + 1 ) / AIR( I ) )
        VO3 ( ILAY ) = DZ * ( O3( I + 1 ) + O3( I ) ) / 2.0
        VCLD( ILAY ) = 0.0
        VAER( ILAY ) = ( AER( I + 1 ) - AER( I ) )
     &               / ALOG( AER( I + 1 ) / AER( I ) )
        VT  ( ILAY ) = ( T( I + 1 ) + T( I ) ) / 2.0
      END DO
      ZMID( NLAYS ) = 50.5
      VAIR( NLAYS ) = DZ * HAIR * AIR( 51 )
      VO3 ( NLAYS ) = DZ * HO3 * O3( 51 )
      VCLD( NLAYS ) = 0.0
      VAER( NLAYS ) = HAER * AER( 51 )
      VT  ( NLAYS ) = T( 51 )

C...normalize aerosol optical depth to unity sum

      SUM = 0.0
      DO ILAY = 1, NLAYS
        SUM = SUM + VAER( ILAY )
      END DO
      DO ILAY = 1, NLAYS
        VAER( ILAY ) = VAER( ILAY ) / SUM
      END DO

C...calculated vertical column of O2 above the midpoint of each layer:
C...  want to use this for computing the average Schumann-Runge cross 
C...  section in each layer.
C...  so use half of current layer and half of previous higher layer

      CVO2( NLAYS ) = 0.2095 * VAIR( NLAYS ) / 2.0
      DO II = 2, NLAYS
        ILAY = NLAYS - II + 1
        CVO2( ILAY ) = CVO2( ILAY + 1 ) + 0.2095 * ( VAIR( ILAY )
     &               + VAIR( ILAY + 1 ) ) / 2.0
      END DO

C...correct attenuation coefficients for pressure and/or temperature
C...  dep. for now do only ozone absorption.

      DO ILAY = 1, NLAYS              ! layer loop

C...compute O3 cross sections for wavelength bands corresponding
C...  to the reference data for the temperature coefficients

        TDIFFX = VT( ILAY ) - 230.0
        DO IWLO3 = 1, NWLO3
          CSO3W1( IWLO3 ) = 1.0E-18 * ( SO3TX( IWLO3, 1 )
     &                      + SO3TX( IWLO3, 2 ) * TDIFFX
     &                      + SO3TX( IWLO3, 3 ) * TDIFFX * TDIFFX )
        END DO

C...now transfrom the computed O3 cross sections from their
C...  reference wavelength bands to the same wavelength bands
C...  as the extraterrestrial irradiance data

        CALL INTAVG ( WLO3L, CSO3W1, NWLO3+1, TYPE,
     &                STWL, ENDWL, CSO3W2, NWL )

C...replace the O3 cross sections data only within the wavelengths
C...  which are sensitive to temperature changes

        DO IWL = 1, NWL
          AO3( ILAY, IWL ) = O3ABS( IWL )
          IF ( ( STWL( IWL ) .GE. 263.158 ) .AND. 
     &         ( ENDWL( IWL ) .LE. 347.5 ) ) THEN
            AO3( ILAY, IWL ) =  CSO3W2( IWL )
          END IF
        END DO

      END DO   ! layer loop

      RETURN
      END SUBROUTINE SUBGRID
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE TRIDIAG ( A, B, E, D, U, N )
C-----------------------------------------------------------------------
C
C  FUNCTION:
C    Solves tridiagonal system by Thomas algorithm.  Algorithm fails
C    if first pivot is zero.  In that case, rewrite the
C    equation as a set of order N-1, with U(2) trivially eliminated.
C The associated tri-diagonal system is stored in 3 arrays
C   B: diagonal
C   A: sub-diagonal
C   E: super-diagonal
C   D: right hand side function
C   U : return solution from tridiagonal solver
C
C     [ B(1) E(1) 0    0    0 ...       0     ]
C     [ A(2) B(2) E(2) 0    0 ...       .     ]
C     [ 0    A(3) B(3) E(3) 0 ...       .     ]
C     [ .       .     .     .           .     ] U(i) = D(i)
C     [ .             .     .     .     0     ]
C     [ .                   .     .     .     ]
C     [ 0                           A(n) B(n) ]
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION HISTORY:
C    NO.   DATE     WHO      WHAT
C    __    ____     ___      ____
C    4     4/3/96    SJR  copied code and modified for use in JPROC
C    3     8/16/94   XKX  configuration management include statements
C    2     3/15/92   CJC  For use in Models-3 LCM.
C    1     10/19/89  JKV  converted for use on IBM
C    0      3/89     BDX  Initial version
C                    yoj
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, PARAMETER :: NMAX = 400

C...ARGUMENTS and their descriptions:

      INTEGER, INTENT( IN ) :: N                ! number of rows in matrix

      REAL, INTENT( IN ) :: A( : )        ! subdiagonal
      REAL, INTENT( IN ) :: B( : )        ! diagonal
      REAL, INTENT( IN ) :: E( : )        ! superdiagonal
      REAL, INTENT( IN ) :: D( : )        ! R.H. side
      REAL, INTENT( OUT ) :: U( : )       ! solution

C SCRATCH LOCAL VARIABLES and their descriptions:

      INTEGER     J                ! loop index

      REAL        BET              !
      REAL        GAM( NMAX )      ! 

C...begin body of subroutine  TRIDIAG
C...  Decomposition and forward substitution:

      BET = 1.0 / B( 1 )
      U( 1 ) = BET * D( 1 )

      DO J = 2, N   
        GAM( J ) = BET * E( J - 1 )
        BET = 1.0 / ( B( J ) - A( J ) * GAM( J ) )
        U( J ) = BET * ( D( J ) - A( J ) * U( J - 1) )
      END DO

C...Back-substitution:

      DO J = N - 1, 1, -1  
        U( J ) = U( J ) - GAM( J + 1 ) * U( J + 1 )
      END DO

      RETURN
      END SUBROUTINE TRIDIAG
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE TWOSTR ( NLEVEL, MU, RSFC, TAUU, OMU, GU,
     &                    FDR, FUP, FDN, EDR, EUP, EDN )

C*********************************************************************
C
C TWO-STREAM EQUATIONS FOR MULTIPLE LAYERS
C   based on equations from Toon et al.,  Journal of Geophysical Research
C   Volume 94, #D13  Nov. 20, 1989 Issue 
C   programmed by:  Kathleen G. Mosher
C   for Sasha Madronich, N.C.A.R. A.C.D.
C
C Now it contains 9 two-stream methods to choose from.
C   programmed  on 05.26.94 by:  Irina V.Petropavlovskikh
C   for Sasha Madronich, N.C.A.R. A.C.D.
C
C*********************************************************************

      IMPLICIT NONE

C...........PARAMETERS and their descriptions

      INTEGER, PARAMETER :: KZ = 200             ! maximum levels
      INTEGER, PARAMETER :: NROWS = 2 * KZ
      
      REAL, PARAMETER :: EPS = 1.0E-3
      REAL, PARAMETER :: PRECIS = 1.0E-7
      REAL, PARAMETER :: PI = 3.1415926535898

C...........ARGUMENTS and their descriptions

      INTEGER NLEVEL          ! number of levels

      REAL    MU              ! cosine of solar zenith angle
      REAL    RSFC            ! surface albedo
      REAL    EDN ( KZ )      ! 
      REAL    EDR ( KZ )      ! 
      REAL    EUP ( KZ )      ! 
      REAL    FDN ( KZ )      ! 
      REAL    FDR ( KZ )      ! 
      REAL    FUP ( KZ )      ! 
      REAL    GU  ( KZ )      ! unscaled asymmetry factor
      REAL    OMU ( KZ )      ! unscaled single scattering albedo
      REAL    TAUU( KZ )      ! unscaled optical depth of each layer


C...........LOCAL VARIABLES and their descriptions:

      INTEGER I
      INTEGER J
      INTEGER LEV
      INTEGER MROWS
      INTEGER NLAYER      ! number of layers in the atmosphere
      INTEGER ROW

      REAL DIVISR
      REAL DN
      REAL EXPON
      REAL EXPON0
      REAL EXPON1
      REAL F
      REAL FDN0
      REAL G
      REAL GAM1
      REAL GAM2
      REAL GAM3
      REAL GAM4
      REAL OM
      REAL PIFS
      REAL SSFC
      REAL TAU
      REAL TAUC
      REAL TAUG
      REAL TEMP
      REAL TEMPG
      REAL UP
      REAL BGAM ( KZ )
      REAL CDN  ( KZ )
      REAL CDNTN( KZ )
      REAL CUP  ( KZ )
      REAL CUPTN( KZ )
      REAL E1   ( KZ )
      REAL E2   ( KZ )
      REAL E3   ( KZ )
      REAL E4   ( KZ )
      REAL LAM  ( KZ )
      REAL MU1  ( KZ )
      REAL TAUN ( KZ )
      REAL A( NROWS )
      REAL B( NROWS )
      REAL D( NROWS )
      REAL E( NROWS )
      REAL Y( NROWS )

C...For calculations of Associated Legendre Polynomials for GAMA1,2,3,4
C    in delta-function, modified quadrature, hemispheric constant,
C    Hybrid modified Eddington-delta function metods, p633,Table1.
C    W.E.Meador and W.R.Weaver, GAS,1980,v37,p.630
C    W.J.Wiscombe and G.W. Grams, GAS,1976,v33,p2440, 
C...uncomment the following two lines and the appropriate statements further
C     down.
CCC     REAL YLM0, YLM2, YLM4, YLM6, YLM8, YLM10, YLM12, YLMS, BETA0,
CCC    &     BETA1, BETAn, amu1, subd

C*********************************************************************
C     begin body of subroutine TWOSTR

C...initial conditions:  pi*solar flux = 1;  diffuse incidence = 0

      PIFS = 1.0
      FDN0 = 0.0

      NLAYER = NLEVEL - 1

C...compute coefficients for each layer:
C...   GAM1 - GAM4 = 2-stream coefficients, different for
C...                 different approximations
C...   EXPON0 = calculation of e when TAU is zero
C...   EXPON1 = calculation of e when TAU is TAUN
C...   CUP and CDN = calculation when TAU is zero
C...   CUPTN and CDNTN = calc. when TAU is TAUN
C...   DIVISR = prevents division by zero

      TAUC = 0.
      DO I = 1, NLAYER

        G   = GU  ( I )
        TAU = TAUU( I )
        OM  = OMU ( I )

C...stay away from 1 by precision.  For g, also stay away from -1

        TEMPG = AMIN1( ABS( G ), 1.0 - PRECIS )
        G = SIGN( TEMPG, G )
        OM = AMIN1( OM, 1.0 - PRECIS )

C...delta-scaling. Have to be done for delta-Eddington approximation, 
C...  delta discrete ordinate, Practical Improved Flux Method, delta function,
C...  and Hybrid modified Eddington-delta function methods approximations

        F = G * G
        G = ( G - F ) / ( 1.0 - F )
        TAUN( I ) = ( 1.0 - OM * F ) * TAU
        OM = ( 1.0 - F ) * OM / ( 1.0 - OM * F )

C...the following gamma equations are from pg 16,289, Table 1

C...Eddington approximation(Joseph et al., 1976, JAS, 33, 2452):

        GAM1 =  ( 7.0 - OM * ( 4.0 + 3.0 * G ) ) / 4.0
        GAM2 = -( 1.0 - OM * ( 4.0 - 3.0 * G ) ) / 4.0
        GAM3 =  ( 2.0 - 3.0 * G * MU ) / 4.0
        GAM4 = 1.0 - GAM3

C...quadrature (Liou, 1973, JAS, 30, 1303-1326; 1974, JAS, 31, 1473-1475):
C        GAM1 = 1.7320508 * ( 2.0 - OM * ( 1.0 + G ) ) / 0.2
C        GAM2 = 1.7320508 * OM * ( 1.0 - G ) / 2.0
C        GAM3 = ( 1.0 - 1.7320508 * G * MU ) / 2.0
C        GAM4 = 1.0 - GAM3
         
C...hemispheric mean (Toon et al., 1089, JGR, 94, 16287):
C        GAM1 = 2.0 - OM * ( 1.0 + G )
C        GAM2 = OM * ( 1.0 - G )
C        GAM3 = ( 2.0 - G * MU ) / 4.0
C        GAM4 = 1.0 - GAM3

C...PIFM  (Zdunkovski et al.,1980, Conrib.Atmos.Phys., 53, 147-166):
C        GAM1 = 0.25 * ( 8.0 - OM * ( 5.0 + 3.0 * G ) )
C        GAM2 = 0.75 * OM * ( 1.0 - G )
C        GAM3 = 0.25 * ( 2.0 -3.0 * G * MU )
C        GAM4 = 1.0 - GAM3

C...delta discrete ordinates  (Schaller, 1979, Contrib.Atmos.Phys, 52, 17-26):
C        GAM1 = 0.5 * 1.7320508 * ( 2.0 - OM * ( 1.0 + G ) )
C        GAM2 = 0.5 * 1.7320508 * OM * ( 1.0 - G )
C        GAM3 = 0.5 * (1.0 - 1.7320508 * G * MU )
C        GAM4 = 1.0 - GAM3

C...Calculations of Associated Legendre Polynomials for GAMA1,2,3,4
C...  in delta-function, modified quadrature, hemispheric constant,
C...  Hybrid modified Eddington-delta function metods, p633,Table1.
C...  W.E.Meador and W.R.Weaver, GAS,1980,v37,p.630
C...  W.J.Wiscombe and G.W. Grams, GAS,1976,v33,p2440
C        YLM0 = 2.0
C        YLM2 = -3.0 * G * MU
C        YLM4 = 0.875 * G**3 * MU * ( 5.0 * MU**2 - 3.0 )
C        YLM6 = -0.171875 * G**5 * MU
C     &       * ( 15.0 - 70.0 * MU**2 + 63.0 * MU**4 )
C        YLM8 = 0.073242 * G**7 * MU
C     &       * ( -35.0 + 315.0 * MU**2 - 693.0 * MU**4 + 429.0 * MU**6 )
C        YLM10 = -0.008118 * G**9 * MU
C     &        * ( 315.0 - 4620.0 * MU**2 + 18018.0 * MU**4 - 25740.0
C     &        * MU**6 + 12155.0 * MU**8 )
C        YLM12 = 0.003685 * G**11 * MU
C     &        * ( -693.0 + 15015.0 * MU**2 - 90090.0 * MU**4 + 218790.0
C     &        * MU**6 - 230945.0 * MU**8 + 88179.0 * MU**10 )
C        YLMS = YLM0 + YLM2 + YLM4 + YLM6 + YLM8 + YLM10 + YLM12
C        YLMS = 0.25 * YLMS
C        BETA0 = YLMS
C
C        AMU1 = 1.0 / 1.7320508
C        YLM0 = 2.0
C        YLM2 = -3.0 * G * AMU1
C        YLM4 = 0.875 * G**3 * AMU1 * ( 5.0 * AMU1**2 - 3.0 )
C        YLM6 = -0.171875 * G**5 * AMU1
C     &       * ( 15.0 - 70.0 * AMU1**2 + 63.0 * AMU1**4 )
C        YLM8 = 0.073242 * G**7 * AMU1
C     &       * ( -35.0 + 315.0 * AMU1**2 - 693.0 * AMU1**4
C     &       + 429.0 * AMU1**6 )
C        YLM10 = -0.008118 * G**9 * AMU1
C     &        * ( 315.0 - 4620.0 * AMU1**2 + 18018.0 * AMU1**4
C     &        - 25740.0 * AMU1**6 + 12155.0 * AMU1**8 )
C        YLM12 = 0.003685 * G**11 * AMU1
C     &        * ( -693.0 + 15015.0 * AMU1**2 - 90090.0 * AMU1**4
C     &        + 218790.0 * AMU1**6 - 230945.0 * AMU1**8
C     &        + 88179.0 * AMU1**10 )
C        YLMS = YLM0 + YLM2 + YLM4 + YLM6 + YLM8 + YLM10 + YLM12
C        YLMS = 0.25 * YLMS
C        BETA1 = YLMS
C
C        BETAN = 0.25 * (2.0 - 1.5 * G - 0.21875 * G**3 - 0.085938 * G**5
C     &        - 0.045776 * G**7 )

C...Hybrid modified Eddington-delta function(Meador and Weaver,1980,JAS,37,630):
C        SUBD = 4.0 * (1.0 - G * G * ( 1.0 - MU ) )
C        GAM1 = ( 7.0 - 3.0 * G * G - OM * ( 4.0 + 3.0 * G )
C     &       + OM * G * G * ( 4.0 * BETA0 + 3.0 * G ) ) / SUBD
C        GAM2 = -( 1.0 - G * G - OM * ( 4.0 - 3.0 * G )
C     &       - OM * G * G * ( 4.0 * BETA0 + 3.0 * G - 4.0 ) ) / SUBD
C        GAM3 = BETA0
C        GAM4 = 1.0 - GAM3

C...delta function  (Meador, and Weaver, 1980, JAS, 37, 630):
C        GAM1 = ( 1.0 - OM * ( 1.0 - BETA0 ) ) / MU
C        GAM2 = OM * BETA0 / MU
C        GAM3 = BETA0
C        GAM4 = 1.0 - GAM3

C...modified quadrature (Meador, and Weaver, 1980, JAS, 37, 630):
C        GAM1 = 1.7320508 * ( 1.0 - OM * (1.0 - BETA1 ) )
C        GAM2 = 1.7320508 * OM * BETA1
C        GAM3 = BETA0
C        GAM4 = 1.0 - GAM3

C...hemispheric constant (Toon et al., 1989, JGR, 94, 16287):
C        GAM1 = 2.0 * (1.0 - OM * ( 1.0 - BETAN ) )
C        GAM2 = 2.0 * OM * BETAN
C        GAM3 = BETA0
C        GAM4 = 1.0 - GAM3

C...save mu1 for use in converting irradiance to actinic flux

        MU1( I ) = ( 1 - OM ) / ( GAM1 - GAM2 )

C...lambda = pg 16,290 equation 21
C...  big gamma = pg 16,290 equation 22
 
        LAM ( I ) = SQRT( GAM1 * GAM1 - GAM2 * GAM2 )
        BGAM( I ) = ( GAM1 - LAM( I ) ) / GAM2

        EXPON = EXP( -LAM( I ) * TAUN( I ) )

C...e1 - e4 = pg 16,292 equation 44
         
        E1( I ) = 1.0 + BGAM( I ) * EXPON
        E2( I ) = 1.0 - BGAM( I ) * EXPON
        E3( I ) = BGAM( I ) + EXPON
        E4( I ) = BGAM( I ) - EXPON

C...the following sets up for the C equations 23, and 24
C...  found on page 16,290
C...  prevent division by zero (if LAMBDA=1/MU, shift 1/MU^2 by EPS = 1.E-3
C...  which is approx equiv to shifting MU by 0.5*EPS* (MU)**3

        EXPON0 = EXP( -( TAUC ) / MU )
        EXPON1 = EXP( -( TAUC + TAUN( I ) ) / MU )

        DIVISR = LAM( I ) * LAM( I ) - 1.0 / ( MU * MU )
        TEMP   = AMAX1( EPS, ABS( DIVISR ) )
        DIVISR = SIGN( TEMP, DIVISR )

        UP = OM * PIFS
     &     * ( ( GAM1 - 1.0 / MU ) * GAM3 + GAM4 * GAM2 ) / DIVISR
        DN = OM * PIFS
     &     * ( ( GAM1 + 1.0 / MU ) * GAM4 + GAM2 * GAM3 ) / DIVISR
         
C...cup and cdn are when tau is equal to zero
C...  cuptn and cdntn are when tau is equal to taun

        CUP  ( I ) = UP * EXPON0
        CDN  ( I ) = DN * EXPON0
        CUPTN( I ) = UP * EXPON1
        CDNTN( I ) = DN * EXPON1

        TAUC = TAUC + TAUN( I )

      END DO

C...set up matrix
C...  ssfc = pg 16,292 equation 37  where pi Fs is one (unity).

      SSFC = RSFC * MU * EXP( -TAUC / MU ) * PIFS

C...MROWS = the number of rows in the matrix

      MROWS = 2 * NLAYER     
      
C...the following are from pg 16,292  equations 39 - 43.
C...   set up first row of matrix:

      I = 1
      A( 1 ) = 0.0
      B( 1 ) =  E1( I )
      D( 1 ) = -E2( I )
      E( 1 ) = FDN0 - CDN( I )

      ROW = 1

C...set up odd rows 3 thru (MROWS - 1):

      I = 0
      DO ROW = 3, MROWS - 1, 2
        I = I + 1
        A( ROW ) = E2( I ) * E3( I ) - E4( I ) * E1( I )
        B( ROW ) = E1( I ) * E1( I + 1 ) - E3( I ) * E3( I + 1 )
        D( ROW ) = E3( I ) * E4( I + 1 ) - E1( I ) * E2( I + 1 )
        E( ROW ) = E3( I ) * ( CUP( I + 1 ) - CUPTN( I ) )
     &           + E1( I ) * ( CDNTN( I ) - CDN( I + 1 ) )
      END DO

C...set up even rows 2 thru (MROWS - 2): 

      I = 0
      DO ROW = 2, MROWS - 2, 2
        I = I + 1
        A( ROW ) = E2( I + 1 ) * E1( I ) - E3( I ) * E4( I + 1 )
        B( ROW ) = E2( I ) * E2( I + 1 ) - E4( I ) * E4( I + 1 )
        D( ROW ) = E1( I + 1) * E4( I + 1 ) - E2( I + 1 ) * E3( I + 1 )
        E( ROW ) = ( CUP( I + 1 ) - CUPTN( I ) ) * E2( I + 1 )
     &           - ( CDN( I + 1 ) - CDNTN( I ) ) * E4( I + 1 )
      END DO

C...set up last row of matrix at MROWS:

      ROW = MROWS
      I   = NLAYER
      
      A( ROW ) = E1( I ) - RSFC * E3( I )
      B( ROW ) = E2( I ) - RSFC * E4( I )
      D( ROW ) = 0.0
      E( ROW ) = SSFC - CUPTN( I ) + RSFC * CDNTN( I )

C...solve tri-diagonal matrix:

      CALL TRIDIAG ( A, B, D, E, Y, MROWS )

C...unfold solution of matrix, compute output fluxes:

      ROW = 1 
      LEV = 1
      J   = 1
      TAUG = 0.0
      
C...the following equations are from pg 16,291  equations 31 & 32

      FDR( LEV ) = 1.0
      EDR( LEV ) = MU
      EDN( LEV ) = FDN0
      EUP( LEV ) = Y( ROW ) * E3( J ) - Y( ROW + 1 ) * E4( J )
     &           + CUP( J )
      FDN( LEV ) = EDN( LEV ) / MU1( LEV )
      FUP( LEV ) = EUP( LEV ) / MU1( LEV )

      DO LEV = 2, NLAYER + 1
        TAUG = TAUG + TAUN( J )
        FDR( LEV ) = EXP( -TAUG / MU )
        EDR( LEV ) = MU * FDR( LEV )
        EDN( LEV ) = Y( ROW ) * E3( J ) + Y( ROW + 1 ) * E4( J )
     &             + CDNTN( J )
        EUP( LEV ) = Y( ROW ) * E1( J ) + Y( ROW + 1 ) * E2( J )
     &             + CUPTN( J )
        FDN( LEV ) = EDN( LEV ) / MU1( J )
        FUP( LEV ) = EUP( LEV ) / MU1( J )

        ROW = ROW + 2
        J = J + 1
      END DO

      RETURN
      END SUBROUTINE TWOSTR
      END MODULE PHOT_MOD
