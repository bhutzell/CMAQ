
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
      SUBROUTINE RESCLD ( CGRID, JDATE, JTIME, TSTEP,
     &                    N_SPC_WDEP, WDEP_MAP, RESTRANS,
     &                    WCBAR_RS, WETFAC_RS, ALFA0_RS, ALFA3_RS )

C-----------------------------------------------------------------------
C  FUNCTION: Resolved-scale CLOUD processor Models-3 science process:
C
C  This function only obtains the cloud properties necessary for MCHEM.
C
C  Calls the following subroutines:  SCAVWDEP
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
      USE ASX_DATA_MOD,  ONLY: MET_DATA
      USE CENTRALIZED_IO_MODULE

      IMPLICIT NONE

C...........Includes:

      INCLUDE SUBST_CONST                ! constants
      INCLUDE SUBST_FILES_ID             ! file name parameters

C...........Arguments:
      REAL, POINTER            :: CGRID( :,:,:,: )   ! concentrations
      INTEGER, INTENT( IN )    :: JDATE              ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN )    :: JTIME              ! current model time, coded HHMMSS
      INTEGER, INTENT( IN )    :: TSTEP( 3 )         ! model time steps, coded HHMMSS
      INTEGER, INTENT( IN )    :: N_SPC_WDEP         ! # of wet deposition species
      INTEGER, INTENT( IN )    :: WDEP_MAP( : )      ! wet deposition species map to CGRID
      REAL,    INTENT( OUT )   :: RESTRANS( :,: )    ! resolved cloud transmissivity
      REAL(8), INTENT( OUT )   :: WCBAR_RS( :,:,: )  ! liq water content of cloud (kg/m3)
      REAL(8), INTENT( OUT )   :: WETFAC_RS( :,:,: ) ! wet deposition rate coefficient
      REAL(8), INTENT( OUT )   :: ALFA0_RS( :,:,: )  ! aitken mode number scavenging coef
      REAL(8), INTENT( OUT )   :: ALFA3_RS( :,:,: )  ! aitken mode mass scavenging coef

      CHARACTER( 120 ) :: XMSG  = ' '    ! Exit status message

C...........Parameters:
      REAL, PARAMETER :: PERMIN_TO_PERSEC = 1.0 / 60.0
      REAL, PARAMETER :: GPKG = 1.0E+03  ! g/kg
      REAL, PARAMETER :: M2PHA = 1.0E+04 ! 1 hectare = 1.0e4 m**2
      REAL, PARAMETER :: M2PHA_OVER_GPKG = M2PHA / GPKG

C Number of species in CGRID
      INTEGER, SAVE :: MXSPCS


C...........Local Variables:

      LOGICAL, SAVE :: FIRSTIME = .TRUE.   ! flag for first pass thru

      CHARACTER( 16 ), SAVE :: PNAME = 'RESCLD'    ! process name
      CHARACTER( 16 ), SAVE ::  VNAME_RN

      INTEGER       COL                 ! column loop counter
      INTEGER       ROW                 ! row loop counter
      INTEGER       LAY                 ! layer loop counter
      INTEGER       FINI                ! ending position
      INTEGER       MDATE               ! process date (yyyyddd)
      INTEGER, SAVE :: MSTEP            ! met file time step (hhmmss)
      INTEGER       MTIME               ! process time (hhmmss)
      INTEGER, SAVE :: SDATE            ! met file start date
      INTEGER       SPC                 ! liquid species loop counter
      INTEGER       STRT                ! starting position
      INTEGER, SAVE :: STIME            ! met file start time
      INTEGER       TCLD                ! cloud lifetime (sec)
      INTEGER       VAR                 ! variable loop counter
      INTEGER       ALLOCSTAT           ! memory allocation status

      REAL          AIRM                ! total airmass (mol/m2) in cloudy air
      REAL          ALFA0               ! aitken mode number scavenging coef
      REAL          ALFA2               ! aitken mode sfc area scavenging coef
      REAL          ALFA3               ! aitken mode mass scavenging coef
      REAL          CTHK1               ! cloud thickness (m)
      REAL          METSTEP             ! timestep on the met file (hr)
      SAVE          METSTEP
      REAL          PBARC               ! mean cloud pressure (Pa)
      REAL          PRATE1              ! storm rainfall rate (mm/hr)
      REAL          QCRGCOL             ! vert column integrated liquid water content
      REAL          QCRISGCOL           ! vert column integrated total water content
      REAL          QRSGCOL             ! vert column integrated precip content
      REAL          QCICOL              ! vert column integrated cloud content
      REAL          RAIN                ! non-conv rainfall rate (mm/hr)
      REAL          REMOVAC             ! variable storing H+ deposition
      REAL          TAUCLD              ! cloud lifetime (sec)
      REAL          TBARC               ! mean cloud temp (K)
      REAL          WCBAR               ! liq water content of cloud (kg/m3)
      REAL          WPBAR               ! precipitation water content (kg/m3)
      REAL          WTBAR               ! total water content of cloud (kg/m3)
      REAL          LWP, CLOD

      REAL, ALLOCATABLE, SAVE :: POLC ( : )   ! incloud conc (mol/mol)
      REAL, ALLOCATABLE, SAVE :: CEND ( : )   ! ending conc (mol/mol)
      REAL, ALLOCATABLE, SAVE :: REMOV( : )   ! moles/m2 or mm*mol/lit scavenged

      REAL          RN   ( NCOLS, NROWS ) ! non-convective rainfall (cm)
      REAL          DENS ( NCOLS, NROWS, NLAYS )  ! air density (kg/m3)
      REAL          DZZ  ( NCOLS, NROWS, NLAYS )  ! layer thickness (m)
      REAL          PRES ( NCOLS, NROWS, NLAYS )  ! air pressure (Pa)
      REAL          QC   ( NCOLS, NROWS, NLAYS )  ! cloud water content (kg/kg)
      REAL          QG   ( NCOLS, NROWS, NLAYS )  ! graupel content (kg/kg)
      REAL          QI   ( NCOLS, NROWS, NLAYS )  ! ice content (kg/kg)
      REAL          QR   ( NCOLS, NROWS, NLAYS )  ! rain water content (kg/kg)
      REAL          QS   ( NCOLS, NROWS, NLAYS )  ! snow content (kg/kg)
      REAL          TA   ( NCOLS, NROWS, NLAYS )  ! air temperature (K)
      REAL          ZF   ( NCOLS, NROWS, NLAYS )  ! level/layer-face height (m)

C Gridded meteorology data:
C Latitude and longitude for zenith angle calculation: Golam Sarwar * July 1, 2011 
      REAL          COSZ                            ! local cosine of zenith angle
      REAL          JH2O2                           ! H2O2 photolysis rate, 1/min 
      REAL          JHNO3                           ! HNO3 photolysis rate, 1/min 
C      LOGICAL       DARK                            ! DARK = TRUE is night,  DARK = FALSE is day

C...........External Functions:

      INTERFACE
        SUBROUTINE SCAVWDEP ( JDATE, JTIME, WTBAR, WCBAR, TBARC, PBARC,
     &                        CTHK1, AIRM, PRATE1, TAUCLD, POLC, CEND,
     &                        REMOV, REMOVAC, ALFA0, ALFA2, ALFA3 )
           INTEGER, INTENT( IN )  :: JDATE, JTIME
           REAL,    INTENT( IN )  :: WTBAR, WCBAR, TBARC, PBARC,
     &                               CTHK1, AIRM, PRATE1, TAUCLD
           REAL,    INTENT( IN )  :: POLC ( : )
           REAL,    INTENT( OUT ) :: REMOVAC
           REAL,    INTENT( OUT ) :: CEND( : ), REMOV( : )
           REAL,    INTENT( OUT ) :: ALFA0, ALFA2, ALFA3
        END SUBROUTINE SCAVWDEP
      END INTERFACE
C-----------------------------------------------------------------------

C...Initialization

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

        IF (RNA_AVAIL) THEN
           VNAME_RN = 'RNA'
        ELSE
           VNAME_RN = 'RN'
        END IF

C...store met file time, date, and step information and compute
C...  the met timestep in hours

        SDATE = cio_model_sdate
        STIME = cio_model_stime
        MSTEP = file_tstep(f_met)

        METSTEP = FLOAT( TIME2SEC( MSTEP ) ) / 3600.0

        IF ( .not. QI_AVAIL ) THEN
          XMSG = 'Parameter QI (cloud ice) was not found on file '
     &           // MET_CRO_3D
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
          WRITE( LOGDEV, '(3(/10X,A),(/10X,3(A,1X)),(/10X,A))' )
     &           'YOU SHOULD VERIFY that the cloud microphysics scheme used',
     &           'in the Meteorological Model did not include ice/snow.  If',
     &           'it did, then you need to reprocess the meteorological data',
     &           'through MCIP and pass QI to file ',
     &           TRIM( MET_CRO_3D ), ' to avoid',
     &           'errors in the wet deposition.'
          WRITE( LOGDEV, '((/5X,A),/)' )
     &           'Processing will continue with QI set to ZERO.  <<---<<'
        END IF

        IF ( .not. QS_AVAIL ) THEN
          XMSG = 'Parameter QS (snow) was not found on file ' // MET_CRO_3D
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
          WRITE( LOGDEV, '(3(/10X,A),(/10X,3(A,1X)),(/10X,A))' )
     &           'YOU SHOULD VERIFY that the cloud microphysics scheme used',
     &           'in the Meteorological Model did not include ice/snow.  If',
     &           'it did, then you need to reprocess the meteorological data',
     &           'through MCIP and pass QS to file ',
     &           TRIM( MET_CRO_3D ), ' to avoid',
     &           'errors in the wet deposition.'
          WRITE( LOGDEV, '((/5X,A),/)' )
     &           'Processing will continue with QS set to ZERO.  <<--<<'
        END IF

        MXSPCS = N_GC_SPCD + N_AE_SPC + N_NR_SPC + N_TR_SPC

        ALLOCATE ( CEND ( MXSPCS ),
     &             POLC ( MXSPCS ),
     &             REMOV( MXSPCS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating CEND, POLC or REMOV'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

      END IF  ! FIRSTIME

      MDATE = JDATE
      MTIME = JTIME

C...set the cloud lifetime (=adv timestep)

      TCLD = TIME2SEC( TSTEP( 2 ) )
      TAUCLD = REAL( TCLD )

C...set time to the midpoint of this timestep for data interpolation

      CALL NEXTIME ( MDATE, MTIME, SEC2TIME( TCLD / 2 ) )

C...Actual Science Process (loop on internal process time steps):
C...  Interpolate time dependent layered input variables
C...  (reading those variables for which it is necessary)

C...  Get ambient temperature (K)

      call interpolate_var ('TA', mdate, mtime, TA)

C...Get resolved cloud water mixing ratio (kg H2O / kg air)

      call interpolate_var ('QC', MDATE, MTIME, QC )

C...Get resolved rain water mixing ratio (kg H2O / kg air)

      call interpolate_var ('QR', MDATE, MTIME, QR )

C...read resolved ice mixing ratio (kg H2O / kg air) from the met
C...  file if it is available

      IF ( QI_AVAIL ) THEN

        call interpolate_var ('QI', MDATE, MTIME, QI )

      ELSE

        QI = 0.0    ! otherwise fill the array with zeros

      END IF

C...read resolved snow mixing ratio (kg H2O / kg air) from the met
C...  file if it is available

      IF ( QS_AVAIL ) THEN

        call interpolate_var ('QS', MDATE, MTIME, QS )

      ELSE

        QS = 0.0    ! otherwise fill the array with zeros

      END IF

C...read graupel mixing ratio (kg H2O / kg air) from the met
C...  file if it is available

      IF ( QG_AVAIL ) THEN

        call interpolate_var ('QG', MDATE, MTIME, QG )

      ELSE

        QG = 0.0    ! otherwise fill the array with zeros

      END IF

C...Get level heights / layer faces (m)

      call interpolate_var ('ZF', MDATE, MTIME, ZF )

C...Get pressure (Pa)

      call interpolate_var ('PRES', MDATE, MTIME, PRES )

C...Get air density (kg/m3)

      call interpolate_var ('DENS', MDATE, MTIME, DENS )

C...compute layer thicknesses (m)

      DO ROW = 1, NROWS
        DO COL = 1, NCOLS
          DZZ( COL, ROW, 1 ) = ZF( COL, ROW, 1 )
          DO LAY = 2, NLAYS
            DZZ( COL, ROW, LAY ) = ZF( COL, ROW, LAY )
     &                           - ZF( COL, ROW, LAY - 1 )
          END DO
        END DO
      END DO

C...advance the MDATE and MTIME to the next time on the met file
C...  to get ready to read the precipitation amounts.
C...  Precipitation data WILL NOT BE INTERPOLATED!  Precipitation data
C...  on the input file are amounts within the metfiles timestep.

      IF ( .NOT. CURRSTEP( JDATE, JTIME, SDATE, STIME, MSTEP,
     &                     MDATE, MTIME ) ) THEN
        XMSG = 'Cannot get step-starting date and time'
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
      END IF

      CALL NEXTIME ( MDATE, MTIME, MSTEP )  ! set mdate:mtime to the hour

C...Get nonconvective precipitation amount (cm)

      call interpolate_var (VNAME_RN, MDATE, MTIME, RN )

C.. Initialize output arrays
      WCBAR_RS  = 0.
      WETFAC_RS = 0.
      ALFA0     = 0.
      ALFA3     = 0.

C...Loop through all grid cells

      DO ROW = 1, NROWS
        DO COL = 1, NCOLS

C...Convert the rainfall into a rainfall rate (mm/hr)

          RAIN = 10.0 * RN( COL, ROW ) / METSTEP

          IF ( RAIN .LT. 0.0 ) THEN
            XMSG = 'NEGATIVE RAIN...PROBABLE BAD MET DATA...'
     &              // MET_CRO_2D
            CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
          END IF

C...calculate the integrated column cloud+rain water content
C... JP & SJR:  treat graupel as rainwater until we have a more
C...            advanced cloud microphysics scheme
C... include layer thickness in the column integration

          QCRGCOL   = 0.0
          QCRISGCOL = 0.0
          QRSGCOL   = 0.0
          QCICOL    = 0.0
          DO LAY = 1, NLAYS
            QC( COL, ROW, LAY ) = MAX( QC( COL, ROW, LAY ), 0.0 )
            QR( COL, ROW, LAY ) = MAX( QR( COL, ROW, LAY ), 0.0 )
            QI( COL, ROW, LAY ) = MAX( QI( COL, ROW, LAY ), 0.0 )
            QS( COL, ROW, LAY ) = MAX( QS( COL, ROW, LAY ), 0.0 )
            QG( COL, ROW, LAY ) = MAX( QG( COL, ROW, LAY ), 0.0 )
            QCRGCOL   = QCRGCOL   + DENS( COL, ROW, LAY )
     &                * DZZ( COL, ROW, LAY )
     &                * ( QC( COL, ROW, LAY ) + QR( COL, ROW, LAY )
     &                +   QG( COL, ROW, LAY ) )
            QCRISGCOL = QCRISGCOL + DENS( COL, ROW, LAY )
     &                * DZZ( COL, ROW, LAY )
     &                * ( QC( COL, ROW, LAY ) + QR( COL, ROW, LAY )
     &                +   QI( COL, ROW, LAY ) + QS( COL, ROW, LAY )
     &                +   QG( COL, ROW, LAY ) )
            QRSGCOL   = QRSGCOL   + DENS( COL, ROW, LAY )
     &                * DZZ( COL, ROW, LAY )
     &                * ( QR( COL, ROW, LAY ) + QS( COL, ROW, LAY )
     &                +   QG( COL, ROW, LAY ) )
            QCICOL    = QCICOL + DENS( COL, ROW, LAY )
     &                * DZZ( COL, ROW, LAY )
     &                * ( QC( COL, ROW, LAY ) + QI( COL, ROW, LAY ) )
          END DO

C...Calculate the cloud optical depth using a formula derived from
C...  Stephens (1978), JAS(35), pp2111-2132.
C...  only calculate the cloud optical depth when the liquid water
C...  path is >= 10 g/m2

          LWP = QCICOL * 1000.0  ! converts to g/m2
          IF ( LWP .GE. 10.0 ) THEN
             CLOD = 10.0**( 0.2633 + 1.7095 * LOG( LOG10( LWP ) ) )
          ELSE
             CLOD = 0.0
          END IF

C...If no cloud or optical depth < 5, set clear sky values.
C...  (i.e. don't do anything)

          IF ( CLOD .GE. 5.0 ) THEN

             RESTRANS( COL, ROW ) = ( 5.0 - EXP ( -CLOD ) ) / ( 4.0 + 0.42 * CLOD )
              
          END IF

C...loop through layers

          IF ( QCRGCOL .GT. 0.0 ) THEN
            DO LAY = 1, NLAYS

C...Compute cloud quantities

              IF ( ( QC( COL, ROW, LAY ) + QR( COL, ROW, LAY )
     &               + QG( COL, ROW, LAY ) ) .GT. 0.00005 ) THEN

                TBARC = TA( COL, ROW, LAY )

                PBARC = PRES( COL, ROW, LAY )

                CTHK1 = DZZ( COL, ROW, LAY )

                AIRM = PBARC * CTHK1 * 1.0E3 / ( RDGAS * MWAIR * TBARC )

                WCBAR = ( QC( COL, ROW, LAY ) + QR( COL, ROW, LAY )
     &                +   QG( COL, ROW, LAY ) ) * DENS( COL, ROW, LAY )

                WTBAR = ( QC( COL, ROW, LAY ) + QR( COL, ROW, LAY )
     &                +   QI( COL, ROW, LAY ) + QS( COL, ROW, LAY ) 
     &                +   QG( COL, ROW, LAY ) ) * DENS( COL, ROW, LAY )

C...Compute precipitation flux
C...  include layer thickness in the calculation

                IF ( QRSGCOL .GT. 0.0 ) THEN
                  WPBAR = ( QR( COL, ROW, LAY ) + QS( COL, ROW, LAY )
     &                  +   QG( COL, ROW, LAY ) ) * DENS( COL, ROW, LAY )
                  PRATE1 = RAIN * WPBAR * CTHK1 / QRSGCOL  ! convert to mm/hr
                ELSE
                  PRATE1 = RAIN * WTBAR * CTHK1 / QCRISGCOL  ! convert to mm/hr
                END IF

C...Finally, get in-cloud pollutant concentrations in moles sp
C...  per mole air

                DO SPC = 1, NSPCSD
                  POLC ( SPC ) = CGRID( COL, ROW, LAY, SPC )
                  CEND ( SPC ) = POLC( SPC )
                  REMOV( SPC ) = 0.0
                END DO

C...perform scavenging and aqueous chemistry within the cloud
C...  currently, the concentrations are not affected
C...  this function is only used to obtain the scavenging parameters
C...  needed for mchem

                CALL SCAVWDEP ( JDATE, JTIME, WTBAR, WCBAR, TBARC,
     &                          PBARC, CTHK1, AIRM, PRATE1, TAUCLD,
     &                          POLC, CEND, REMOV, REMOVAC, ALFA0,
     &                          ALFA2, ALFA3 )

C.. Save output
                WCBAR_RS  ( COL, ROW, LAY ) = REAL( WCBAR, 8 )
                WETFAC_RS ( COL, ROW, LAY ) = REAL( WTBAR * 1000.0D0 * CTHK1 * 3600.0D0 /
     &                                           ( 1000.0D0 * MAX( 1.0D-20, PRATE1 ) ), 8 )
                WETFAC_RS ( COL, ROW, LAY ) = 1.0D0 /  WETFAC_RS ( COL, ROW, LAY )
                ALFA0_RS  ( COL, ROW, LAY ) = REAL( ALFA0, 8 )
                ALFA3_RS  ( COL, ROW, LAY ) = REAL( ALFA3, 8 )

              END IF        ! Sum( QC,QR,QG ) > 0.00005 ?
            END DO       ! lay
          END IF      ! QCRGCOL > 0 ?

        END DO   ! col
      END DO   ! row

      RETURN

      END
