
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

       SUBROUTINE CHEM( CGRID, JDATE, JTIME, TSTEP )


C**********************************************************************
C
C  Function: To control multi-phase chemistry calculations performed by
C            the Rosenbrock solver
C
C  Preconditions: None
C
C**********************************************************************

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE PHOT_MOD, Only: INIT_PHOT_SHARED, RJ     ! photolysis rate, in-line module
      USE AEROSOL_CHEMISTRY
      USE CENTRALIZED_IO_MODULE, ONLY : INTERPOLATE_VAR, OCEAN, SZONE

C.. Cloud data
      USE MCHEM_CLOUDS

C.. MCHEM Data
      USE MCHEM_DATA, ONLY: ALLOC_KPP_SPC, ALLOC_DIAGNOSTICS,
     &                      NSTEPS, NFAILS, pH_RS, pH_CV
      USE MCHEM_PARAMETERS, ONLY: NREACT_MCHEM => NREACT
      USE GAS_PARAMETERS, ONLY: NREACT_GAS => NREACT

      IMPLICIT NONE

C..Includes:

      INCLUDE SUBST_FILES_ID    ! CMAQ files
      INCLUDE SUBST_CONST       ! CMAQ constants
 
C..Arguments:

      REAL, POINTER :: CGRID( :,:,:,: )  ! Concentrations

      INTEGER JDATE                     ! Current date (YYYYDDD)
      INTEGER JTIME                     ! Current time (HHMMSS)
      INTEGER TSTEP( 3 )                ! Time step vector (HHMMSS)

C..Parameters:

      INTEGER, PARAMETER :: IZERO = 0                ! Integer zero

      REAL,    PARAMETER :: CONCMIN   = 1.0E-30         ! Minimum conc
      REAL,    PARAMETER :: CONCOFM   = 1.0E+06         ! conc. of M = 1E+06 ppm
      REAL,    PARAMETER :: PA2ATM    = 1.0 / STDATMPA  ! Pascal to atm conv fac
      REAL,    PARAMETER :: MAOMV     = MWAIR / MWWAT   ! Mol Wt of air over Mol Wt of water
      REAL,    PARAMETER :: QV_TO_PPM = CONCOFM * MAOMV ! factor to convert water wapor into ppm
C..External Functions:

C..Local Variables:
      LOGICAL, SAVE :: LFIRST    = .TRUE. ! Flag for first call to this subroutine

      INTEGER, SAVE :: NOXYZ           ! Total number of grid cells

      REAL,    SAVE :: AIRFC           ! Factor to convert gms air to ppm

      REAL( 8 )     :: CHEMSTEP   ! Chem integration interval (s)

      CHARACTER(  16 ) :: PNAME = 'MCHEM_DRIVER' ! Procedure name
      CHARACTER(  16 ) :: VNAME                  ! Name of I/O API data variable
      CHARACTER( 144 ) :: XMSG                   ! Message text

      INTEGER ALLOCSTAT       ! Allocate status code
      INTEGER NCSP            ! Mech no: 1=gas/day 2=gas/night
      INTEGER IPAR            ! Pointer for cell sort routine
      INTEGER IRVAL           ! Pointer for cell sort routine
      INTEGER IRXN            ! Reaction number
      INTEGER ISP             ! Species index
      INTEGER ISPOLD          ! Species number in original order
      INTEGER ISPNEW          ! Species number in new sorted order 
      INTEGER ITMSTEP         ! Chemistry integration interval (sec)
      INTEGER JPAR            ! Pointer for cell sort routine
      INTEGER JREORD          ! Index holder for sort routine
      INTEGER LVAL            ! Pointer for cell sort routine
      INTEGER MIDDATE         ! Date at time step midpoint
      INTEGER MIDTIME         ! Time at time step midpoint
      INTEGER NCELL           ! Index for number of cells
      INTEGER NRX             ! Index for number of reactions
      INTEGER SPC             ! Species loop index
      INTEGER STRT
      INTEGER FINI
      INTEGER IVAR

      INTEGER COL             ! Column index
      INTEGER LEV             ! Layer index
      INTEGER ROW             ! Row index

      REAL, ALLOCATABLE, SAVE :: SEAICE ( :, : )         ! fractional seaice cover, [-]

      REAL, ALLOCATABLE, SAVE :: DENS   ( :, :, : )      ! Cell density (Kg/m**3)
      REAL, ALLOCATABLE, SAVE :: PRES   ( :, :, : )      ! Cell pressure (Atm)
      REAL, ALLOCATABLE, SAVE :: QV     ( :, :, : )      ! Cell water vapor (Kg/Kg air)
      REAL, ALLOCATABLE, SAVE :: TA     ( :, :, : )      ! Cell temperature (K)

      REAL, ALLOCATABLE, SAVE :: SEAWATER_ZONE   ( :,: )          ! fractional area of OPEN+SURF

C.. Cloud chemistry related
      REAL( 8 ),    PARAMETER :: COEF1 = 7.33981D+15  ! Molec/cc to ppm conv factor 
      REAL( 8 )               :: VOL                  ! Cell air specific volume, m^3/kg
      REAL( 8 )               :: DENSI                ! Cell ambient MASS DENSITY in Kg/m^3
      REAL( 8 )               :: NA = 6.02214076D23   ! Avogadro constant [mol^-1]

C.. KPP related variables
      REAL( 8 )               :: TEMP
      REAL( 8 )               :: PRESS
      REAL( 8 )               :: CAIR
      REAL( 8 )               :: SEAWATER
      LOGICAL                 :: OPEN_WATER

C.. Scavenging related variables
      REAL(8)                 :: ALFA                 ! total (RS + CV) scavenging rate coefficient (1/s)

#ifdef mchem_stats
      CHARACTER( 16 ), SAVE              :: CTM_MCHEMSTATS_1 = 'CTM_MCHEMSTATS_1'
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: VSTATS( : )

      INTEGER, SAVE                      :: WSTEP = 0
      INTEGER  EDATE, ETIME
#endif

      INTERFACE
         SUBROUTINE HETCHEM_UPDATE_AERO( CGRID )
           REAL, POINTER :: CGRID( :,:,:,: )    !  species concentration in cell
         END SUBROUTINE HETCHEM_UPDATE_AERO      

         SUBROUTINE RESCLD ( CGRID, JDATE, JTIME, TSTEP,
     &                       N_SPC_WDEP, WDEP_MAP, RESTRANS,
     &                       WCBAR_RS, WETFAC_RS, ALFA0_RS, ALFA3_RS,
     &                       PRATE1_RS, RSCAV_RS, AIRMASS )
           IMPLICIT NONE
           REAL, POINTER            :: CGRID( :,:,:,: )
           INTEGER, INTENT( IN )    :: JDATE
           INTEGER, INTENT( IN )    :: JTIME
           INTEGER, INTENT( IN )    :: TSTEP( 3 )
           INTEGER, INTENT( IN )    :: N_SPC_WDEP
           INTEGER, INTENT( IN )    :: WDEP_MAP( : )
           REAL,    INTENT( OUT )   :: RESTRANS( :,: )
           REAL(8), INTENT( OUT )   :: WCBAR_RS( :,:,: )
           REAL(8), INTENT( OUT )   :: WETFAC_RS( :,:,: )
           REAL(8), INTENT( OUT )   :: ALFA0_RS( :,:,: )
           REAL(8), INTENT( OUT )   :: ALFA3_RS( :,:,: )
           REAL(8), INTENT( OUT )   :: PRATE1_RS( :,:,: )
           REAL(8), INTENT( OUT )   :: RSCAV_RS( :,:,:,: )
           REAL(8), INTENT( OUT )   :: AIRMASS( :,:,: )
         END SUBROUTINE RESCLD

         SUBROUTINE CONVCLD_ACM ( CGRID, JDATE, JTIME, TSTEP,
     &                            N_SPC_WDEP, WDEP_MAP, SUBTRANS,
     &                            WCBAR_CV, WETFAC_CV, ALFA0_CV, ALFA3_CV,
     &                            PRATE1_CV, RSCAV_CV, FRAC_CV )
           IMPLICIT NONE
           REAL, POINTER            :: CGRID( :,:,:,: )
           INTEGER, INTENT( IN )    :: JDATE
           INTEGER, INTENT( IN )    :: JTIME
           INTEGER, INTENT( IN )    :: TSTEP( 3 )
           INTEGER, INTENT( IN )    :: N_SPC_WDEP
           INTEGER, INTENT( IN )    :: WDEP_MAP( : )
           REAL,    INTENT( OUT )   :: SUBTRANS( :,:,: )
           REAL(8), INTENT( OUT )   :: WCBAR_CV( :,:,: )
           REAL(8), INTENT( OUT )   :: WETFAC_CV( :,:,: )
           REAL(8), INTENT( OUT )   :: ALFA0_CV( :,:,: )
           REAL(8), INTENT( OUT )   :: ALFA3_CV( :,:,: )
           REAL(8), INTENT( OUT )   :: PRATE1_CV( :,:,: )
           REAL(8), INTENT( OUT )   :: RSCAV_CV( :,:,:,: )
           REAL(8), INTENT( OUT )   :: FRAC_CV( :,:,: )
         END SUBROUTINE CONVCLD_ACM

         SUBROUTINE MCHEM_CHEM ( CGRID, TA, PRES, DENSI, OPEN_WATER_IN,
     &                           SEAWATER_IN, CAIR_IN, QV,
     &                           CHEMSTEP, LEV, COL, ROW )
           IMPLICIT NONE
           REAL, POINTER            :: CGRID( :,:,:,: )
           REAL(8), INTENT( IN )    :: TA
           REAL(8), INTENT( IN )    :: PRES
           REAL(8), INTENT( IN )    :: DENSI
           LOGICAL, INTENT( IN )    :: OPEN_WATER_IN
           REAL(8), INTENT( IN )    :: SEAWATER_IN
           REAL(8), INTENT( IN )    :: CAIR_IN
           REAL,    INTENT( IN )    :: QV
           REAL(8), INTENT( IN )    :: CHEMSTEP
           INTEGER, INTENT( IN )    :: LEV
           INTEGER, INTENT( IN )    :: COL
           INTEGER, INTENT( IN )    :: ROW
         END SUBROUTINE

         SUBROUTINE GAS_CHEM ( CGRID, TA, PRES, DENSI, OPEN_WATER_IN,
     &                         SEAWATER_IN, CAIR_IN, QV,
     &                         CHEMSTEP, LEV, COL, ROW )
           IMPLICIT NONE
           REAL, POINTER            :: CGRID( :,:,:,: )
           REAL(8), INTENT( IN )    :: TA
           REAL(8), INTENT( IN )    :: PRES
           REAL(8), INTENT( IN )    :: DENSI
           LOGICAL, INTENT( IN )    :: OPEN_WATER_IN
           REAL(8), INTENT( IN )    :: SEAWATER_IN
           REAL(8), INTENT( IN )    :: CAIR_IN
           REAL,    INTENT( IN )    :: QV
           REAL(8), INTENT( IN )    :: CHEMSTEP
           INTEGER, INTENT( IN )    :: LEV
           INTEGER, INTENT( IN )    :: COL
           INTEGER, INTENT( IN )    :: ROW
         END SUBROUTINE
      END INTERFACE

C**********************************************************************


#ifdef isam
      MSG = 'ERROR: Rosenbrock Chemistry Solver does not perform source apportionment.'
      WRITE(LOGDEV,'(A)')TRIM( MSG )
      MSG = 'Must use the EBI solver for the chemical mechanism'
      CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
#endif


      IF ( ( NREACT_MCHEM .EQ. 0 ) .OR. ( NREACT_GAS .EQ. 0 ) ) THEN
         CALL M3MESG( '*** WARNING: Gas-Chemistry processing bypassed!' )
         RETURN
      END IF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  On first call, call routines to set-up for Gear solver and 
c  set-up to do emissions here if that option is invoked
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN
         LFIRST   = .FALSE.

         NOXYZ = NCOLS * NROWS * NLAYS

C.. Get fractional seawater coverage from the OCEAN file.
         ALLOCATE( SEAWATER_ZONE( NCOLS, NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'ERROR allocating SEAWATER_ZONE'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
         END IF

         DO ROW = 1, NROWS
            DO COL = 1, NCOLS
                 SEAWATER_ZONE( COL,ROW ) =  OCEAN( COL,ROW ) + SZONE( COL,ROW )
             END DO
         END DO

         ALLOCATE( DENS( NCOLS, NROWS, NLAYS ), PRES( NCOLS, NROWS, NLAYS ),
     &             QV  ( NCOLS, NROWS, NLAYS ), TA  ( NCOLS, NROWS, NLAYS ),
     &             SEAICE( NCOLS, NROWS ) )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'ERROR allocating DENS, PRES, QV, TA, SEAICE'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
         END IF

         CALL ALLOC_KPP_SPC()

         CALL CL_SPCS_DEF()

C..Initialize shared photolysis data
         CALL INIT_PHOT_SHARED()

C..Allocate AE related pointers
         CALL ALLOC_AE_DATA()

C..Allocate wet deposition data
         CALL ALLOC_WDEP_DATA( JDATE, JTIME )

C..Allocate diagnostics
         CALL ALLOC_DIAGNOSTICS( JDATE, JTIME )

C..Open file for mchem stats if requested
#ifdef mchem_stats
         ALLOCATE( VSTATS( 9 ) )
         VSTATS( 1 ) = 'LWC_RS'
         VSTATS( 2 ) = 'LWC_CV'
         VSTATS( 3 ) = 'PRATE1_RS'
         VSTATS( 4 ) = 'PRATE1_CV'
         VSTATS( 5 ) = 'pH_RS'
         VSTATS( 6 ) = 'pH_CV'
         VSTATS( 7 ) = 'N_STEPS'
         VSTATS( 8 ) = 'N_FAILS'
         VSTATS( 9 ) = 'FRAC_CV'

         IF ( MYPE .EQ. 0 ) THEN

            EDATE = JDATE
            ETIME = JTIME
            CALL NEXTIME( EDATE, ETIME, TSTEP( 1 ) )

            SDATE3D = EDATE
            STIME3D = ETIME
            TSTEP3D = TSTEP( 1 )
            NVARS3D = 9
            NCOLS3D = GL_NCOLS
            NROWS3D = GL_NROWS
            NLAYS3D = NLAYS
            VNAME3D( 1 ) = 'LWC_RS'
            VNAME3D( 2 ) = 'LWC_CV'
            VNAME3D( 3 ) = 'PRATE1_RS'
            VNAME3D( 4 ) = 'PRATE1_CV'
            VNAME3D( 5 ) = 'pH_RS'
            VNAME3D( 6 ) = 'pH_CV'
            VNAME3D( 7 ) = 'N_STEPS'
            VNAME3D( 8 ) = 'N_FAILS'
            VNAME3D( 9 ) = 'FRAC_CV'
            VDESC3D( 1 ) = 'Liquid water content of resolved clouds'
            VDESC3D( 2 ) = 'Liquid water content of convective clouds'
            VDESC3D( 3 ) = 'Precipitation rate of resolved clouds'
            VDESC3D( 4 ) = 'Precipitation rate of convective clouds'
            VDESC3D( 5 ) = 'pH of resolved clouds'
            VDESC3D( 6 ) = 'pH of convective clouds'
            VDESC3D( 7 ) = 'Number of steps in KPP'
            VDESC3D( 8 ) = 'Number of rejected steps in KPP'
            VDESC3D( 9 ) = 'Convective cloud fraction'
            UNITS3D( 1 ) = 'kg/m3'
            UNITS3D( 2 ) = 'kg/m3'
            UNITS3D( 3 ) = 'mm/hr'
            UNITS3D( 4 ) = 'mm/hr'
            UNITS3D( 5 ) = '-'
            UNITS3D( 6 ) = '-'
            UNITS3D( 7 ) = '-'
            UNITS3D( 8 ) = '-'
            UNITS3D( 9 ) = '1'
            VTYPE3D( 1 ) = M3REAL
            VTYPE3D( 2 ) = M3REAL
            VTYPE3D( 3 ) = M3REAL
            VTYPE3D( 4 ) = M3REAL
            VTYPE3D( 5 ) = M3REAL
            VTYPE3D( 6 ) = M3REAL
            VTYPE3D( 7 ) = M3REAL
            VTYPE3D( 8 ) = M3REAL
            VTYPE3D( 9 ) = M3REAL
            IF ( .NOT. OPEN3( CTM_MCHEMSTATS_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// TRIM( CTM_MCHEMSTATS_1 ) // ' file'
               CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT2 )
            END IF

         END IF
#endif

      END IF      ! First call

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Set date and time to center of time step, get necessary physical
C  data, and get photolysis rates
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      MIDDATE = JDATE
      MIDTIME = JTIME
      ITMSTEP = TIME2SEC( TSTEP( 2 ) )
      CHEMSTEP = REAL( ITMSTEP, 8 )
      CALL NEXTIME( MIDDATE, MIDTIME, SEC2TIME( ITMSTEP / 2 ) )

C.. Get fractional seaice coverage from the METCRO2D file.
      CALL INTERPOLATE_VAR ('SEAICE', MIDDATE, MIDTIME, SEAICE)

C.. Get ambient temperature in K
      CALL INTERPOLATE_VAR ('TA', MIDDATE, MIDTIME, TA)

C.. Get specific humidity in Kg H2O / Kg air
      CALL INTERPOLATE_VAR ('QV', MIDDATE, MIDTIME, QV)

C.. Get ambient MASS DENSITY in Kg/m^3
      CALL INTERPOLATE_VAR ('DENS', MIDDATE, MIDTIME, DENS)

C.. Get pressure in Pascals
      CALL INTERPOLATE_VAR ('PRES', MIDDATE, MIDTIME, PRES)

C.. Get Heterogeneous Rates using Aerosol Surface Area. Also Store
C   a snapshot of the aerosol surface area so that it can be
C   appropriately updated after the solver finds a solution.

      CALL HETCHEM_RATES( TA, PRES, QV, CGRID, DENS )

C.. Reset wet deposition and pH array for current time step
      WET_DEP = 0.0D0
      pH_RS   = -9999.9D0
      pH_CV   = -9999.9D0
      NSTEPS  = 0.0D0
      NFAILS  = 0.0D0

C.. Convert CGRID species
C..  gas, non-reactive, and tracer to molar mixing ratio (by volume)
C..  aerosol mass concentration to moles of aerosol per mole of air by volume
C..  aerosol number concentration to # per mole of air by volume
C..  aerosol surface area to m2 per mole of air by volume
      CALL CONVERT_FORWARD( CGRID, DENS )

C.. Obtain wet deposition related data
C..  Resolved clouds
      CALL RESCLD ( CGRID, JDATE, JTIME, TSTEP,
     &              N_SPC_WDEP, WDEP_MAP, RESTRANS,
     &              WCBAR_RS, WETFAC_RS, ALFA0_RS, ALFA3_RS,
     &              PRATE1_RS, RSCAV_RS, AIRMASS )
C..  Convective clouds
      CALL CONVCLD_ACM ( CGRID, JDATE, JTIME, TSTEP,
     &                   N_SPC_WDEP, WDEP_MAP, SUBTRANS,
     &                   WCBAR_CV, WETFAC_CV, ALFA0_CV, ALFA3_CV,
     &                   PRATE1_CV, RSCAV_CV, FRAC_CV )


C.. Convert to molecules/cm3
      DO LEV = 1, NLAYS
        DO ROW = 1, NROWS
          DO COL = 1, NCOLS
            TEMP  = REAL( TA( COL,ROW,LEV ), dp )
            PRESS = REAL( PA2ATM * PRES( COL,ROW,LEV ), dp )
            CAIR  = ( PRESS/PA2ATM ) / ( TEMP * 8.314510 ) * 6.0221413D17
            ! Reactive gases
            STRT = GC_STRT
            FINI = GC_STRT + N_GC_SPC - 1
            DO SPC = STRT, FINI
              CGRID( COL,ROW,LEV,SPC ) = CGRID( COL,ROW,LEV,SPC ) * CAIR
            END DO
            ! Aerosols
            STRT = 1
            FINI = NQAE
            DO IVAR = STRT, FINI
              SPC = QAE( IVAR )
              CGRID( COL,ROW,LEV,SPC ) = CGRID( COL,ROW,LEV,SPC ) * CAIR
            END DO
            ! Non-reactive gases
            STRT = NR_STRT
            FINI = NR_STRT + N_NR_SPC - 1
            DO SPC = STRT, FINI
              CGRID( COL,ROW,LEV,SPC ) = CGRID( COL,ROW,LEV,SPC ) * CAIR
            END DO
            ! Trace gases
            STRT = TR_STRT
            FINI = TR_STRT + N_TR_SPC - 1
            DO SPC = STRT, FINI
              CGRID( COL,ROW,LEV,SPC ) = CGRID( COL,ROW,LEV,SPC ) * CAIR
            END DO
          END DO
        END DO
      END DO

      DO COL = 1, NCOLS
         DO ROW = 1, NROWS

            OPEN_WATER = .FALSE.
            SEAWATER = REAL(MAX ( 0.0, ( SEAWATER_ZONE( COL,ROW ) - SEAICE (COL,ROW) ) ), dp )
            IF ( SEAWATER .GT. 0.001D0 ) OPEN_WATER = .TRUE.

            DO LEV = 1, NLAYS

               TEMP     = REAL( TA( COL,ROW,LEV ), 8 )
               PRESS    = REAL( PA2ATM * PRES( COL,ROW,LEV ), 8 )
               DENSI    = REAL( DENS( COL,ROW,LEV ), 8 )
               CAIR     = REAL( ( PRESS/PA2ATM ) / ( TEMP * 8.314510 ) * 6.0221413D17, 8 )

C.. Check if any cloud is present in grid box. If any resolved or
C   convective cloud is present, run full multiphase mechanism.
C   Else only calculate gas/hetero chemistry.
               IF ( ( WCBAR_RS( COL,ROW,LEV ) .GT. LWC_LIMIT ) .OR.
     &              ( WCBAR_CV( COL,ROW,LEV ) .GT. LWC_LIMIT ) ) THEN

C.. Perform multiphase chemistry via KPP (includes scavenging)
                  CALL MCHEM_CHEM ( CGRID, TEMP, PRESS, DENSI, OPEN_WATER,
     &                              SEAWATER, CAIR, QV( COL,ROW,LEV),
     &                              CHEMSTEP, LEV, COL, ROW )

               ELSE

C.. Perform gas and heterogeneous chemistry only via KPP
                  CALL GAS_CHEM   ( CGRID, TEMP, PRESS, DENSI, OPEN_WATER,
     &                              SEAWATER, CAIR, QV( COL,ROW,LEV),
     &                              CHEMSTEP, LEV, COL, ROW )

C.. Perform scavenging if lwc limit is not exceeded to be consistent with
C   earlier implementations
                  DO SPC = 1, NSPCSD
                    IF ( LSCAV( SPC ) ) THEN
                       ALFA = RSCAV_RS( COL, ROW, LEV, SPC ) +
     &                        RSCAV_CV( COL, ROW, LEV, SPC ) * FRAC_CV( COL, ROW, LEV )
                       ALFA = MAX( ALFA, 0.0 )
                       IF ( ALFA .GT. 0.0 ) THEN
                         WET_DEP( COL, ROW, LEV, SPC ) = REAL( CGRID( COL, ROW, LEV, SPC ), 8 ) *
     &                                                   ( 1.0 - EXP( -ALFA * CHEMSTEP ) )
                         CGRID( COL, ROW, LEV, SPC )   = CGRID( COL, ROW, LEV, SPC ) -
     &                                                   REAL( WET_DEP( COL, ROW, LEV, SPC ), 4 )
                       END IF
                    END IF
                  END DO

               END IF

            END DO ! LEV
         END DO ! ROW
      END DO ! COL

C.. Convert to molar mixing ratio (by volume)
      DO LEV = 1, NLAYS
        DO ROW = 1, NROWS
          DO COL = 1, NCOLS
            TEMP  = REAL( TA( COL,ROW,LEV ), dp )
            PRESS = REAL( PA2ATM * PRES( COL,ROW,LEV ), dp )
            CAIR  = ( PRESS/PA2ATM ) / ( TEMP * 8.314510 ) * 6.0221413D17
            ! Reactive gases
            STRT = GC_STRT
            FINI = GC_STRT + N_GC_SPC - 1
            DO SPC = STRT, FINI
              CGRID( COL,ROW,LEV,SPC ) = CGRID( COL,ROW,LEV,SPC ) / CAIR
            END DO
            ! Aerosols
            STRT = 1
            FINI = NQAE
            DO IVAR = STRT, FINI
              SPC = QAE( IVAR )
              CGRID( COL,ROW,LEV,SPC ) = CGRID( COL,ROW,LEV,SPC ) / CAIR
            END DO
            ! Non-reactive gases
            STRT = NR_STRT
            FINI = NR_STRT + N_NR_SPC - 1
            DO SPC = STRT, FINI
              CGRID( COL,ROW,LEV,SPC ) = CGRID( COL,ROW,LEV,SPC ) / CAIR
            END DO
            ! Trace gases
            STRT = TR_STRT
            FINI = TR_STRT + N_TR_SPC - 1
            DO SPC = STRT, FINI
              CGRID( COL,ROW,LEV,SPC ) = CGRID( COL,ROW,LEV,SPC ) / CAIR
            END DO
            ! Wet deposition
            DO SPC = 1, NSPCSD
              WET_DEP( COL,ROW,LEV,SPC ) = WET_DEP( COL,ROW,LEV,SPC ) / CAIR
            END DO
            ! Wet deposition of H+
            WDEP_Hp( COL,ROW,LEV ) = WDEP_Hp( COL,ROW,LEV ) / CAIR
          END DO
        END DO
      END DO

C.. Convert wet deposition
      DO ROW = 1, NROWS
        DO COL = 1, NCOLS
          DO LEV = 1, NLAYS
             CALL CONVERT_WDEP( WET_DEP( COL, ROW, LEV, : ), WDEP_Hp( COL, ROW, LEV ),
     &                          AIRMASS( COL, ROW, LEV ) )
          END DO
        END DO
      END DO

C.. Add wet deposition for output
      DO ROW = 1, NROWS
        DO COL = 1, NCOLS
          DO LEV = 1, NLAYS
            DO SPC = 1, N_SPC_WDEP
              TWET_DEP( COL, ROW, SPC ) = TWET_DEP ( COL, ROW, SPC )
     &                  + REAL( WET_DEP ( COL, ROW, LEV, WDEP_MAP( SPC ) ), 4 )
            END DO
            TWET_DEP( COL, ROW, N_SPC_WDEP+1 ) = TWET_DEP( COL, ROW, N_SPC_WDEP+1 )
     &                  + REAL( WDEP_Hp ( COL, ROW, LEV ), 4 )
          END DO
        END DO
      END DO

C.. Convert CGRID back to original unit
      CALL CONVERT_BACKWARD( CGRID, DENS )

C.. Update Aerosol Surface Area
      CALL HETCHEM_UPDATE_AERO( CGRID )

C.. Reset transmissivity arrays
      RESTRANS = 1.0
      SUBTRANS = 1.0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Output statistics if required and return
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )
      EDATE = JDATE
      ETIME = JTIME
      CALL NEXTIME( EDATE, ETIME, TSTEP( 2 ) )
      IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN

         WSTEP = 0

#ifdef mchem_stats
         IF ( .NOT. WRITE3( CTM_MCHEMSTATS_1, VSTATS( 1 ),
     &         EDATE, ETIME, REAL( WCBAR_RS, 4) ) ) THEN
            XMSG = 'Could not write ' // CTM_MCHEMSTATS_1 // ' file'
            CALL M3EXIT( PNAME, EDATE, ETIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. WRITE3( CTM_MCHEMSTATS_1, VSTATS( 2 ),
     &         EDATE, ETIME, REAL( WCBAR_CV, 4 ) ) ) THEN
            XMSG = 'Could not write ' // CTM_MCHEMSTATS_1 // ' file'
            CALL M3EXIT( PNAME, EDATE, ETIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. WRITE3( CTM_MCHEMSTATS_1, VSTATS( 3 ),
     &         EDATE, ETIME, REAL( PRATE1_RS, 4 ) ) ) THEN
            XMSG = 'Could not write ' // CTM_MCHEMSTATS_1 // ' file'
            CALL M3EXIT( PNAME, EDATE, ETIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. WRITE3( CTM_MCHEMSTATS_1, VSTATS( 4 ),
     &         EDATE, ETIME, REAL( PRATE1_CV, 4 ) ) ) THEN
            XMSG = 'Could not write ' // CTM_MCHEMSTATS_1 // ' file'
            CALL M3EXIT( PNAME, EDATE, ETIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. WRITE3( CTM_MCHEMSTATS_1, VSTATS( 5 ),
     &         EDATE, ETIME, REAL( pH_RS, 4 ) ) ) THEN
            XMSG = 'Could not write ' // CTM_MCHEMSTATS_1 // ' file'
            CALL M3EXIT( PNAME, EDATE, ETIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. WRITE3( CTM_MCHEMSTATS_1, VSTATS( 6 ),
     &         EDATE, ETIME, REAL( pH_CV, 4 ) ) ) THEN
            XMSG = 'Could not write ' // CTM_MCHEMSTATS_1 // ' file'
            CALL M3EXIT( PNAME, EDATE, ETIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. WRITE3( CTM_MCHEMSTATS_1, VSTATS( 7 ),
     &         EDATE, ETIME, REAL( NSTEPS, 4 ) ) ) THEN
            XMSG = 'Could not write ' // CTM_MCHEMSTATS_1 // ' file'
            CALL M3EXIT( PNAME, EDATE, ETIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. WRITE3( CTM_MCHEMSTATS_1, VSTATS( 8 ),
     &         EDATE, ETIME, REAL( NFAILS, 4 ) ) ) THEN
            XMSG = 'Could not write ' // CTM_MCHEMSTATS_1 // ' file'
            CALL M3EXIT( PNAME, EDATE, ETIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. WRITE3( CTM_MCHEMSTATS_1, VSTATS( 9 ),
     &         EDATE, ETIME, REAL( FRAC_CV, 4 ) ) ) THEN
            XMSG = 'Could not write ' // CTM_MCHEMSTATS_1 // ' file'
            CALL M3EXIT( PNAME, EDATE, ETIME, XMSG, XSTAT1 )
         END IF

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &          'Timestep written to', CTM_MCHEMSTATS_1,
     &          'for date and time', EDATE, ETIME
#endif

C...write data to the normal wet deposition file
         IF ( .NOT. WRITE3( CTM_WET_DEP_1, ALLVAR3, EDATE,
     &                      ETIME, TWET_DEP ) ) THEN
           XMSG = 'Could not write ' // CTM_WET_DEP_1 // ' file'
           CALL M3EXIT ( PNAME, EDATE, ETIME, XMSG, XSTAT1 )
         END IF

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &          'Timestep written to', CTM_WET_DEP_1,
     &          'for date and time', EDATE, ETIME

C...reinitialize deposition array
         TWET_DEP  = 0.0

      END IF   ! time to write

      END
