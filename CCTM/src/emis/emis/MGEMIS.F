
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
      MODULE MGEMIS
C-----------------------------------------------------------------------
C Description:

C Revision History:
C   03 Nov 15: B. Gantt and G. Sarwar: created the initial version for marine 
C      gas emissions
C   26 Sep 16: D. Wong: removed repeated SEAICE interpx statement
C   20 Feb 18: G. Sarwar: removed Br2, added DMS, and revised estimates of 
C              halocarbon and HOI/I2 emissions
C   04 Dec 18: G. Srawar: updated local time calculation
C   01 Feb 19: D. Wong: Implemented centralized I/O approach
C   15 May 19: D. Wong: Replaced MGEM with USE_MARINE_GAS_EMISSION which is set in RUNTIME_VARS.F
C   16 May 19: D. Wong: * Removed initialization of OCEAN, SZONE, CHLR, and DMSL. 
C                       * Used LON directly to save space.
C   04 Sep 20: G. Sarwar* Fixed bug for Kw
C   03 Mar 22: D. Wong and G. Sarwar* updated for DMS emissions with cb6r5
C   09 Jul 24 G. Sarwar - updated for halogen chemistry in CRACMM3M

      USE RUNTIME_VARS
      USE HGRD_DEFN
      USE DESID_VARS
      use mio_module
#ifdef mpas
      use util_module
#endif

      IMPLICIT NONE

      INTEGER, PARAMETER :: NMGSPC = 12

C Marine gas Emissions Rates
      REAL, ALLOCATABLE, SAVE :: VDEMIS_MG( :,:,: )   ! marine gas emission rates [moles/s]

      PUBLIC NMGSPC, VDEMIS_MG, MGEMIS_INIT, GET_MGEMIS, MG_SPC, MGSPC_MAP
      PRIVATE

      INTEGER, SAVE              :: LGC_O3            ! pointer to O3 in CGRID

C Variables for the marine gas diagnostic file
      INTEGER :: NMGDIAG                              ! number of species in marine gas
                                                      ! diagnostic emission file
      REAL,    ALLOCATABLE       :: MGOUTD( : )       ! emission rates
      REAL,    ALLOCATABLE, SAVE :: MGBF( :,:,: )     ! marine gas emiss accumulator
      REAL,    ALLOCATABLE, SAVE :: WRMG( :,: )       ! marine gas emiss write buffer
      INTEGER, SAVE              :: MGSPC_MAP( NMGSPC )

! Species names in the speciated marine gas-emissions
      CHARACTER( 16 ), ALLOCATABLE :: WRMG_SPC( : )   ! species names

      CHARACTER( 16 ) :: MG_SPC( NMGSPC)           ! emitted species

      INTEGER :: MG_SPC_IND ( NMGSPC)              ! emitted species index
         
      DATA  MG_SPC            /   'TRIBROMOMETHANE ',
     &                            'DIBROMOMETHANE  ',
     &                            'BROMOCHLOROMETHA',
     &                            'DIBROMOCHLORMETA',
     &                            'BROMODICHLOROMET',
     &                            'METHYLIODIDE    ',
     &                            'CHLOROIODOMETHAN',
     &                            'BROMOIODOMETHANE',
     &                            'DIIODOMETHANE   ',
     &                            'MOLECULARIODINE ',
     &                            'HYPOIODUSACID   ',
     &                            'DIMETHYLSULFIDE ' /      

       REAL :: MG_MW( NMGSPC )                    ! emitted species molecular weight (g/mol)

       DATA MG_MW   / 252.7,
     &                173.8,
     &                129.4,
     &                208.3,
     &                243.8,
     &                141.9,
     &                176.4,
     &                219.9,
     &                267.8,
     &                253.8,
     &                143.9,
     &                62.0     /

C Variables interpolated from the meteorological input files
      REAL,    ALLOCATABLE, SAVE :: U10( :,: )      ! wind speed at 10m [m/s]
      REAL,    ALLOCATABLE, SAVE :: TSEASFC( :,: )  ! sea surface temp K
      REAL,    ALLOCATABLE, SAVE :: SEAICE( :,: )   ! seaice fraction

      INTEGER I

C Domain decomposition info from emission and meteorology files
      INTEGER, SAVE :: GXOFF, GYOFF                 ! origin offset
      INTEGER, SAVE :: STRTCOL_O1, ENDCOL_O1, STRTROW_O1, ENDROW_O1
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
      INTEGER, SAVE :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2

      CONTAINS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      FUNCTION MGEMIS_INIT( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )

C Revision History:
C   03 Nov 15: B. Gantt and G. Sarwar: created the initial version for marine gas emissions

            USE UTILIO_DEFN
            USE HGRD_DEFN
            USE CGRID_SPCS
            USE RXNS_DATA
            USE RUNTIME_VARS, ONLY : USE_MARINE_GAS_EMISSION
            USE CENTRALIZED_IO_MODULE, only : chlr, dmsl

            IMPLICIT NONE
            INCLUDE SUBST_FILES_ID                                  ! file name parameters

C Arguments:
            INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP
            LOGICAL :: SUCCESS

            CHARACTER( 80 )       :: VARDESC                        ! env variable description

C Local Variables:

            CHARACTER( 16 ), SAVE :: PNAME = 'MGEMIS_INIT'
            CHARACTER( 96 ) :: XMSG = ' '
            CHARACTER(  16 ) :: VARNM
            INTEGER STATUS
            INTEGER S, N, L, K, V
            INTEGER :: START_INDEX, END_INDEX

C-----------------------------------------------------------------------

            SUCCESS = .TRUE.

C flag for marine gas emissions; this flag is set in the subroutine EM_FILE_INIT 
            IF ( .NOT. USE_MARINE_GAS_EMISSION ) RETURN

C Allocate MG arrays using NMGSPC value
            DESID_EMVAR( IMGSRM )%LEN = NMGSPC
            ALLOCATE( DESID_EMVAR( IMGSRM )%ARRY ( NMGSPC ) )
            ALLOCATE( DESID_EMVAR( IMGSRM )%UNITS( NMGSPC ) )
            ALLOCATE( DESID_EMVAR( IMGSRM )%MW   ( NMGSPC ) )
            ALLOCATE( DESID_EMVAR( IMGSRM )%USED ( NMGSPC ) )
            ALLOCATE( DESID_EMVAR( IMGSRM )%CONV ( NMGSPC ) )
            ALLOCATE( DESID_EMVAR( IMGSRM )%BASIS( NMGSPC ) )
            ALLOCATE( DESID_EMVAR( IMGSRM )%LAREA( NMGSPC ) )
            ALLOCATE( DESID_EMVAR( IMGSRM )%LAREAADJ( NMGSPC ) )
            
            DESID_EMVAR( IMGSRM )%ARRY  = MG_SPC
            DESID_EMVAR( IMGSRM )%UNITS = 'MOLES/S'
            DESID_EMVAR( IMGSRM )%MW    = MG_MW
            DESID_EMVAR( IMGSRM )%USED  = .FALSE.
            DESID_EMVAR( IMGSRM )%CONV  = 1.0
            DESID_EMVAR( IMGSRM )%BASIS = 'MOLE'
            DESID_EMVAR( IMGSRM )%LAREA = .FALSE.
            DESID_EMVAR( IMGSRM )%LAREAADJ = .FALSE.

            ALLOCATE ( VDEMIS_MG( NMGSPC,NCOLS,NROWS), STAT = STATUS )	    
	    VDEMIS_MG = 0.0
	    
            IF ( STATUS .NE. 0 ) THEN
               XMSG = '*** VDEMIS_MG, memory allocation failed'
               CALL LOG_WARNING( PNAME, LOGDEV , XMSG)
               SUCCESS = .FALSE.; RETURN
            END IF

            ALLOCATE ( TSEASFC( NCOLS,NROWS ),
     &                 U10( NCOLS,NROWS ),
     &                 SEAICE( NCOLS,NROWS ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
              XMSG = '*** DMSL, TSEASFC, U10, or SEAICE memory'
              CALL LOG_WARNING( PNAME, LOGDEV , XMSG)
              SUCCESS = .FALSE.; RETURN
            END IF   
            U10     = 0.0
            TSEASFC = 293.15
            SEAICE  = 0.0

C Get index for surface ozone concentration 
            VARNM = 'O3'
            LGC_O3 = INDEX1( VARNM, N_GC_SPC, GC_SPC )
            IF ( LGC_O3 .LE. 0 ) THEN
               XMSG = 'Could not find ' // VARNM // 'in species table'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF

            IF (USE_MARINE_GAS_EMISSION .AND. MGEMDIAG ) THEN           ! Open the marine gas emission diagnostic file
C Determine number of marine gas species for diagnostic file
                NMGDIAG = NMGSPC

C Allocate diagnostic arrays
                ALLOCATE ( MGOUTD( NMGDIAG ),
     &                     WRMG_SPC( NMGDIAG ), STAT = STATUS )
                IF ( STATUS .NE. 0 ) THEN
                    XMSG = '*** MGOUTD or WRMG_SPC memory alloc failed'
                    CALL LOG_WARNING( PNAME, LOGDEV , XMSG)
                    SUCCESS = .FALSE.; RETURN
                END IF

C Build diagnostic file write buffer (WRMG_SPC) array
                   START_INDEX = 1
                   END_INDEX = 12

                NMGDIAG = 0
                DO S = START_INDEX, END_INDEX
                   NMGDIAG = NMGDIAG + 1
                   WRMG_SPC( NMGDIAG ) = MG_SPC( S )
                   MG_SPC_IND( NMGDIAG ) = S
                END DO

C Open the marine gas emission dignostic file
                IF ( IO_PE_INCLUSIVE ) CALL OPMGEMIS ( STDATE, STTIME, TSTEP, NMGDIAG, WRMG_SPC )

                ALLOCATE ( MGBF( NMGDIAG,NCOLS,NROWS ),
     &                     WRMG( NCOLS,NROWS ), STAT = STATUS )
                IF ( STATUS .NE. 0 ) THEN
                   XMSG = '*** MGBF or WRMG memory allocation failed'
                   CALL LOG_WARNING( PNAME, LOGDEV , XMSG)
                   SUCCESS = .FALSE.; RETURN
                END IF
                MGBF = 0.0

            END IF

         END FUNCTION MGEMIS_INIT

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE OPMGEMIS ( JDATE, JTIME, TSTEP, NMGDIAG, WRMG_SPC )

C 8 Aug 14 B.Gantt adapted from sea salt diagnostic

         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE UTILIO_DEFN
         USE MIO_ASCII

         IMPLICIT NONE

         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER, INTENT( IN )         :: JDATE      ! current model date, coded YYYYDDD
         INTEGER, INTENT( IN )         :: JTIME      ! current model time, coded HHMMSS
         INTEGER, INTENT( IN )         :: TSTEP      ! output time step
         INTEGER, INTENT( IN )         :: NMGDIAG
         CHARACTER( 16 ), INTENT( IN ) :: WRMG_SPC( NMGDIAG )

C Local variables:
         CHARACTER( 16 ) :: PNAME = 'OPMGEMIS'
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER      V, L       ! loop induction variables
         
C-----------------------------------------------------------------------

#ifdef mpas
         call mio_fcreate (CTM_MGEM_1, mio_new_file)

#else

            NVARS3D = NMGDIAG
            NLAYS3D = 1

            DO V = 1, NMGDIAG
               VTYPE3D( V ) = M3REAL
               VNAME3D( V ) = WRMG_SPC( V )
               UNITS3D( V ) = 'mol s-1'
               VDESC3D( V ) = 'hourly ' // TRIM( VNAME3D( V ) )               
     &                     // ' marine gas emission rate'
            END DO

            ! Store MIO Metadata
            NDIMS3D( 1:NVARS3D ) = 4
            L_TSTEP( 1:NVARS3D ) = .True.
            L_LAY  ( 1:NVARS3D ) = .True. ! even if 2D
            L_COL  ( 1:NVARS3D ) = .True.
            L_ROW  ( 1:NVARS3D ) = .True.
            L_VEXT ( 1:NVARS3D ) = .False.

            CALL LOAD_MIO_FILE ( 'CTM_MGEM_1', 0, 0, 0,
     &           VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &           VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &           L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),
     &           L_VEXT(1:NVARS3D) )
            

#endif

         RETURN

         END SUBROUTINE OPMGEMIS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE GET_MGEMIS ( JDATE, JTIME, TSTEP, CGRID, L_DESID_DIAG )

C GET_MGEMIS calculates the marine gas emission rates in a grid cell
C given the fractional grid-cell area covered by open ocean and surf zone

C Key Subroutines/Functions Called:  NONE

C Revision History:
C   03 Nov 15: B. Gantt and G. Sarwar: created the initial version from sea salt diagnostic file
C   20 Feb 18: G. Sarwar: removed Br2, added DMS, and revised estimates of halocarbon and HOI/I2 emissions
C   04 Dec 18: G. Srawar: updated local time calculation
C
CReferences:
C MacDonald, S. M., Gómez Martín, J. C., Chance, R., Warriner, S., Saiz-Lopez, A., Carpenter, L. J.,
C and Plane, J. M. C.: A laboratory characterisation of inorganic iodine emissions from the sea
C surface: dependence on oceanic variables and parameterisation for global modelling, Atmos. Chem.
C Phys., 14, 5841-5852, doi:10.5194/acp-14-5841-2014, 2014.
C
C     Sherwen, T., Evans, M. J., Carpenter, L. J., Andrews, S. J., Lidster, R. T., Dix, B., Koenig, T. K., 
C Sinreich, R., Ortega, I., Volkamer, R., Saiz-Lopez, A., Prados-Roman, C., Mahajan, A. S., and Ordóñez, C.: 
C Iodine's impact on tropospheric oxidants: a global model study in GEOS-Chem. Atmos. Chem. Phys., 2016, 16, 1161-1186.

C-----------------------------------------------------------------------
            USE UTILIO_DEFN
            USE CONST, ONLY: PI180, DG2M, DKBOLTZ, GRAV
            USE ASX_DATA_MOD, ONLY: MET_DATA                 ! added by Sarwar
            USE CGRID_SPCS     ! CGRID mechanism species
            USE PCGRID_DEFN    ! get cgrid
            USE GRID_CONF
            USE CENTRALIZED_IO_MODULE

            IMPLICIT NONE

C Arguments
            INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP( 3 )
            REAL, POINTER         :: CGRID( :,:,:,: )         !  concentrations
            LOGICAL, INTENT( IN ) :: L_DESID_DIAG ! flag determining whether or not DESID
                                                  !   is in diagnostic mode              
C Includes:
            INCLUDE SUBST_FILES_ID  ! file name parameters

C Parameters:
C.. Define Marine Gas indices
	    
            INTEGER, PARAMETER :: ITRIBROMOMETHANE = 1
            INTEGER, PARAMETER :: IDIBROMOMETHANE  = 2
            INTEGER, PARAMETER :: IBROMOCHLOROMETH = 3
            INTEGER, PARAMETER :: IDIBROMOCHLORMET = 4
            INTEGER, PARAMETER :: IBROMODICHLOROME = 5
            INTEGER, PARAMETER :: IMETHYLIODIDE    = 6
            INTEGER, PARAMETER :: ICHLOROIODOMETHA = 7
            INTEGER, PARAMETER :: IBROMOIODOMETHAN = 8
            INTEGER, PARAMETER :: IDIIODOMETHANE   = 9
            INTEGER, PARAMETER :: IMOLECULARIODINE = 10
            INTEGER, PARAMETER :: IHYPOIODUSACID   = 11
            INTEGER, PARAMETER :: IDIMETHYLSULFID  = 12
	       	      	      
            REAL, PARAMETER :: SECS2HR = 1.0 / 3600.0          ! hour per seconds
            REAL, PARAMETER :: C_FACT  = 1.0E-9 /(24.0*3600.0) ! factor to convert nmole/m2/day to mole/m2/sec
            REAL, PARAMETER :: LON2TZ  = 1.0 / 15.0            ! longitude to time zone, 1/degrees
            REAL, PARAMETER :: FHRLY(0:23) =
     &                         ( / 0.032, 0.032, 0.032, 0.033, 0.034, 0.036,
     &                             0.039, 0.044, 0.051, 0.057, 0.062, 0.064,
     &                             0.062, 0.057, 0.051, 0.044, 0.039, 0.036,
     &                             0.034, 0.033, 0.032, 0.032, 0.032, 0.032 / )

C Local Variables:
            REAL    :: CHLA
            REAL    :: SSTC
            REAL    :: SST            
            REAL    :: DMS_L              ! DMS in ocean water - nM  
            REAL    :: DMS                ! DMS in atmosphere                        
            REAL    :: SICE               ! SEAICE in a grid cell

            REAL    :: SCN
            REAL    :: K600
            REAL    :: KW
            REAL    :: KA
            REAL    :: KT
            REAL    :: GAMMA
            REAL    :: ALPHA
            REAL    :: H_DMS

            CHARACTER( 16 ), SAVE :: PNAME = 'GET_MGEMIS'
            CHARACTER( 96 )       :: XMSG = ' '
            CHARACTER( 80 ) :: MSG                      ! Message

            INTEGER         :: C, R, L, N, V, S         ! loop indices

            INTEGER         :: LOGUNIT
            INTEGER         :: NVARS, NSTEPS
            INTEGER         :: LOCHR
            INTEGER         :: OFF_SET
            INTEGER, SAVE   :: WSTEP  = 0                      ! local write counter
            INTEGER         :: MDATE, MTIME                    ! MGEM write date&time
 
            REAL            :: IWSTEP                          ! reciprocal of local write counter
            REAL            :: DX1, DX2                        ! CX x1- and x2-cell widths
            REAL            :: O3, I_AQ                        ! SST in K, SSTC in C, O3 in ppb, SWPD10 in m/s
            REAL            :: DUMMY                           ! dummy variable for calculating marine gas emission
            REAL            :: LOC_LON                         ! grid-cell longitude
            REAL            :: WSPD10                          ! wind speed
            REAL            :: WSPD10_M5                       ! wind speed with a minimum speed of 5 m/s (used for HOI and I2 emission calculation)
            REAL            :: MARINE_AREA                     ! marine fraction of surface and later marine surface area
            REAL            :: CURRHR                          ! current GMT hour
            REAL, SAVE      :: A                               ! horizontal area of cells, m**2
            REAL, PARAMETER :: TWOTHIRDS = 2.0 / 3.0           ! 2.0 / 3.0
            REAL, PARAMETER :: ONESIXHUNDREDS = 1.0 / 600.0    ! 1.0 / 600.0

            LOGICAL, SAVE   :: FIRST_TIME = .TRUE.
            LOGICAL, SAVE   :: FIRSTWRITE = .TRUE.

            CHARACTER( 20 ) :: time_stamp

C----------------------------- Begin calc ------------------------------

           IF( FIRST_TIME ) THEN
C calculate grid-cell area
#ifndef mpas
             IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
                DX1 = DG2M * XCELL_GD ! in m.
                DX2 = DG2M * YCELL_GD
     &              * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( GL_NROWS/2 )))! in m.
             ELSE
                DX1 = XCELL_GD        ! in m.
                DX2 = YCELL_GD        ! in m.
             END IF
             A = DX1 * DX2                                      ! m2
#endif

             FIRST_TIME = .FALSE.
           END IF

           CURRHR = REAL ( TIME2SEC( JTIME ) ) / 3600.0

C Assume MET_CRO_2D file is already opened

           IF (WSPD10_AVAIL) THEN
              call interpolate_var ('WSPD10', jdate, jtime, U10)
           ELSE
              call interpolate_var ('WIND10', jdate, jtime, U10)
           END IF

           IF (TSEASFC_AVAIL) THEN
              call interpolate_var ('TSEASFC', jdate, jtime, TSEASFC)
           ELSE
              call interpolate_var ('TEMPG', jdate, jtime, TSEASFC)
           END IF

           call interpolate_var ('SEAICE', jdate, jtime, SEAICE)

           ! Only write out marine gas diagnostics if not in diagnostic
           ! mode
           IF ( MGEMDIAG .AND. WSTEP .EQ. 0 .AND. .NOT. L_DESID_DIAG ) MGBF = 0.0

C Initialize marine gas output buffer
          DO R = 1, NROWS
             DO C = 1, NCOLS

                MARINE_AREA = ( OCEAN( C,R ) + SZONE( C,R ) )

                IF ( MARINE_AREA .GT. 0.0 .AND. SEAICE( C, R ) .LE. 0.0) THEN
!            convert area fraction to an actual area
#ifdef mpas
                     MARINE_AREA = MARINE_AREA * cell_area(c,1)
#else
                     MARINE_AREA = MARINE_AREA * A / msfx2 (C, R)		     
#endif

                     CHLA      = MIN ( 1.0, CHLR ( C,R ) )
                     WSPD10    = U10( C,R )
                     WSPD10_M5 = MAX (5.0, WSPD10)
                     SST       = TSEASFC (C,R)
                     SSTC      = MIN( (SST - 273.15), 30.0)                          
                     LOC_LON   = LON (C,R)
                     O3       = 1000.0 * CGRID(C, R, 1, LGC_O3)
                     DMS_L     = DMSL( C, R )   

C.. calculate iodide in water
                     I_AQ = 1.46E6 * EXP( -9134.0 / SST)

!..    calculate parameters needed for DMS emissions 

!..    Calculate Schmidt number of DMS (Saltzman et al, JGR, 1993)
       SCN = 2674.0 - 147.12 * SSTC + 3.726 * SSTC * SSTC - 0.038 * SSTC * SSTC * SSTC

!..    Calculate water-side DMS gas-transfer velocity following Liss and Merlivat,  1986 (unit - cm/hr)
       IF ( WSPD10 .LE. 3.6 ) THEN
          KW = 0.17 * WSPD10 / ( (SCN * ONESIXHUNDREDS)**TWOTHIRDS)
        ELSE IF ( WSPD10 .GT. 3.6 .AND. WSPD10 .LE. 13.0 ) THEN
          KW = ( 2.85 * WSPD10 - 9.65 ) / SQRT(SCN * ONESIXHUNDREDS)
        ELSE IF ( WSPD10 .GT. 13.0) THEN
          KW = ( 5.9 * WSPD10 - 49.3 ) / SQRT(SCN * ONESIXHUNDREDS)
       ENDIF
    
!..    Calculate air-side DMS gas-transfer velocity (McGills et al, JGR, 2000) (unit - cm/hr)
       KA = 659.0 * WSPD10 / SQRT(62.0/18.0)
            
!..    Calculate Henry's Law Coefficient of DMS expressed as C_air/C_water following Dacey et al, GRL, 1984  (unit - atm . L /mole)
       H_DMS = EXP ( 12.64 - 3547.0/SST )                             
       
!..    Calculate Solubility Coeffcient of DMS expressed as C_water/C_air following McGills et al, JGR, 2000 - (unit dimensionless ) 
!..    Dacey et al. calculates Henry's Law Coefficient as C_air/C_water, thus, an inverse is taken for unit consistency 
!..    and then it is converted into dimensionless unit
       ALPHA = 0.082058 * SST / H_DMS                                

!..    Calculate Atmospheric Gradient Fraction following Lan et al,  Global Biogeochemical Cycles, 2011
       GAMMA = 1.0 /( 1.0 + KA / (ALPHA * KW))  

!..    Calculate total gas transfer velocity for DMS following Lan et al,  Global Biogeochemical Cycles, 2011 
       KT = KW * (1.0 - GAMMA)

C.. calculate local hour
                     OFF_SET = NINT(LOC_LON * LON2TZ )
                     LOCHR =  INT(CURRHR + OFF_SET )

                     IF (LOCHR .LT.  0) LOCHR = LOCHR + 24
                     IF (LOCHR .GT. 23) LOCHR = LOCHR - 24
            
                     DUMMY = 4.31E-8 * MARINE_AREA * FHRLY(LOCHR) * CHLA * SECS2HR

C.. Calculate TRIBROMOMETHANE emission rate in mole/s
                     VDEMIS_MG( ITRIBROMOMETHANE, C, R ) = DUMMY * 2.00

C.. Calculate DIBROMOMETHANE emission rate in mole/s
                     VDEMIS_MG( IDIBROMOMETHANE, C, R ) = DUMMY * 0.50

C.. Calculate BROMOCHLOROMETHANE emission rate in mole/s
                     VDEMIS_MG( IBROMOCHLOROMETH, C, R ) = DUMMY * 0.08

C.. Calculate DIBROMOCHLORMETHANE emission rate in mole/s
                     VDEMIS_MG( IDIBROMOCHLORMET, C, R ) = DUMMY * 0.12

C.. Calculate BROMODICHLOROMETHANE emission rate in mole/s
                     VDEMIS_MG( IBROMODICHLOROME, C, R ) = DUMMY * 0.10

C.. Calculate METHYLIODIDE emission rate in mole/s
                     VDEMIS_MG( IMETHYLIODIDE, C, R ) = DUMMY * 1.60

C.. Calculate CHLOROIODOMETHANE emission rate in mole/s
                     VDEMIS_MG( ICHLOROIODOMETHA, C, R ) = DUMMY * 1.41

C.. Calculate BROMOIODOMETHANE emission rate in mole/s
                     VDEMIS_MG( IBROMOIODOMETHAN, C, R ) = DUMMY * 0.42

C.. Calculate DIIODOMETHANE emission rate in mole/s
                     VDEMIS_MG(IDIIODOMETHANE, C, R ) = DUMMY * 0.46

C.. Calculate MOLECULARIODINE emission rate in mole/s (Macdonald et al., ACP, 14, 5841-5852, 2014)
                     DUMMY = O3 * (I_AQ**1.3) * (1.74E9 - 6.54E8*LOG(WSPD10_M5)) ! NMOL/M2/D
                     DUMMY = MARINE_AREA * DUMMY * C_FACT
                     VDEMIS_MG(  IMOLECULARIODINE, C, R ) = MAX( 0.0, DUMMY )

C.. Calculate HYPOIODUSACID emission rate in mole/s (Macdonald et al., ACP, 14, 5841-5852, 2014)
                     DUMMY = SQRT(I_AQ)
                     DUMMY = O3 * ((4.15E5*DUMMY - 20.6)/WSPD10_M5 - 23600.0*DUMMY)  ! NMOL/M2/D
                     DUMMY = MARINE_AREA * DUMMY * C_FACT
                     VDEMIS_MG( IHYPOIODUSACID, C, R ) = MAX( 0.0, DUMMY )

C .. Calculate DIMETHYLSULFIDE emission rate in mole/s 
                     DUMMY = MARINE_AREA *  DMS_L * KT * 2.78E-12
                     VDEMIS_MG( IDIMETHYLSULFID, C, R ) = MAX ( 0.0, DUMMY)
		     
                     END IF  ! Marine area

C Update the MGBF array, for writing the diagnostic marine gas emission file
                IF ( MGEMDIAG .AND. .NOT. L_DESID_DIAG ) THEN
                   V = 0
                   DO S = 1, NMGSPC
                      V = V + 1
                      MGOUTD( V ) = VDEMIS_MG( S,C,R )
                   END DO

                   DO S = 1, NMGDIAG
                      MGBF( S,C,R ) = MGBF( S,C,R ) + MGOUTD( MG_SPC_IND (S) )
     &                              * REAL( TIME2SEC ( TSTEP( 2 ) ) )
                  END DO
                END IF  ! MGEMDIAG

             END DO   ! C
         END DO   ! R


C If last call this hour, write out the total MG emissions [moles/s].
C Then reset the MG emissions array and local write counter.
         IF ( MGEMDIAG .AND. .NOT. L_DESID_DIAG ) THEN

             WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )

             IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN
                IWSTEP = 1.0 / REAL( WSTEP )

                IF ( .NOT. CURRSTEP( JDATE, JTIME, STDATE, STTIME, TSTEP( 1 ),
     &                                MDATE, MTIME ) ) THEN
                     XMSG = 'Cannot get step date and time'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
                END IF
                CALL NEXTIME( MDATE, MTIME, TSTEP( 1 ) )
                call mio_time_format_conversion (mdate, mtime, time_stamp)

                DO V = 1, NMGDIAG
                   DO R = 1, NROWS
                      DO C = 1, NCOLS
                         WRMG( C,R ) = MGBF( V,C,R ) * IWSTEP
                      END DO
                   END DO
#ifdef mpas
                   call mio_fwrite (CTM_MGEM_1, WRMG_SPC(V), pname, WRMG(:,1), time_stamp)
#else

                   if ( firstwrite ) then
                      firstwrite = .false.
                      call mio_setfile( INIT_CONC_1 )
                      mio_nlays = 1
                      call mio_fcreate( CTM_MGEM_1, mio_new_file )
                      if (mype .eq. 0) then
                         call mio_set_global_attr ( CTM_MGEM_1, 'NLAYS', 1 )
                         call mio_set_global_attr ( CTM_MGEM_1, 'VGLVLS', INIT_CONC_1, 1, 2 )
                      end if
                   end if

                   call mio_fwrite( CTM_MGEM_1, WRMG_SPC( V ),
     &                              pname, WRMG, time_stamp )
#endif
               END DO
#ifdef mpas
               WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), a19)')
     &                      'Timestep written to', CTM_MGEM_1,
     &                      'for time stamp', time_stamp
#else
               WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )')
     &                      'Timestep written to', CTM_MGEM_1,
     &                      'for date and time', MDATE, MTIME
#endif
               WSTEP = 0
               MGBF = 0.0
             END IF

          END IF  ! MGDIAG

         RETURN

       END SUBROUTINE GET_MGEMIS

      END MODULE MGEMIS
