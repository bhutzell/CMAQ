
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
      SUBROUTINE AQ_MAP ( JDATE, JTIME, WTBAR, WCBAR, TBARC, PBARC,
     &                    CTHK1, AIRM, PRATE1, TAUCLD, POLC, CEND,
     &                    REMOV, REMOVAC, ALFA0, ALFA2, ALFA3, DARK )

C-----------------------------------------------------------------------
C  DESCRIPTION:  This is an interface processor between the cloud dynamics
C     module(s) and the aqueous chemistry module.  It uses indices to
C     map species from their order in the array POLC, CEND, REMOV to
C     the species that are expected for AQCHEM, ie GAS and AEROSOL.
 
C  Revision History:
C      No   Date   Who  What
C      -- -------- ---  -----------------------------------------
C       0 01/15/98 sjr  created program
C       1 02/13/98 sjr  modified/generalized the mapping procedure
C         Dec 00   Jeff move CGRID_MAP into f90 module
C       3 06/07/05 sjr  added logic for coarse sulfate
C       4 04/11/08 jtk  added logic for coarse ammonium
C       5 10/10/10 yoj  update to use aero_reeng by Steve Howard, Prakash Bhave,
C                       Jeff Young, Sergey Napelenok, and Shawn Roselle
C       6 03/01/11 sjr  replaced I/O API include files with UTILIO_DEFN;
C       7 07/01/01 gs   calculate zenith angle to determine daytime and nightime 
C                       needed for sulfur oxidation via metal catalysis
C       8 09/10/11 wth  adapted for multiple pollutant model, i.e., Gas phase mercury 
C                       PM mercury and tracer species
C       07 Jul 14  wth: replaced mechanism include file(s) with fortran module
C       01 Feb 19 D.Wong: replaced run time dynamic arrays with allocatable arrays
C       30 Dec 19 S.Napelenok: ddm-3d implementation for v 5.3.1
 
C  Called by:  RADMCLD and RESCLD
 
C  Calls the following subroutines:  AQCHEM
 
C  ARGUMENTS    TYPE      I/O       DESCRIPTION
C  ---------   -------  ------  --------------------------------
C    JDATE     integer   input  current model julian date (yyyyddd)
C    JTIME     integer   input  current model time (hhmmss)
C    WTBAR      real     input  avg total water content (kg/m3)
C    WCBAR      real     input  avg liquid water content (kg/m3)
C    TBARC      real     input  avg cloud temperature (K)
C    PBARC      real     input  avg cloud pressure (Pa)
C    CTHK1      real     input  cloud thickness (m)
C    AIRM       real     input  total air mass (moles/m2) in cloudy air
C    PRATE1     real     input  precipitation rate (mm/hr)
C    TAUCLD     real     input  cloud lifetime (s)
C    POLC       real     input  ave vert conc incloud (moles sp/ mole air)
C    CEND       real    output  ending incloud conc (moles/mole)
C    REMOV      real    output  moles/m2 or mm*mol/lit scavenged
C    REMOVAC    real    output  variable storing H+ deposition
C    ALFA0      real     input  scav coef for aitken aerosol number
C    ALFA2      real     input  scav coef for aitken aerosol surface area
C    ALFA3      real     input  scav coef for aitken aerosol mass
C    DARK       logical  input DARK = TRUE is night,  DARK = FALSE is day
C-----------------------------------------------------------------------

      USE CGRID_SPCS     ! CGRID mechanism species
      USE AERO_DATA      ! aerosol shared parameters
      USE AQ_DATA        ! aqueous chemistry shared parameters
      USE UTILIO_DEFN
#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX, S_POLC, S_CEND, S_REMOV,
     &                       S_REMOVAC, S_HPWDEP, S_GAS, S_GASWDEP,
     &                       S_AEROSOL, S_AERWDEP, S_WSRGGAS, S_WSRGAER
#endif 

      IMPLICIT NONE
      INCLUDE SUBST_CONST   ! for DPI
      
      CHARACTER( 120 ) :: XMSG = ' '    ! Exit status message

C Parameters:

      INTEGER, SAVE :: MXSPCS   ! Number of species in CGRID

      REAL, PARAMETER :: ONETHIRD  = 1.0 / 3.0
      REAL, PARAMETER :: TWOTHIRDS = 2.0 / 3.0

C Arguments:

      INTEGER, INTENT( IN )    :: JDATE     ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN )    :: JTIME     ! current model time, coded HHMMSS

      REAL,    INTENT( IN )    :: WTBAR     ! total wat cont (kg/m2) int. thru cld
      REAL,    INTENT( IN )    :: WCBAR     ! liq water content of cloud (kg/m3)
      REAL,    INTENT( IN )    :: TBARC     ! mean cloud temp (K)
      REAL,    INTENT( IN )    :: PBARC     ! mean cloud pressure (Pa)
      REAL,    INTENT( IN )    :: CTHK1     ! aq chem calc cloud thickness
      REAL,    INTENT( IN )    :: AIRM      ! total air mass (moles/m2) in cloudy air
      REAL,    INTENT( IN )    :: PRATE1    ! storm rainfall rate (mm/hr)
      REAL,    INTENT( IN )    :: TAUCLD    ! cloud lifetime
      REAL,    INTENT( IN )    :: POLC ( : )  ! avg vert conc incloud (moles/mole)
      REAL,    INTENT( INOUT ) :: CEND ( : )  ! ending incloud conc (moles/mole)
      REAL,    INTENT( INOUT ) :: REMOV( : )  ! moles/m2 or mm*mol/lit scavenged
      REAL,    INTENT( INOUT ) :: REMOVAC   ! variable storing H+ deposition
      REAL,    INTENT( IN )    :: ALFA0     ! scav coef for aitken aerosol number
      REAL,    INTENT( IN )    :: ALFA2     ! scav coef for aitken aerosol sfc area
      REAL,    INTENT( IN )    :: ALFA3     ! scav coef for aitken aerosol mass
      LOGICAL, INTENT( IN )    :: DARK      ! DARK = TRUE is night,  DARK = FALSE is day

C Local Variables (scalars):

      LOGICAL, SAVE :: FIRSTIME = .TRUE.      ! flag for first pass thru

      CHARACTER(16), SAVE :: PNAME = 'AQ_MAP' ! program name

      INTEGER       IAER                ! aerosol loop counter
      INTEGER       IM                  ! aerosol mode loop counter
      INTEGER       IGAS                ! gas loop counter
      INTEGER       ISRG                ! surrogate loop counter
      INTEGER       PNTR                ! relative pointer variable
      INTEGER       SPC, ISPC, IAQ      ! liquid species loop counter
      INTEGER ::    STAT

      REAL( 8 )  :: BETASO4
      REAL       :: EALFA2T                ! EXP( -ALFA2 * TAUCLD )
      REAL( 8 )  :: M3NEW( N_MODE )        ! modal mass at time t
      REAL( 8 )  :: M3OLD( N_MODE )        ! modal mass at time 0
      REAL( 8 )  :: Dens_wmean_old( N_MODE ) ! concentration weighted molecular weight g/mol
      REAL( 8 )  :: Dens_wmean_new( N_MODE ) ! concentration weighted molecular weight g/mol

      REAL       :: HPWDEP                        ! hydrogen wet dep (mm mol/liter)
      REAL( 8 ), ALLOCATABLE, SAVE :: GAS    ( : )    ! gas phase conc (mol/mol)
      REAL( 8 ), ALLOCATABLE, SAVE :: GASWDEP( : )    ! gas phase wet dep array (mm mol/liter)
      REAL( 8 ), ALLOCATABLE, SAVE :: AEROSOL( :, :)  ! aerosol conc (mol/mol)
      REAL( 8 ), ALLOCATABLE, SAVE :: AERWDEP( :, :)  ! aerosol wet dep array (mm mol/liter)
      REAL( 8 ), ALLOCATABLE, SAVE :: WSRGAQ( : )     ! weights for surrogate

C External Functions:

      INTEGER, EXTERNAL :: INDEXN           ! external func to get species pointers


      INTERFACE
        SUBROUTINE AQCHEM ( JDATE, JTIME, TEMP, PRES_PA, TAUCLD, PRCRATE,
     &                      WCAVG, WTAVG, AIRM, ALFA0, ALFA2, ALFA3, GAS,
     &                      AEROSOL, GASWDEP, AERWDEP, HPWDEP, BETASO4, DARK )
           INTEGER,   INTENT( IN )  :: JDATE  ! current model date, coded YYYYDDD
           INTEGER,   INTENT( IN )  :: JTIME  ! current model time, coded HHMMSS
           REAL,      INTENT( IN )  :: AIRM      ! total air mass in cloudy layers (mol/m2)
           REAL,      INTENT( IN )  :: ALFA0     ! scav coef for aitken aerosol number
           REAL,      INTENT( IN )  :: ALFA2     ! scav coef for aitken aerosol sfc area
           REAL,      INTENT( IN )  :: ALFA3     ! scav coef for aitken aerosol mass
           REAL,      INTENT( OUT ) :: HPWDEP    ! hydrogen wet deposition (mm mol/liter)
           REAL( 8 ), INTENT( OUT ) :: BETASO4  
           REAL,      INTENT( IN )  :: PRCRATE   ! precip rate (mm/hr)
           REAL,      INTENT( IN )  :: PRES_PA   ! pressure (Pa)
           REAL,      INTENT( IN )  :: TAUCLD    ! timestep for cloud (s)
           REAL,      INTENT( IN )  :: TEMP      ! temperature (K)
           REAL,      INTENT( IN )  :: WCAVG     ! liquid water content (kg/m3)
           REAL,      INTENT( IN )  :: WTAVG     ! total water content (kg/m3)
           REAL( 8 ), INTENT( INOUT ) :: GAS    ( : )   ! gas phase concentrations (mol/molV)
           REAL( 8 ), INTENT( INOUT ) :: AEROSOL( :,: ) ! aerosol concentrations (mol/molV)
           REAL( 8 ), INTENT( INOUT ) :: GASWDEP( : )   ! gas phase wet deposition array (mm mol/liter)
           REAL( 8 ), INTENT( INOUT ) :: AERWDEP( :,: ) ! aerosol wet deposition array (mm mol/liter)
           LOGICAL,   INTENT( IN )    :: DARK           ! DARK = TRUE is night,  DARK = FALSE is day
        END SUBROUTINE AQCHEM
      END INTERFACE      

C-----------------------------------------------------------------------

C...Initialization
C...  event-statistics variables.

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

        CALL AQ_DATA_INIT()      

        ALLOCATE ( GAS    ( NGAS ),
     &             GASWDEP( NGAS ),
     &             AEROSOL( NAER, N_MODE),
     &             AERWDEP( NAER, N_MODE),
     &             WSRGAQ ( N_CGRID_SPC ),
     &             STAT = STAT)
        IF (STAT .NE. 0) THEN
           XMSG = 'Memory allocation failure'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

#ifdef sens
C... Allocate sensitivity arrays
        ALLOCATE ( S_HPWDEP( NPMAX ), STAT = STAT )
        IF ( STAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_HPWDEP'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( S_GAS( NPMAX,NGAS ), S_GASWDEP( NPMAX,NGAS ), STAT = STAT )
        IF ( STAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_GAS or S_GASWDEP'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( S_AEROSOL( NPMAX,NAER,N_MODE ), S_AERWDEP( NPMAX,NAER,N_MODE ), STAT = STAT )
        IF ( STAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_AEROSOL or S_AERWDEP'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( S_WSRGAQ( NPMAX,NGAS,MXSRG ), STAT = STAT )
        IF ( STAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_WSRGGAS or S_WSRGAQ'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
#endif

      END IF

C...for subsequent calls, check to make sure some surrogates were
C...  specified, otherwise there is no need to perform aqueous chemistry

      IF ( N_SPC_AQ .EQ. 0 ) RETURN

C...Zero out concentrations

      GAS     = 0.0D0
      GASWDEP = 0.0D0
      AEROSOL = 0.0D0
      AERWDEP = 0.0D0
      WSRGAQ  = 0.0D0
      
      ! Initialize Gas and Aerosol Vectors at Background
      DO ISPC = 1,N_CGRID_SPC
        IAQ = MAP_CGRIDtoSRG( ISPC )
        IF ( IAQ .GT. 0 ) THEN 
          IF ( CGRID_MASK_GAS( ISPC ) .OR.
     &         CGRID_MASK_NR( ISPC ) .OR.
     &         CGRID_MASK_TRAC( ISPC ) ) THEN
             GAS( IAQ ) = REAL( SRGGAS( IAQ )%BACKGND, 8 ) * 1.0D-6
          ELSE IF ( CGRID_MASK_NUM( ISPC ) .OR.
     &              CGRID_MASK_SRF( ISPC ) ) THEN
             AEROSOL( IAQ,: ) = REAL( SRGAER( IAQ )%BACKGND * CTHK1 / AIRM, 8 )

          ELSE IF ( CGRID_MASK_AERO( ISPC ) ) THEN
             AEROSOL( IAQ,: ) = REAL( SRGAER( IAQ )%BACKGND * 1.0E-6 * CTHK1, 8 )
     &                         / REAL( CGRID_MW( ISPC ) * AIRM, 8 )

          END IF
        END IF
      END DO

#ifdef sens
      S_GAS = 0.0D0
      S_GASWDEP = 0.0D0
      S_AEROSOL = 0.0D0
      S_AERWDEP = 0.0D0
      S_WSRGAQ = 0.0D0
#endif

      ! Load all Gas and Aerosol Concentrations
      DO ISPC = 1,N_CGRID_SPC
        IAQ = MAP_CGRIDtoSRG( ISPC )
        IF ( IAQ .GT. 0 ) THEN 

          IF ( CGRID_MASK_GAS( ISPC ) .OR.
     &         CGRID_MASK_NR( ISPC ) .OR.
     &         CGRID_MASK_TRAC( ISPC ) ) THEN
             GAS( IAQ ) = GAS( IAQ ) + REAL( POLC( ISPC ), 8 )
#ifdef sens
             DO NP = 1, NPMAX
                S_GAS( NP, IAQ ) = S_GAS( NP, IAQ ) + S_POLC ( NP, ISPC )
             ENDDO
#endif
          ELSE IF ( CGRID_MASK_AERO( ISPC ) ) THEN
             IM = MAP_CGRIDtoSRGMODE( ISPC )
             AEROSOL( IAQ,IM ) = AEROSOL( IAQ,IM ) + REAL( POLC( ISPC ),8 )
#ifdef sens
             DO NP = 1, NPMAX
                S_AEROSOL( NP, IAQ, IM ) = S_AEROSOL( NP, IAQ, IM ) + S_POLC( NP, ISPC )
             ENDDO
#endif
          END IF
        END IF
      END DO

      ! Now Calculate Surrogate Fractions
      DO ISPC = 1,N_CGRID_SPC
        IAQ = MAP_CGRIDtoSRG( ISPC ) 
        IF ( IAQ .GT. 0 ) THEN 
          IF ( CGRID_MASK_GAS( ISPC ) .OR.
     &         CGRID_MASK_NR( ISPC ) .OR.
     &         CGRID_MASK_TRAC( ISPC ) ) THEN
             IF ( GAS( IAQ ) .GT. 0.0D0 ) THEN
               WSRGAQ( ISPC ) = REAL( POLC( ISPC ), 8 ) / GAS( IAQ )
#ifdef sens
               DO NP = 1, NPMAX
                  IF ( S_GAS( NP, IGAS ) .NE. 0.0D0 ) THEN
                     S_WSRGAQ( NP, ISPC ) = S_POLC( NP,ISPC ) / S_GAS(NP,IGAS)
                  ELSE
                     S_WSRGAQ( NP, ISPC ) = 0.0D0
                  END IF
               END DO
#endif
             ELSE
                WSRGAQ( ISPC ) = 1.0D0 / N_AQSRG_GAS( IAQ )
#ifdef sens
                DO NP = 1, NPMAX
                  S_WSRGAQ( NP, ISPC ) = 0.0D0
                END DO
#endif
             END IF

          ELSE IF ( CGRID_MASK_AERO( ISPC ) ) THEN
             IM = MAP_CGRIDtoSRGMODE( ISPC )
             IF ( AEROSOL( IAER, IM ) .GT. 0.0D0 ) THEN
                WSRGAQ( ISPC ) = REAL( POLC( ISPC ), 8 )
     &                                       / REAL( AEROSOL( IAQ, IM ), 8 )
#ifdef sens
                DO NP = 1, NPMAX
                   IF ( S_AEROSOL( NP, IAQ, IM ) .NE. 0.0D0 ) THEN
                      S_WSRGAQ( NP, ISPC ) = S_POLC( NP, ISPC ) / S_AEROSOL( NP, IAQ, IM )
                   ELSE
                      S_WSRGAQ( NP, ISPC ) = 0.0D0
                   END IF
                END DO
#endif
            ELSE
                WSRGAQ( ISPC ) = 1.0D0 / REAL( N_AQSRG_AER( IAQ, IM ), 8 )
#ifdef sens
                DO NP = 1, NPMAX
                    S_WSRGAQ( NP, ISPC ) = 0.0D0
                ENDDO
#endif
            END IF
          END IF
        END IF

      END DO

C *** extract grid cell concentrations of aero species from CGRID
C     into aerospc_conc in aero_data module

#ifdef sens
      CALL EXTRACT_AERO ( POLC, .FALSE., REAL(S_POLC,4), .TRUE. )
#else
      CALL EXTRACT_AERO ( POLC, .FALSE. )
#endif

C *** Calculate pseudo aerosol 3rd moment (ignore factors that cancel in the division)
C ... M3OLD is in units of m3/kmol_air
      M3OLD = 0.0
      Dens_wmean_old = 0.0
      DO IM = 2, N_MODE

        DO SPC = 1, N_AEROSPC
          IF (  AEROSPC( SPC )%TRACER ) CYCLE 
          IF ( ( AEROSPC( SPC )%NAME( IM ) .NE. ' ' ) .AND.
     &         ( .NOT. AEROSPC( SPC )%NO_M2WET ) ) THEN
             M3OLD( IM ) = M3OLD( IM )
     &                      + ( AEROSPC_CONC( SPC,IM ) * AEROSPC_MW( SPC )
     &                      / AEROSPC( SPC )%DENSITY )
             Dens_wmean_old( IM ) = Dens_wmean_old( IM )  
     &                      + AEROSPC_CONC( SPC,IM ) * AEROSPC_MW( SPC )
          END IF
        END DO
      END DO
      Dens_wmean_old(2:N_MODE) = Dens_wmean_old(2:N_MODE) / M3OLD(2:N_MODE)

C...perform aqueous-phase chemistry calculations

      CALL AQCHEM ( JDATE, JTIME, TBARC, PBARC, TAUCLD, PRATE1,
     &              WCBAR, WTBAR, AIRM, ALFA0, ALFA2, ALFA3, GAS,
     &              AEROSOL, GASWDEP, AERWDEP, HPWDEP, BETASO4, DARK )

C...  compute the scavenging coefficient
      EALFA2T = EXP( -ALFA2 * TAUCLD )

C...store the amount of hydrogen deposition

      REMOVAC = HPWDEP
#ifdef sens
      DO NP = 1, NPMAX
        S_REMOVAC( NP ) = S_HPWDEP( NP )
      END DO
#endif

C...Now, re-apportion mass back into cend/remov (cgrid-type) array

      DO ISPC = 1,N_CGRID_SPC
        IAQ = MAP_CGRIDtoSRG( ISPC )
        IF ( IAQ .GT. 0 ) THEN 

          IF ( CGRID_MASK_GAS( ISPC ) .OR.
     &         CGRID_MASK_NR( ISPC ) .OR.
     &         CGRID_MASK_TRAC( ISPC ) ) THEN
             CEND ( ISPC ) = GAS    ( IAQ ) * WSRGAQ( ISPC )
             REMOV( ISPC ) = GASWDEP( IAQ ) * WSRGAQ( ISPC )
#ifdef sens
             DO NP = 1, NPMAX
               S_CEND ( NP, ISPC ) = S_GAS    ( NP, IAQ ) * S_WSRGAQ( NP, ISPC )
               S_REMOV( NP, ISPC ) = S_GASWDEP( NP, IAQ ) * S_WSRGAQ( NP, ISPC )
             ENDDO
#endif
          
           ELSE IF ( CGRID_MASK_AERO( ISPC ) ) THEN
             IM = MAP_CGRIDtoSRGMODE( ISPC )
             CEND ( ISPC ) = AEROSOL( IAQ, IM ) * WSRGAQ( ISPC )
             REMOV( ISPC ) = AERWDEP( IAQ, IM ) * WSRGAQ( ISPC )
#ifdef sens
             DO NP = 1, NPMAX
                S_CEND ( NP,ISPC ) = S_AEROSOL( NP,IAQ,IM ) * S_WSRGAQ( NP, ISPC )
                S_REMOV( NP,ISPC ) = S_AERWDEP( NP,IAQ,IM ) * S_WSRGAQ( NP, ISPC )
             ENDDO
#endif
          END IF
        END IF
      END DO
      
C *** extract grid cell concentrations of aero species from CGRID
C     into aerospc_conc in aero_data module

#ifdef sens
      CALL EXTRACT_AERO ( CEND, .FALSE., REAL(S_CEND,4), .TRUE. )
#else
      CALL EXTRACT_AERO ( CEND, .FALSE. )
#endif

C *** Calculate pseudo aerosol 3rd moment (ignore factors that cancel in the division)
C ... M3NEW is in units of m3/kmol_air
      M3NEW = 0.0
      Dens_wmean_new = 0.0
      DO IM = 2, N_MODE

        DO SPC = 1, N_AEROSPC
          IF (  AEROSPC( SPC )%TRACER ) CYCLE 
          IF ( ( AEROSPC( SPC )%NAME( IM ) .NE. ' ' ) .AND.
     &         ( .NOT. AEROSPC( SPC )%NO_M2WET ) ) THEN
             M3NEW( IM ) = M3NEW( IM )
     &                      + ( AEROSPC_CONC( SPC,IM ) * AEROSPC_MW( SPC )
     &                      / AEROSPC( SPC )%DENSITY )
             Dens_wmean_new( IM ) = Dens_wmean_new( IM )  
     &                      + AEROSPC_CONC( SPC,IM ) * AEROSPC_MW( SPC )
          END IF
        END DO
      END DO

C...Update aerosol number 
      CEND( AERONUM_MAP( 1 ) ) = MAX( CEND( AERONUM_MAP( 1 ) ), aeromode_minNum( 1 )/ (AIRM/CTHK1) )
      CEND( AERONUM_MAP( 2 ) ) = MAX( CEND( AERONUM_MAP( 2 ) ), aeromode_minNum( 2 )/ (AIRM/CTHK1) )
      CEND( AERONUM_MAP( 3 ) ) = MAX( CEND( AERONUM_MAP( 3 ) ), aeromode_minNum( 3 )/ (AIRM/CTHK1) )

#ifdef sens
C... Remove number sensitivity calculations for now
      S_CEND( :, AERONUM_MAP( 1 ) ) = 0.0
      S_CEND( :, AERONUM_MAP( 2 ) ) = 0.0
      S_CEND( :, AERONUM_MAP( 3 ) ) = 0.0
#endif

C...check for minimums
C...ug/m3*m3/mol_air*1e3 mol_air/kmol_air*1e-9kg/ug*m3/kg=> m3/kmol_air
      DO IM = 2, N_MODE
        IF ( M3NEW( IM ) .GT. 0.0D0 ) THEN  ! Dens_wmean_new will be .GT. 0 also
           Dens_wmean_new( IM ) = Dens_wmean_new( IM ) / M3NEW( IM )
           M3OLD( IM ) = MAX( M3OLD( IM ), CONMIND * 1.0d-6 / ( Dens_wmean_old( IM ) * AIRM / CTHK1 ) )
           M3NEW( IM ) = MAX( M3NEW( IM ), CONMIND * 1.0d-6 / ( Dens_wmean_new( IM ) * AIRM / CTHK1 ) )
        END IF
      END DO

C...Update surface area

      CEND( AEROSRF_MAP( 1 ) ) = POLC( AEROSRF_MAP( 1 ) ) * EALFA2T
      CEND( AEROSRF_MAP( 1 ) ) = MAX( CEND( AEROSRF_MAP( 1 ) ), aeromode_minM2( 1 ) * DPI / (AIRM/CTHK1) )
      
      CEND( AEROSRF_MAP( 2 ) ) = POLC( AEROSRF_MAP( 2 ) )
     &                         * EXP( -BETASO4 * TAUCLD * ONETHIRD )
     &                         * ( M3NEW( 2 ) / M3OLD( 2 ) ) ** TWOTHIRDS
      CEND( AEROSRF_MAP( 2 ) ) = MAX( CEND( AEROSRF_MAP( 2 ) ), aeromode_minM2( 2 ) * DPI / (AIRM/CTHK1) )

      CEND( AEROSRF_MAP( 3 ) ) = POLC( AEROSRF_MAP( 3 ) )
     &                         * ( CEND( AERONUM_MAP( 3 ) )
     &                             / POLC( AERONUM_MAP( 3 ) ) ) ** ONETHIRD
     &                         * ( M3NEW( 3 ) / M3OLD( 3 ) ) ** TWOTHIRDS
      CEND( AEROSRF_MAP( 3 ) ) = MAX( CEND( AEROSRF_MAP( 3 ) ), aeromode_minM2( 3 ) * DPI / (AIRM/CTHK1) )

#ifdef sens
C... Remove surface area sensitivity calculations for now
      S_CEND( :, AEROSRF_MAP( 1 ) ) = 0.0
      S_CEND( :, AEROSRF_MAP( 2 ) ) = 0.0
      S_CEND( :, AEROSRF_MAP( 3 ) ) = 0.0
#endif

      RETURN
      END

