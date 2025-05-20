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

      MODULE MASS_TRANSFER
      
      USE AERO_DATA, ONLY : N_MODE
      USE RUNTIME_VARS, ONLY : AERO_MASS_TRANSFER

      IMPLICIT NONE


      INTEGER, PARAMETER :: NAER = 10      ! number of particle species
      INTEGER, PARAMETER :: NGAS = 4       ! number of gaseous species
 
      REAL( 8 ), SAVE :: CGAS( NGAS )
      REAL( 8 ), SAVE :: CGAS0( NGAS )

      ! Indices for inorganic particle species
      INTEGER, PARAMETER :: KSO4 = 1 
      INTEGER, PARAMETER :: KNH4 = 2 
      INTEGER, PARAMETER :: KNO3 = 3 
      INTEGER, PARAMETER :: KCL  = 4 
      INTEGER, PARAMETER :: KH2O = 5
      INTEGER, PARAMETER :: KH3OP= 6
      INTEGER, PARAMETER :: KNA  = 7 
      INTEGER, PARAMETER :: KMG  = 8 
      INTEGER, PARAMETER :: KK   = 9 
      INTEGER, PARAMETER :: KCA  = 10 

      ! Populate Names for Inorganic Aerosol Species
      TYPE INORG_AER_TYPE
          CHARACTER( 16 ) :: NAME
          LOGICAL         :: SEMIVOL
          INTEGER         :: ISRP
          INTEGER         :: BUDGET
      END TYPE INORG_AER_TYPE

      TYPE ( INORG_AER_TYPE ), PARAMETER :: ATTR( NAER ) = (/
                   !    Name     Volatile?  ISORROPIA Index  Budget Index
                   !  ---------- ---------  ---------------  ------------
     & INORG_AER_TYPE( 'ASO4   ' ,.TRUE.     ,  2          ,  1 ),
     & INORG_AER_TYPE( 'ANH4   ' ,.TRUE.     ,  3          ,  2 ),
     & INORG_AER_TYPE( 'ANO3   ' ,.TRUE.     ,  4          ,  3 ),
     & INORG_AER_TYPE( 'ACL    ' ,.TRUE.     ,  5          ,  4 ),
     & INORG_AER_TYPE( 'AH2O   ' ,.TRUE.     ,  0          ,  5 ),
     & INORG_AER_TYPE( 'AH3OP  ' ,.FALSE.    ,  0          ,  6 ),
     & INORG_AER_TYPE( 'ANA    ' ,.FALSE.    ,  1          ,  0 ),
     & INORG_AER_TYPE( 'AMG    ' ,.FALSE.    ,  8          ,  0 ),
     & INORG_AER_TYPE( 'AK     ' ,.FALSE.    ,  7          ,  0 ),
     & INORG_AER_TYPE( 'ACA    ' ,.FALSE.    ,  6          ,  0 ) /)

                   ! For ISORROPIA indices, 9 = other

      ! Indices for inorganic gas species
      INTEGER, PARAMETER :: KH2SO4 = 1 
      INTEGER, PARAMETER :: KNH3   = 2
      INTEGER, PARAMETER :: KHNO3  = 3
      INTEGER, PARAMETER :: KHCL   = 4

      ! Populate Names for Inorganic Gas Species
      TYPE INORG_GAS_TYPE
          CHARACTER( 16 ) :: NAME
          INTEGER         :: KAER
          INTEGER         :: ISRP
          INTEGER         :: BUDGET
      END TYPE INORG_GAS_TYPE

      TYPE ( INORG_GAS_TYPE ), PARAMETER :: GTTR( NGAS ) = (/
                   !    Name     AERO Index   ISORROPIA Index  Budget Index
                   !  ---------- ----------   ---------------  ------------
     & INORG_GAS_TYPE( 'SULF   ' ,KSO4       ,  0            ,  1 ),
     & INORG_GAS_TYPE( 'NH3    ' ,KNH4       ,  1            ,  2 ),
     & INORG_GAS_TYPE( 'HNO3   ' ,KNO3       ,  2            ,  3 ),
     & INORG_GAS_TYPE( 'HCL    ' ,KCL        ,  3            ,  4 ) /)

      REAL( 8 ), SAVE :: CFAC_AER(NAER)      ! 1.0e-6/MW [ug/m3 --> mol/m3]
      REAL( 8 ), SAVE :: CFAC_AER_INV(NAER)  ! 1.0e6*MW  [mol/m3 --> ug/m3]
      REAL( 8 ), SAVE :: CFAC_ASEACAT        ! 1.0e-6/MW [ug/m3 --> mol/m3]
      REAL( 8 ), SAVE :: CFAC_GAS(NGAS)      ! 1.0e-6/MW [ug/m3 --> mol/m3]
      REAL( 8 ), SAVE :: MASStoVOL_AER(NAER) ! 1.0e-9*6pi/rho [ug/m3 --> m3/m3]
      REAL( 8 ), SAVE :: MASStoVOL_SOIL      ! 1.0e-9*6pi/rho [ug/m3 --> m3/m3]
      REAL( 8 ), SAVE :: MASStoVOL_CORS      ! 1.0e-9*6pi/rho [ug/m3 --> m3/m3]

      REAL( 8 ), SAVE :: MW_AER(NAER)      ! g mol-1
      REAL( 8 ), SAVE :: MW_GAS(NGAS)      ! g mol-1
      REAL( 8 ), SAVE :: MW_RATIO(NGAS)    ! [g mol-1]_gas / [g mol-1]_aer
      REAL( 8 ), SAVE :: MW_RATIO_INV(NGAS)! [g mol-1]_aer / [g mol-1]_gas
      REAL( 8 ), SAVE :: AER_CHARGE(NAER)  ! ion charge in aerosol species

      ! ISORROPIA MAPS
      INTEGER, PARAMETER :: NISRP = 8
      INTEGER, PARAMETER :: MAP_ISRP( NISRP ) = (/ KNA, KSO4, KNH4, KNO3,
     &                                             KCL, KCA,  KK,   KMG  /)
 
      ! Budget Maps
      INTEGER, PARAMETER :: N_COND_BUDGET=6 ! number of species in condensation budget array
      INTEGER, PARAMETER :: MAP_BUDGETtoINORG( N_COND_BUDGET ) = (/
     &                           KSO4, KNH4, KNO3, KCL, KH2O, KH3OP /)
 
      ! Logical for Equilibrium or Dynamic Mode (Default to Dynamic)
      LOGICAL, SAVE :: L_MODE_EQB( N_MODE ) = .FALSE.

      CONTAINS

C routines for aerosol mass transfer
      SUBROUTINE VOLINORG( DT, COL, ROW, LAYER, DV_SO4, CGR )

C *** Calculates the partitioning of inorganic components (CL,NO3,NH4,SO4)
C     between the aerosol and gas phase over the operator synchronization
C     timestep (DT). Partitioning is calculated using the Hybrid approach,
C     where dynamic mass transfer of species to/from the coarse mode is
C     calculated using multiple sub-operator time steps (TSTEP) and the
C     fine modes are equilibrated with the gas phase. The mass transfer
C     calculations are made using the H+ flux-limiting approach of Pilinis
C     et al. (2000). If 'OPTIONFLAG' is not set to 'Hybrid', the mass
C     transfer calculations for the coarse mode are skipped, and the fine
C     modes are equilibrated with the gas phase.

C     Returns updated volatile inorganic species concentrations in the gas
C     and particulate phase, and the aerosol modal parameters

C *** Revision history: 4/07 - Moved HCOND3 and NEWPART3 calls from 
C                              AEROPROC to this subroutine for 
C                              mass transfer calculation  
C     15 Jul 08 J.Young, P.Bhave: increased cutoff to hybrid from .01 to .05 ug/m**3
C               J.Young: change 'OPTIONFLAG' to just a logical variable, 'Hybrid'

C     10 Mar 11 S.Howard Renamed met_data to aeromet_data
C     25 Mar 11 S.Roselle Replaced I/O API include files with UTILIO_DEFN
C     26 Apr 11 G.Sarwar Replaced existing ISORROPIA with ISORROPIA 2.1
C               Updated coarse-mode aerosol speciation and H+ calculation  
C     12 Apr 16 H. Pye and B. Murphy: Update with consistent treatment for wet particles.
C     15 Apr 16 J.Young: Use aerosol factors from AERO_DATA module named constants

C *** References
C 1. Pilinis C, Capaldo KP, Nenes A, Pandis SN (2000) MADM - A new
C    multicomponent aerosol dynamics model. AEROSOL SCIENCE AND TECHNOLOGY.
C    32(5):482-502
C
C 2. Capaldo KP, Pilinis C, Pandis SN (2000) A computationally efficient hybrid
C    approach for dynamic gas/aerosol transfer in air quality models. ATMOSPHERIC
C    ENVIRONMENT. 34(21):3617-3627
C
C 3. Fountoukis C, Nenes, A (2007) ISORROPIA II: a comnputationally efficient 
C    thermodynamic equilibrium model for K+-Ca2+-Mg2+-NH4+-SO42--NO3--Cl-H2O 
C    aerosols. ATMOSPHERIC CHEMISTRY AND PHYSICS. 7, 4639-4659

      USE AERO_DATA
      USE PRECURSOR_DATA
      USE CGRID_SPCS, ONLY : N_GC_G2AE, GC_G2AE, GC_STRT, GC_G2AE_MAP, 
     &                       N_NR_N2AE, NR_N2AE, NR_STRT, NR_N2AE_MAP,
     &                       CGRID_MW
      USE AEROMET_DATA
      USE CONST
      USE UTILIO_DEFN

#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX
      USE AERO_DDM3D, ONLY : S_SO4RATE, S_PRECURSOR_CONC
#endif

      IMPLICIT NONE

C *** Arguments:
      REAL    DT              ! time step [sec]
      INTEGER COL             ! grid column index
      INTEGER ROW             ! grid row index
      INTEGER LAYER           ! model layer index
      REAL    DV_SO4          ! molecular diffusivity of H2SO4 vapor 
                              ! after correction for ambient conditions
      REAL( 8 ) :: CGR( N_MODE-1 ) ! 3rd moment SO4 growth rate [m^3/m^3-s]

C *** Parameters: 
      INTEGER, PARAMETER :: NVOLINORG = 3   ! number of gaseous species

      REAL( 8 ), PARAMETER :: D_TWOTHIRDS = 2.0D0 / 3.0D0

      REAL, PARAMETER :: CUTOFF = 0.05  ! [ug/m**3]
      REAL, PARAMETER :: ALPHSULF = 0.1 ! Accommodation coefficient for sulfuric acid
                                        ! see Capaldo et al. (2000)

C *** Local Variables:

C *** Inputs to subroutine HCOND3

      REAL, SAVE :: COFCBAR_SO4  ! Temperature-independent coefficients
                                 ! for caculating molecular vel [m/s]
                                 ! = sqrt((8*Rgas)/(pi*MW)) 
      REAL         CBAR_SO4      ! molecular velocity of H2SO4                      

      REAL( 8 ) :: AM0( N_MODE ) ! zeroth moments
      REAL( 8 ) :: AM1( N_MODE ) ! first moments
      REAL( 8 ) :: AM2( N_MODE ) ! second moments

      REAL( 8 ) :: M2DRY_INIT( N_MODE )  ! Dry Second Moment Initial Array
      REAL( 8 ) :: M3DRY_INIT( N_MODE )  ! Dry Third Moment Initial Array
      REAL( 8 ) :: M2WET_INIT( N_MODE )  ! Wet Second Moment Initial Array
      REAL( 8 ) :: M3WET_INIT( N_MODE )  ! Wet Third Moment Initial Array
      REAL( 8 ) :: M2WET_FINAL( N_MODE ) ! Wet Second Moment Final Array
      REAL( 8 ) :: M3WET_FINAL( N_MODE ) ! Wet Third Moment Final Array

C *** Outputs from HCOND3: size-dependent term in the condensational-growth 
C     expressions defined in Equations A13-A14 of [Binkowski & Shankar,1995]
      REAL( 8 ) :: FCONC_SO4( N_MODE,2 )  ! All sizes 2nd and 3rd moments
      REAL( 8 ) :: FCONC_OUT( 2 )         ! Output for HCOND3, 2nd and 3rd moments
      REAL( 8 ) :: FCONCM1_SO4       ! reciprocals of total SO4 cond rates

C *** Modal partition factors [ dimensionless ]
C     defined in Equations A17-A18 of [Binkowski & Shankar,1995]
      REAL( 8 ) :: OMEGA_AT_SO4  ! Aitken mode 2nd and 3rd moments
      REAL( 8 ) :: OMEGA_AC_SO4  ! Accumulation mode 2nd and 3rd moments
      REAL( 8 ) :: OMEGA( 2 )    ! partitioning coefficient for equilibrium PM mass
      REAL( 8 ) :: PHI( NAER,2 ) ! Mass Fraction of each component in each aerosol mode
      REAL( 8 ) :: TOTAER( NAER )! Total aerosol component across multiple modes 

C *** Variables for new particle formation:
      REAL XH2SO4            ! steady state H2SO4 concentration
      REAL( 8 ) :: DMDT_SO4  ! particle mass production rate [ ug/m**3 s ]
      REAL( 8 ) :: DNDT      ! particle number production rate [ # / m**3 s ]
      REAL( 8 ) :: DM2DT     ! second moment production rate [ m**2 / m**3 s]
      REAL( 8 ) :: SCONDRATE ! SO4 condensation rate [ ug/m**3 s ]

C *** Mode-specific sulfate production rate [ ug/m**3 s ]
      REAL( 8 ) :: CONDSO4( N_MODE )    ! sulfate condensation rate [ ug/m**3 s ]
      REAL( 8 ) :: RATE                 ! CONDSO4 or cond+nucl rate

C *** Size-dependent portion of mass-transfer rate equation
      REAL( 8 ) :: GRFAC1( N_MODE )     ! 2nd moment [ m**2/m**3-s ] 
      REAL( 8 ) :: GRFAC2( N_MODE )     ! 3rd moment [ m**3/m**3-s ] 
      REAL( 8 ) :: GRFAC2TOT            ! 2nd moment across all equilibrium modes [ m**3/m**3-s ] 
      
C *** ISORROPIA input variables
      REAL( 8 ) :: WI( NAER - 2 )       ! species array [ mol/m**3 ]
      REAL( 8 ) :: RHI                  ! relative humidity [ fraction ]
      REAL( 8 ) :: TEMPI                ! temperature   [ deg K]
      REAL( 8 ) :: CNTRL( 2 )           ! ISOROPIA control parameters 

C *** ISORROPIA output variables
      REAL( 8 ) :: WT( NAER - 2 )       ! species output array (unused)
      REAL( 8 ) :: ISRP_GAS( 3 )        ! gas-phase   "     " 
      REAL( 8 ) :: ISRP_AERLIQ( 15 )    ! liq aerosol "     " 
      REAL( 8 ) :: ISRP_AERSLD( 19 )    ! solid "     "     "  (unused)
      REAL( 8 ) :: OTHER( 9 )           ! supplmentary output array (unused)
      CHARACTER( 15 ) :: SCASI          ! subcase number output (unused)

C *** Variables to account for mass conservation violations in ISRP3F
      LOGICAL TRUSTCL                   ! false if ISOROPIA's partitioning       
                                        !  of Cl/HCl is to be ignored
C *** Variables for volatile species mass transfer between gas and aerosol and
C     mass partitioning between the modes 
      LOGICAL DO_COARSE_MT              ! coarse-mode mass transfer option flag 
                                        !  (mass transfer if .TRUE.)
      REAL( 8 ) :: DELT                 ! time step DT [s]
      REAL( 8 ) :: HPLUS( N_MODE )      ! scratch var for H+ [umol/m**3]

      REAL( 8 ) :: DVOLINORG( NGAS )      ! vol inorg spcs mass to be xferred [mol/m3]
      REAL( 8 ) :: DVOLMAX                ! max value for DVOLINORG 
      REAL( 8 ) :: CINORG( NAER,N_MODE )  ! scratch array for inorg spcs [ug/m**3]
      REAL( 8 ) :: CINORG0( NAER,N_MODE ) ! scratch array for inorg spcs [ug/m**3]
      REAL( 8 ) :: ASEACAT                ! coarse sea-salt cations [ug/m**3]
      REAL( 8 ) :: INT_TIME               ! internal mass transfer time (s)
      REAL( 8 ) :: TSTEP                  ! mass transfer time step [s]
      REAL( 8 ) :: DRYM20, Y              ! scratch vars for 2nd moment [m**2/m**3]
      REAL( 8 ) :: M3OTHR                 ! vars for 3rd moment calculation [m**3/m**3]
      REAL( 8 ) :: J( NGAS )              ! condensation/evaporation flux [mol/m**3-s]
      REAL( 8 ) :: CFINAL( NAER,N_MODE )  ! conc after mass xfer step [ug/m**3]
      REAL( 8 ) :: H2O_NEW                ! Update of LWC after condensation 
      REAL( 8 ) :: H2O_NEW_INV            ! Reciprocal of update of LWC after condensation 
      REAL( 8 ) :: SO4                    ! modal SO4 after condensation or cond + nucl
      REAL( 8 ) :: DDRYM3DT               ! rate of 3rd moment transfer - dry inorg spcs
      REAL( 8 ) :: DDRYM2DT               ! rate of 2nd moment transfer -  "     "    "
      REAL( 8 ) :: DRYM3, WETM3           ! scratch vars for 3rd moment calc [m**3/m**3]
      REAL( 8 ) :: DRYM2, WETM2           ! scratch vars for 2nd moment calc [m**2/m**3]
      REAL( 8 ) :: LOSS                   ! rate of loss of second moment [1/s] 
      REAL( 8 ) :: EQLBHIJ                ! H+ concentration from isoropia for I plus J-modes [ug/m**3]

      REAL( 8 ) :: DELNUM( N_MODE ) ! Change in Number due to some process
      REAL( 8 ) :: DELSRF( N_MODE ) ! Change in Surface Area due to some process
      REAL( 8 ) :: DAER( NAER,N_MODE ) ! Change in aerosol mass
      REAL( 8 ) :: CBUDGET0_NUM( N_MODE ) ! Initial Number before some process
      REAL( 8 ) :: CBUDGET0_SRF( N_MODE ) ! Initial Surface Area before some process
      REAL( 8 ) :: CBUDGET0_MASS( NGAS+2, N_MODE ) ! Initial Mass before some process

      INTEGER, SAVE :: MAP_INORGtoAERO( NAER )
      INTEGER, SAVE :: MAP_IGAStoCGRID( NGAS )
      INTEGER, SAVE :: MAP_IGAStoPREC( NGAS )

      INTEGER, SAVE :: IAER_SO4, IGAS_SULF

      REAL( 8 ) :: TMP
      INTEGER I, N, IB, ISRP, K, P,       ! loop and array indices
     &        IAER, KAER
      INTEGER IMODE, IM                   ! mode loop index  
      INTEGER ISTEP                       ! loop index, mass transfer time step loop
      INTEGER ISP                         ! loop index, species loop
      LOGICAL TrustIso                    ! For negative vap. press., TrustIso = F

C *** Local Saved Variables 
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL, SAVE :: FIRSTWRITE = .TRUE.

#ifdef sens
      INTERFACE
         SUBROUTINE AERO_SENS( SCASI, W )
           CHARACTER( 15 ), INTENT( IN ) :: SCASI    ! (INPUT) subcase number output
           REAL(KIND=8),    INTENT( IN ) :: W( : )   ! (INPUT) WI concentrations 
         END SUBROUTINE AERO_SENS
      END INTERFACE 
#endif

C-----------------------------------------------------------------------
 
      IF ( FIRSTIME ) THEN
      
         FIRSTIME = .FALSE.

         ! Determine whether each mode should be solved with a Dynamic 
         ! or Equilibrium approach. The default is hybrid.
         L_MODE_EQB( : )  = AEROMODE( : )%HYB_EQB

         IF ( AERO_MASS_TRANSFER .EQ. 'DYN' ) THEN
            L_MODE_EQB(:) = .FALSE.

         ELSE IF ( AERO_MASS_TRANSFER .EQ. 'EQB' ) THEN
            L_MODE_EQB(:) = .TRUE.
         END IF

         ! Initialize vectors for mapping aerosols and gases
         MAP_INORGtoAERO = 0
         MAP_IGAStoCGRID = 0
         
         CFAC_AER(:) = 0.0D0
         CFAC_AER_INV(:) = 0.0D0
         CFAC_GAS(:) = 0.0D0
         MASStoVOL_AER(:) = 0.0D0

         MW_AER = 0.0D0
         MW_GAS = 0.0D0
         MW_RATIO = 0.0D0
         MW_RATIO_INV = 0.0D0

         ! Map Aerosol Species
         DO K = 1,NAER
            I = INDEX1( ATTR( K )%NAME, N_AEROSPC, AEROSPC(:)%BULKNAME )
            IF ( I .GT. 0 ) THEN
               MAP_INORGtoAERO( K ) = I
               MW_AER( K ) = REAL( AEROSPC_MW( I ), 8 ) 
               CFAC_AER( K ) = 1.0D-6 / MW_AER( K ) ! Conversion ug m-3 to mol m-3
               CFAC_AER_INV( K ) = 1.0D0 / CFAC_AER( K )
               MASStoVOL_AER( K ) = 1.0D-9 * F6DPI / REAL( AEROSPC(I)%DENSITY, 8 )      ! Conversion ug m-3 to m3 m-3
               AER_CHARGE( K ) = REAL( AEROSPC( I )%CHARGE, 8 )
            END IF
         END DO
         CFAC_AER( KH2O ) = 1.0D-6 / 18.0153D0    ! MW( AH2O ) is 18.0, and this disagrees with the 
         CFAC_AER_INV( KH2O ) = 1.0D6 * 18.0153D0 ! global parameter MWWAT, which equals 18.0153.
                                                  ! Set this parameter manually.
         CFAC_ASEACAT = 1.0D-6 / 23.75D0

         MASStoVOL_SOIL = 1.0D-9 * F6DPI / REAL( AEROSPC( ASOIL_IDX )%DENSITY, 8 )
         MASStoVOL_CORS = 1.0D-9 * F6DPI / REAL( AEROSPC( ACORS_IDX )%DENSITY, 8 )

         ! Map Gas Species
         DO K = 1,NGAS
            P = 0
            I = INDEX1( GTTR( K )%NAME, N_GC_G2AE, GC_G2AE )
            IF ( I .GT. 0 ) THEN
               MAP_IGAStoCGRID( K ) = gc_strt - 1 + gc_g2ae_map( I )
               P = I
            END IF
            I = INDEX1( GTTR( K )%NAME, N_NR_N2AE, NR_N2AE )
            IF ( I .GT. 0 ) THEN
               MAP_IGAStoCGRID( K ) = nr_strt - 1 + nr_n2ae_map( I )
               P = I
            END IF
            IF ( P .GT. 0 ) THEN
               MW_GAS( K ) = REAL( CGRID_MW( MAP_IGAStoCGRID(K) ), 8 )
               CFAC_GAS( K ) = 1.0D-6 / MW_GAS( K )  ! Conversion ug m-3 to mol m-3

               MW_RATIO( K ) = MW_GAS( K ) / MW_AER( GTTR(K)%KAER )
               MW_RATIO_INV( K ) = MW_AER( GTTR(K)%KAER ) / MW_GAS( K )
            END IF
         END DO

         ! Map Gas Species to Precursor Array
         MAP_IGAStoPREC( KH2SO4 ) = SULF_IDX
         MAP_IGAStoPREC( KNH3 )   = NH3_IDX
         MAP_IGAStoPREC( KHNO3 )  = HNO3_IDX
         MAP_IGAStoPREC( KHCL )   = HCL_IDX

         ! Sulfate and Sulfuric Acid are Special. Save their pointers
         IAER_SO4 = MAP_INORGtoAERO( KSO4 )
         IGAS_SULF = MAP_IGAStoCGRID( KH2SO4 )

         ! Calculate temperature-independent part of molecular speed
         COFCBAR_SO4 = SQRT( 8.0 * RGASUNIV / ( PI * REAL( MW_GAS( KH2SO4 ),4 ) * 1.0E-3 ) )
      END IF   ! First Time

C *** Determine if Coarse-Mode Mass Transfer will be Executed
      TMP = 0.0
#ifdef noiso
      DO_COARSE_MT = .false.
#else
      DO I = 1, N_AEROSPC
         IF(  AEROSPC( I )%tracer )CYCLE
         IF ( AEROSPC( I )%CHARGE .NE. 0 ) TMP = TMP + AEROSPC_CONC( I,N_MODE )
      END DO
      DO_COARSE_MT = ( TMP .GE. CUTOFF ) .AND. ( AIRRH .GE. 0.18 )
#endif

      DELT  = REAL( DT, 8 )
      TEMPI = AIRTEMP             ! assume const within synch timestep
      RHI   = MIN( 0.95, AIRRH )  ! "        "     "      "     "

C *** Calculate H+ concentration
      HPLUS = 0.0
      DO I = 1, N_MODE
         DO N = 1, N_AEROSPC
            IF(  AEROSPC( N )%tracer )CYCLE
            HPLUS( I ) = HPLUS( I )
     &                 - AEROSPC( N )%CHARGE * AEROSPC_CONC( N,I ) / AEROSPC_MW( N )
         END DO
      END DO

C *** Condensational Growth (Size-dependent terms)
C     Calculate intermediate variables needed to determine the 2nd and
C     3rd moment condensational-growth rates.  3rd moment terms are 
C     needed for the calculation of new particle production.  See 
C     Section 3.3 of Jiang & Roth (2003) for a detailed discussion.
C    
C *** Set moments using Equation 4 of Binkowski & Shankar
C     (1995) or Equation 3 of Binkowski and Roselle (2003).
C     N.B: these are for a "wet" size distribution
      DO I = 1, N_MODE
         AM0( I ) = MOMENT0_CONC( I ) 
         AM1( I ) = MOMENT0_CONC( I ) * AEROMODE_DIAM( I )
     &            * EXP( 0.5 * AEROMODE_LNSG( I ) * AEROMODE_LNSG( I ) )
         AM2( I ) = MOMENT2_CONC( I )
      END DO
      
      IF ( IAER_SO4 .GT. 0 ) THEN
         ! Calculate molecular velocities (temperature dependent) and
         ! the size-dependent terms in the condensational-
         ! growth factor expressions for sulfate using 
         ! Equations A13-A14 of Binkowski & Shankar (1995). 
         CBAR_SO4 = COFCBAR_SO4 * SQRT( AIRTEMP )
         DO I = 1, N_MODE
            CALL HCOND3( AM0( I ), AM1( I ), AM2( I ),
     &                   DV_SO4, ALPHSULF, CBAR_SO4, FCONC_OUT )
            FCONC_SO4( I, : ) = FCONC_OUT( : )

            IF ( .NOT. DO_COARSE_MT ) THEN
               FCONC_SO4( N_MODE,1 ) = 0.0D0
               FCONC_SO4( N_MODE,2 ) = 0.0D0
            END IF

            !IF ( AERO_MISSING( IAER_SO4,I ) ) FCONC_SO4( I,: ) = 0.0D0

            GRFAC1( I ) = FCONC_SO4( I,1 )
            GRFAC2( I ) = FCONC_SO4( I,2 )
         END DO 

         ! New Particle Production
         ! Calculate the new particle production rate due to binary
         ! nucleation of H2O and H2SO4.  These calculations are performed 
         ! only when the gas-phase production rate of H2SO4 (i.e., SO4RATE) 
         ! is non-zero.  The condensation rate of H2SO4 is calculated as the
         ! gas-phase production rate minus the new particle production rate.

         ! Initialize Variables
         DMDT_SO4  = 0.0D0
         DNDT      = 0.0D0
         DM2DT     = 0.0D0
         SCONDRATE = 0.0D0


         ! Produce new particles only during time steps when the gas-phase 
         ! production rate of H2SO4 is non-zero

         IF ( SO4RATE .NE. 0.0D0 ) THEN

            ! Adjust sulfuric acid vapor concentration to a value in
            ! equilibrium with the production of new particles and the
            ! condensation of sulfuric acid vapor on existing particles, based 
            ! on Equations A21 and A23 of Binkowski & Shankar (1995).
            TMP = 0.0
            DO I = 1, N_MODE
               TMP = TMP + FCONC_SO4( I,2 )
            END DO

            XH2SO4 = SO4RATE / REAL( TMP,4 )
            XH2SO4 = MAX( XH2SO4, CONMIN )

#ifdef sens
            DO NP = 1, NPMAX
               IF ( XH2SO4 .EQ. CONMIN ) THEN
                  S_PRECURSOR_CONC( SULF_IDX, NP ) = 0.D0
               ELSE
                  S_PRECURSOR_CONC( SULF_IDX, NP ) = S_SO4RATE( NP ) / REAL(TMP,8)
               ENDIF
            END DO
#endif

            ! Calculate new particle production rate for 0th, 2nd, & 3rd moments
            CALL NEWPART3 ( AIRRH, AIRTEMP, XH2SO4, SO4RATE,
     &                      DNDT, DMDT_SO4, DM2DT )
         
            ! Calculate sulfate condensation rate as the gas-phase production 
            ! rate minus the new particle production rate, following Equation
            ! 3.23 of Jiang & Roth (2003).
            SCONDRATE = MAX( SO4RATE - DMDT_SO4, 0.0D0 )

            ! Propagate NPF Change to budget variables
            IF ( IGAS_SULF .GT. 0 ) 
     &         NPF_BUDGET( PRECURSOR_MAP( SULF_IDX ) ) = -DMDT_SO4 
     &                   * MW_RATIO( KH2SO4 )  * DELT ! ug m-3 of Sulfuric Acid
                                                      ! (converted to ppmv in update_precursor)

            NPF_BUDGET( AEROSPC_MAP( IAER_SO4,1 ) ) =  DMDT_SO4 * DELT ! ug m-3
            NPF_BUDGET( AERONUM_MAP( 1 ) ) =  DNDT * DELT  ! N m-3
            NPF_BUDGET( AEROSRF_MAP( 1 ) ) =  DM2DT * DELT ! m2 m-3

         END IF   ! SO4RATE .NE. 0

         ! Sulfate Condensation (Size-resolved)
         ! Calculate rate at which condensing sulfate should be added to each
         ! mode.  The "omega" factors are defined in Equations 7a and 7b of
         ! Binkowski & Shankar (1995). The i-mode and j-mode factors are 
         ! calculated using Equation A17 of Binkowski & Shankar (1995). The 
         ! condensation rate for accumulation mode (fine-equilibrium scheme) or 
         ! coarse mode is computed by difference, to avoid mass conservation 
         ! violations arising from numerical error.
         TMP = 0.0
         DO I = 1, N_MODE
            TMP = TMP + FCONC_SO4( I,2 )
         END DO

         FCONCM1_SO4  = 1.0D0 / TMP
         OMEGA_AT_SO4 = FCONCM1_SO4 * FCONC_SO4( 1,2 )
         OMEGA_AC_SO4 = FCONCM1_SO4 * FCONC_SO4( 2,2 )

         ! Growth values for mode merge condition
         CGR( 1 ) = MASStoVOL_AER( KSO4 ) * SCONDRATE * OMEGA_AT_SO4
         CGR( 2 ) = MASStoVOL_AER( KSO4 ) * SCONDRATE * OMEGA_AC_SO4

         ! Diagnose Condensaiton Sink to Each Mode
         CONDSO4( 1 ) = OMEGA_AT_SO4 * SCONDRATE      
      
         IF ( DO_COARSE_MT ) THEN 
            CONDSO4( 2 ) = OMEGA_AC_SO4 * SCONDRATE      
            CONDSO4( 3 ) = SCONDRATE - ( CONDSO4( 1 ) + CONDSO4( 2 ) ) 
         ELSE                                  ! fine equilibrium
            CONDSO4( 2 ) = SCONDRATE - CONDSO4( 1 )
            CONDSO4( 3 ) = 0.0D0               ! no coarse mode chemistry
         END IF

      END IF   ! IAER_SO4
 
      ! Initialize Budget Counter Variables         
      CBUDGET0_NUM( : ) = REAL( MOMENT0_CONC( : ), 8)
      CBUDGET0_NUM( 1 ) = CBUDGET0_NUM( 1 ) + DNDT * DELT
      CBUDGET0_SRF( : ) = MOMENT2_CONC( : ) * PI
      CBUDGET0_SRF( 1 ) = CBUDGET0_SRF( 1 ) + DM2DT * DELT * PI
      DO I = 1,NAER
        IF ( ATTR(I)%BUDGET .GT. 0 ) THEN
          IB = ATTR( I )%BUDGET
          IF ( MAP_INORGtoAERO( I ) .GT. 0 ) THEN
            CBUDGET0_MASS( IB,: ) = REAL( AEROSPC_CONC(MAP_INORGtoAERO( I ),:),8 )
            IF ( I .EQ. KSO4 ) THEN
              CBUDGET0_MASS( IB,1 ) = CBUDGET0_MASS( IB,1 ) + DMDT_SO4 * DELT
            END IF
          END IF
        END IF
      END DO


      ! Begin Condensation and Evaporation Calculations
      CINORG  = 0.0
      CINORG0 = 0.0
      CFINAL  = 0.0

      ! Load initial concentrations for all modes
      DO IMODE = 1,N_MODE
        DO I = 1,NAER
          IF ( MAP_INORGtoAERO(I) ) THEN
             IF ( .NOT. AERO_MISSING( MAP_INORGtoAERO(I),IMODE ) ) THEN
                ! Explicit Aerosol Species - load CINORG
                CINORG( I,IMODE ) = REAL( AEROSPC_CONC( MAP_INORGtoAERO(I),IMODE ),8 )
             END IF
          END IF
        END DO

        ! Load Sea Spray Cation Concentrations
        IF ( .NOT. AERO_MISSING( ASEACAT_IDX,IMODE ) ) THEN
           CINORG( KNA,IMODE ) = CINORG( KNA,IMODE ) + 
     &       REAL( AEROSPC_CONC( ASEACAT_IDX,IMODE ),8 ) * ASCAT_NA_FAC
           CINORG( KMG,IMODE ) = CINORG( KMG,IMODE ) + 
     &       REAL( AEROSPC_CONC( ASEACAT_IDX,IMODE ),8 ) * ASCAT_MG_FAC
           CINORG( KK,IMODE )  = CINORG( KK,IMODE ) + 
     &       REAL( AEROSPC_CONC( ASEACAT_IDX,IMODE ),8 ) * ASCAT_K_FAC
           CINORG( KCA,IMODE ) = CINORG( KCA,IMODE ) + 
     &       REAL( AEROSPC_CONC( ASEACAT_IDX,IMODE ),8 ) * ASCAT_CA_FAC
        END IF

        ! Load Soil Concentrations
        IF ( .NOT. AERO_MISSING( ASOIL_IDX,IMODE ) ) THEN
           CINORG( KNA,IMODE ) = CINORG( KNA,IMODE ) + 
     &       REAL( AEROSPC_CONC( ASOIL_IDX,IMODE ),8 ) * ASOIL_NA_FAC/ASOIL_RENORM
           CINORG( KMG,IMODE ) = CINORG( KMG,IMODE ) + 
     &       REAL( AEROSPC_CONC( ASOIL_IDX,IMODE ),8 ) * ASOIL_MG_FAC/ASOIL_RENORM
           CINORG( KK,IMODE )  = CINORG( KK,IMODE ) + 
     &       REAL( AEROSPC_CONC( ASOIL_IDX,IMODE ),8 ) * ASOIL_K_FAC /ASOIL_RENORM
           CINORG( KCA,IMODE ) = CINORG( KCA,IMODE ) + 
     &       REAL( AEROSPC_CONC( ASOIL_IDX,IMODE ),8 ) * ASOIL_CA_FAC/ASOIL_RENORM
        END IF

        ! Load Anthropogenic Coarse Concentrations
        IF ( .NOT. AERO_MISSING( ACORS_IDX,IMODE ) ) THEN
           CINORG( KNA,IMODE ) = CINORG( KNA,IMODE ) + 
     &       REAL( AEROSPC_CONC( ACORS_IDX,IMODE ),8 ) * ACORS_NA_FAC/ACORSEM_RENORM
           CINORG( KMG,IMODE ) = CINORG( KMG,IMODE ) + 
     &       REAL( AEROSPC_CONC( ACORS_IDX,IMODE ),8 ) * ACORS_MG_FAC/ACORSEM_RENORM
           CINORG( KK,IMODE )  = CINORG( KK,IMODE ) + 
     &       REAL( AEROSPC_CONC( ACORS_IDX,IMODE ),8 ) * ACORS_K_FAC /ACORSEM_RENORM
           CINORG( KCA,IMODE ) = CINORG( KCA,IMODE ) + 
     &       REAL( AEROSPC_CONC( ACORS_IDX,IMODE ),8 ) * ACORS_CA_FAC/ACORSEM_RENORM
        END IF

        ! Load H+ Concentration
        CINORG( KH3OP,IMODE ) = REAL( HPLUS( IMODE ),8 )

      END DO
      CINORG0 = CINORG

      ! Initial vapor-phase concentrations [ug/m3]
      DO I = 1,NGAS
         CGAS( I ) = PRECURSOR_CONC( MAP_IGAStoPREC( I ) )
      END DO
      CGAS0 = CGAS

C *** DYNAMIC AEROSOL MODES:
C     Begin mass transfer calculations for aerosol modes using
C     a dynamic approach that solves for vapor pressures and
C     integrates fluxes across time.
      
      DO IMODE = 1,N_MODE
        IF ( .NOT. L_MODE_EQB( IMODE ) ) THEN
  
          ! For Hybrid approach, calculate dynamic mass trasfer for
          ! semi-volatile components of coarse mode (NO3, CL, NH4)  
          IF ( AEROMODE(IMODE)%COARSE_MASK .AND. .NOT. DO_COARSE_MT ) CYCLE

          CNTRL( 1 ) = 1.0D0 ! reverse problem (WI contains only aerosol)
          CNTRL( 2 ) = 1.0D0 ! aerosol in metastable state

          INT_TIME = 0.0D0
          TSTEP    = AERO_DYN_TSTEP
          ISTEP    = 1
          TrustIso = .TRUE.
 
          ! Store Contribution of Non-Volaitle Species to 3rd Moment


          ! Begin sub-time step iteration for dynamic mass transfer
          ! calculations
          DO WHILE ( INT_TIME .LT. DELT ) 

            IF ( INT_TIME + TSTEP .GT. DELT ) TSTEP = DELT - INT_TIME 
            INT_TIME = INT_TIME + TSTEP 
            ISTEP = ISTEP + 1   

            ! Calculate first moments using Equation 4 of Binkowski & Shankar
            ! (1995) or Equation 3 of Binkowski and Roselle (2003).
            ! N.B: these are for a "wet" size distribution
            AM0( IMODE ) = MOMENT0_CONC( IMODE )
            AM1( IMODE ) = MOMENT0_CONC( IMODE ) * AEROMODE_DIAM( IMODE )
     &                  * EXP( 0.5 * AEROMODE_LNSG( IMODE ) * AEROMODE_LNSG( IMODE ) )
            AM2( IMODE ) = MOMENT2_CONC( IMODE )

            ! Calculate the size-dependent terms in the condensational-
            ! growth factor expressions for sulfate using 
            ! Equations A13-A14 of Binkowski & Shankar (1995). 
            CALL HCOND3( AM0( IMODE ), AM1( IMODE ), AM2( IMODE ), DV_SO4, ALPHSULF, 
     &                      CBAR_SO4, FCONC_OUT )  ! adapted from Eq A14
            FCONC_SO4( IMODE, : ) = FCONC_OUT( : )

            GRFAC1( IMODE ) = FCONC_SO4( IMODE,1 ) 
            GRFAC2( IMODE ) = FCONC_SO4( IMODE,2 ) 

            ! Calculate moments prior to mass transfer
            M3OTHR = MASStoVOL_AER( KNA ) * AEROSPC_CONC( ASEACAT_IDX,IMODE )
     &             + MASStoVOL_SOIL * AEROSPC_CONC( ASOIL_IDX,IMODE )
     &             + MASStoVOL_CORS * AEROSPC_CONC( ACORS_IDX,IMODE )
            WETM3  = MOMENT3_CONC( IMODE )
            WETM2  = MOMENT2_CONC( IMODE )
            DRYM3  = WETM3 - REAL( H2OFAC * AEROSPC_CONC( AH2O_IDX, IMODE ), 8 )  ! Assume no SOA in coarse mode
            DRYM20 = WETM2 * ( DRYM3 / WETM3 ) ** D_TWOTHIRDS

            ! Compute sulfate production rate [ug/m3 s] for coarse mode
            RATE = CONDSO4( IMODE )
            CFINAL( KSO4,IMODE )  = CINORG( KSO4,IMODE ) + RATE * TSTEP * MW_RATIO_INV(KH2SO4)

            ! Double Precision vars for ISORROPIA [mole/m3]
            ! Compute equilibrium vapor pressures [mol/m3] of NH3, HNO3, and HCL
            ! at the gas/particle interface of coarse mode aerosol.
            !       GAS(1) = NH3, GAS(2) = HNO3, GAS(3) = HCl
            ! Also obtain the aqueous H+ concentration, ISRP_AERLIQ(1) [mol/m3]
            IF ( TrustIso ) THEN
               ! Load Concentrations into ISORROPIA input vector
               DO K = 1,NAER
                 I = ATTR( K )%ISRP
                 IF ( I .GT. 0 .AND. I .LE. NISRP )
     &              WI( I ) = CINORG( K,IMODE ) * CFAC_AER( K )
               END DO
               WI( ATTR( KSO4 )%ISRP ) = CFINAL( KSO4,IMODE ) * CFAC_AER( KSO4 )
               
               ! Call ISORROPIA Module
               CALL ISOROPIA( WI, RHI, TEMPI, CNTRL, WT, ISRP_GAS, ISRP_AERLIQ,  
     &                        ISRP_AERSLD, SCASI, OTHER )

               IF ( ANY( ISRP_GAS( : ) .LT. 0.0D0 ) ) THEN
                  IF ( FIRSTWRITE ) THEN
                     FIRSTWRITE = .FALSE.
                     WRITE( LOGDEV,2023 )
                  END IF
                  WRITE( LOGDEV,2029 ) COL, ROW, LAYER, ISRP_GAS(:)
                  TrustIso = .FALSE.
               END IF
            END IF   ! TrustIso

            ! Change in volatile inorganic PM concentration to achieve
            ! equilibrium, calculated as initial-gas-phase concentration minus 
            ! equilibrium gas-phase concentration.  DVOLINORG is positive for
            ! condensation and negative for evaporation.
          
            ! Calculate condensation/evaporation flux for this time step and update 
            ! volatile species concentrations.  Final aerosol conc set to be no less
            ! than the minimum aerosol conc.
            J( : ) = 0.0D0
            IF ( TrustIso ) 
     &         CALL COMPUTE_FLUX( CGAS(:), ISRP_GAS(:), GRFAC2( IMODE ),
     &                            ISRP_AERLIQ(1), RATE, J )

            ! Flux Integration for Dynamic Mass Transfer
            DO I = 1,NGAS
               IF ( I .NE. KH2SO4 ) THEN         
                  K = GTTR(I)%KAER

                  ! Protect time integration against overshoot
                  IF ( J( I ) * TSTEP * CFAC_AER_INV(K) * MW_RATIO(I) .GT. CGAS(I) ) THEN
                     WRITE( LOGDEV,* ) 'Condensed amt. exceeds ' // 
     &                                 TRIM(GTTR(I)%NAME) //' conc: aero_subs.f'
                     J( I ) = CGAS(I) / ( TSTEP * CFAC_AER_INV(K) * MW_RATIO(I) )
                  END IF

                  ! Integrate mass transfer equation, convert flux from molar to mass
                  CFINAL( K,IMODE ) = MAX( 0.0D0, CINORG( K,IMODE )
     &                                  + J(I) * TSTEP * CFAC_AER_INV(K) )
               END IF
            END DO
            
            ! For H+, just pass initial value to final
            CFINAL( KH3OP,IMODE ) = CINORG( KH3OP,IMODE )

            ! Pass nonvolatile species to final concentration vector
            CFINAL( KNA,IMODE ) = CINORG( KNA,IMODE )
            CFINAL( KMG,IMODE ) = CINORG( KMG,IMODE )
            CFINAL( KK,IMODE )  = CINORG( KK, IMODE )
            CFINAL( KCA,IMODE ) = CINORG( KCA,IMODE )

            ! Calculate updated H+ concentration 
            HPLUS( IMODE ) = 0.0
            DO I = 1,NAER
               IF ( I .EQ. KNA ) THEN
                  HPLUS( IMODE ) = HPLUS( IMODE ) 
     &               - AEROSPC( ASEACAT_IDX )%CHARGE * CFINAL( KNA,IMODE ) / AEROSPC_MW( ASEACAT_IDX )
               ELSE IF ( I .NE. KH2O .AND. I .NE. KH3OP ) THEN
                  HPLUS( IMODE ) = HPLUS( IMODE ) - 
     &                             AER_CHARGE( I ) * CFINAL( I, IMODE ) / MW_AER( I )
               END IF
            END DO

            ! Equilibrate aerosol LWC with CFINAL by calling CALC_H2O
            WI = 0.0
            DO I = 1,NAER
              ISRP = ATTR(I)%ISRP
              IF ( I .EQ. KNA ) THEN
                 WI( ISRP ) = WI( ISRP ) + CFINAL( I,IMODE ) * CFAC_ASEACAT
              ELSE IF ( ISRP .GT. 0 .AND. ISRP .LE. NISRP ) THEN
                 WI( ISRP ) = WI( ISRP ) + CFINAL( I,IMODE ) * CFAC_AER( I )
              END IF
            END DO

            CALL CALC_H2O( WI, RHI, TEMPI, H2O_NEW ) 

            CFINAL( KH2O,IMODE ) = H2O_NEW * CFAC_AER_INV( KH2O )

            ! Update all Local Vapor Concentrations 
            DO I = 1,NGAS
              IF ( GTTR(I)%ISRP .GT. 0 ) THEN
                K = GTTR( I )%KAER
                CGAS( I ) = CGAS( I ) + MW_RATIO( I ) *
     &                      ( CINORG( K,IMODE ) - CFINAL( K,IMODE ) )
              END IF
            END DO
 
            ! Compute net change in 3rd moment due to dry inorganic mass transfer
            DDRYM3DT = 0.0
            DO I = 1,NGAS
               K = GTTR(I)%KAER
               DDRYM3DT = DDRYM3DT + MASStoVOL_AER( K ) *
     &                    ( CFINAL( K,IMODE ) - CINORG( K,IMODE ) )
            END DO
            DDRYM3DT = DDRYM3DT / TSTEP

            ! Compute net change in 2nd moment due to dry inorganic mass transfer
            ! (including nucleation) using equation A7 of Binkowski & Shankar (1995)
            DDRYM2DT = D_TWOTHIRDS * GRFAC1( IMODE ) / GRFAC2( IMODE ) * DDRYM3DT   

            ! Update dry 2nd moment for condensation/evaporation based on whether
            ! net change in dry 2nd moment is production or loss
            IF ( DDRYM2DT .LT. 0.0D0 ) THEN
               LOSS = DDRYM2DT / DRYM20
               Y = DRYM20 * EXP( LOSS * TSTEP )
            ELSE
               Y = DRYM20 + DDRYM2DT * TSTEP
            END IF

            ! Add water (no SOA) 2nd moment while preserving standard deviation
            ! Calculate 3rd Moment
            DRYM3 = 0.0
            DO I = 1,NGAS
               K = GTTR(I)%KAER
               DRYM3 = DRYM3 + CFINAL( K,IMODE ) * MASStoVOL_AER( K )
            END DO
            DRYM3 = DRYM3 + M3OTHR

            WETM3 = DRYM3 + CFINAL( KH2O,IMODE ) * MASStoVOL_AER(KH2O)

            !Calculate 2nd moment
            DRYM2 = MAX( REAL( AEROMODE_MINM2( IMODE ), 8 ), Y )
            WETM2 = DRYM2 * ( WETM3 / DRYM3 ) ** D_TWOTHIRDS

            MOMENT2_CONC( IMODE ) = REAL( WETM2,4 )

            ! Update the third moments, geometric mean diameters, geometric 
            ! standard deviations, modal mass totals, and modal particle 
            ! densities. It is a waste of time updating the aitken and
            ! accumulation modes but the coarse mode does need to be updated
            ! each sub-time step. This should be for the wet distribution
            
            CALL GETPAR( .TRUE. )

            ! Pass Final Aerosol Concentrations to Next Time Step
            CINORG( :,IMODE ) = CFINAL( :,IMODE )

          END DO   ! end mass transfer time step loop
         
        END IF   ! for 'Hybrid' method
      END DO   ! Loop over modes
C *** End of Coarse Mode dynamic mass transfer calculations



C *** EQUILIBRIUM AEROSOL MODES: 
C     Call ISORROPIA in forward mode to calculate gas-particle 
C     equilibrium. These are often fine modes.
      IF ( ANY( L_MODE_EQB(:) ) ) THEN

      ! Sum all total gas+particle concentrations to passed to
      ! ISORROPIA. Convert everything to [mol m-3].
      WI(:) = 0.0
      DO IAER = 1,NAER
         I = ATTR( IAER )%ISRP
         K = MAP_INORGtoAERO( IAER )

         IF ( IAER .EQ. KSO4 ) THEN
            ! ADD NEW PARTICLE FORMATION MASS
            WI(I) = ( SUM( AEROSPC_CONC(K,:),MASK=L_MODE_EQB(:) ) 
     &                + ( DMDT_SO4 + SUM( CONDSO4(:), MASK=L_MODE_EQB(:) ) ) 
     &                    * DELT * MW_RATIO_INV(KH2SO4) ) * CFAC_AER(IAER)
         ELSE IF ( I .GT. 0 .AND. I .LE. NISRP ) THEN
            ! ADD PARTICLE PHASE MASS FROM BOTH MODES
            WI(I) = SUM( AEROSPC_CONC(K,:),MASK=L_MODE_EQB(:) ) * CFAC_AER( IAER )
         END IF
      END DO

      ! Add Condensable Gas to ISORROPIA input vector, WI
      DO K = 1,NGAS
         IF ( K .NE. KH2SO4 ) THEN
            I = ATTR( GTTR( K )%KAER )%ISRP
            IF ( I .GT. 0 .AND. I .LE. NISRP ) THEN
               ! ADD CONDENSABLE GAS
               WI(I) = WI(I) + CGAS( K )  * CFAC_GAS( K )
            END IF
         END IF
      END DO

      CNTRL( 1 ) = 0.0D0   ! ISORROPIA Forward Problem
      CNTRL( 2 ) = 1.0D0   ! Aerosol in Metastable State

      ! Set flags to account for mass conservation violations in ISRP3F
      TRUSTCL  = .TRUE.
      IF ( (WI( ATTR(KNA)%ISRP ) + WI( ATTR(KCL)%ISRP )) .LT. 1.0D-20 .or.
     &      WI( ATTR(KCL)%ISRP ) .LT. 1.0D-10 ) THEN
         TRUSTCL = .FALSE.
      END IF
         
#ifndef noiso
      CALL ISOROPIA( WI, RHI, TEMPI, CNTRL, WT, ISRP_GAS, ISRP_AERLIQ,
     &               ISRP_AERSLD, SCASI, OTHER )

#ifdef sens
      CALL AERO_SENS( SCASI, WI )
#endif

      ! Save H+ concentration information in microgram/m3 for consistency
      EQLBHIJ = ISRP_AERLIQ(1) * 1.0D6 * MW_AER( KH3OP )

#else

      DO K = 1,NGAS
        IF ( K .NE. KH2SO4 ) THEN
          I = GTTR( K )%ISRP
          ISRP_GAS( I ) = real( PRECURSOR_CONC( MAP_IGAStoPREC( K ) ),  8 ) * CFAC_GAS( K )
        END IF
      END DO
      EQLBHIJ =  HPLUS( 1 ) + HPLUS( 2 )  ! use charge balance if lacking isoropia info 

#endif

      ! Change in volatile inorganic PM concentration to achieve
      ! equilibrium, calculated as initial-gas-phase concentration minus 
      ! equilibrium gas-phase concentration.  DVOLINORG is positive for
      ! condensation and negative for evaporation.
      DVOLINORG = 0.0
      DO K = 1,NGAS
         IF ( K .NE. KH2SO4 ) THEN
            I = GTTR( K )%ISRP
            DVOLINORG( K ) = CGAS( K ) * CFAC_GAS( K )  - ISRP_GAS( I )   ! mol m-3

            IF ( DVOLINORG( K ) .LT. 0.0D0 ) THEN
              KAER = GTTR( K )%KAER
              IAER = MAP_INORGtoAERO( KAER )
              DVOLMAX = EVAPMIND
              DO IMODE = 1,N_MODE
                 IF ( L_MODE_EQB( IMODE ) ) 
     &              DVOLMAX = DVOLMAX - REAL( AEROSPC_CONC( IAER,IMODE ), 8 ) * CFAC_AER(KAER)
              END DO
              DVOLINORG( K ) = MAX( DVOLINORG( K ), DVOLMAX )
            END IF
         END IF
      END DO

      IF ( .not.TRUSTCL ) DVOLINORG( KHCL ) = 0.0D0


      ! Apply modal partitioning of equilibrium aerosol mass
      ! Calculate Distribution of Mass Transfer Among Modes
      GRFAC2TOT = SUM( GRFAC2(:), MASK=L_MODE_EQB(:) )
      DO IMODE = 1,N_MODE
         IF ( L_MODE_EQB( IMODE ) ) THEN
            OMEGA( IMODE ) = GRFAC2( IMODE ) / GRFAC2TOT
         END IF
      END DO

      ! Save Initial Concentrations
      DO IMODE = 1, N_MODE 
         IF ( L_MODE_EQB( IMODE ) ) THEN
            DO K = 1,NAER
               IF ( K .NE. KH2O .AND. K .NE. KH3OP ) THEN
                  CINORG( K,IMODE ) = AEROSPC_CONC( MAP_INORGtoAERO( K ), IMODE )
               END IF
            END DO
         END IF
      END DO

      ! Calculate Initial Distribution of Mass Composition Among Modes
      DO ISP = 1, NGAS
         IF ( ISP .NE. KH2SO4 ) THEN
            K = GTTR( ISP )%KAER
            TOTAER( K ) = MAX( SUM( CINORG( K,: ), MASK=L_MODE_EQB(:) ), CONMIND )
            DO IMODE = 1, N_MODE
               IF ( L_MODE_EQB( IMODE ) ) 
     &            PHI( K, IMODE ) = CINORG( K,IMODE ) / TOTAER( K )
            ENDDO
         END IF
      ENDDO

      ! Initialize Final Concentrations for Equilibrium Modes
      DO IMODE = 1,N_MODE
        IF ( L_MODE_EQB( IMODE ) ) CFINAL( :,IMODE ) = 0.0
      END DO

      ! Calculate Final Concentrations
      DO ISP = 1, NGAS
         IF ( ISP .NE. KH2SO4 .AND. DVOLINORG( ISP ) .LT. 0.0 ) THEN
            ! Evaporate Mass Using Condensed-Phase Fraction in each Mode
            DO IMODE = 1,N_MODE
              IF ( L_MODE_EQB( IMODE ) ) THEN
                CFINAL( ISP,IMODE ) = CFINAL( ISP,IMODE ) + CINORG( ISP,IMODE )
     &                        + PHI( GTTR(ISP)%KAER,IMODE ) 
     &                          * DVOLINORG( ISP ) * CFAC_AER_INV(ISP)
              END IF
            END DO
         ELSE
            ! Condense Mass Using Condensation Sink Factors
            DO IMODE = 1,N_MODE
              IF ( L_MODE_EQB( IMODE ) ) THEN
                CFINAL( ISP,IMODE ) = CFINAL( ISP,IMODE ) + CINORG( ISP,IMODE )
     &                          + OMEGA( IMODE ) 
     &                            * DVOLINORG( ISP ) * CFAC_AER_INV(ISP)
              END IF
            END DO
         END IF
      END DO
      
      END IF  ! Are there any Equilibrium Modes?

C *** End Equilibrium Mode Calculations     
     

C *** Apply Final Concentrations to Moment Variables in Aerosol Scheme
      ! Store Initial Wet Moments
      M3WET_INIT( : ) = MOMENT3_CONC( : )
      M2WET_INIT( : ) = MOMENT2_CONC( : )

      ! Calculate and Store Initial Dry Moments and set wet_moments_flag to false/dry
      ! There has not yet been an update to AEROSPC_CONC so this will
      ! calculate the old 3rd moment.
      call calcmoments( .false. )
      M3DRY_INIT( : ) = MOMENT3_CONC( : )
      M2DRY_INIT( : ) = MOMENT2_CONC( : )

      DO IMODE = 1, N_MODE  ! modal partitioning of equilibrium aerosol mass
         IF ( L_MODE_EQB( IMODE ) ) THEN 

         IF ( IMODE .EQ. NPF_MODE ) THEN
            ! Update Number Concentration with NPF
            MOMENT0_CONC( IMODE ) = REAL( AM0(IMODE) + DNDT * DELT, 4 )
            ! Add NPF to total condensation rate
            RATE = DMDT_SO4 + CONDSO4( IMODE )
            CFINAL( KSO4,IMODE ) = CINORG( KSO4,IMODE ) + RATE * DELT * MW_RATIO_INV(KH2SO4) 
         ELSE
            ! Ignore NPF. The small particles are not in these modes
            CFINAL( KSO4,IMODE ) = CINORG( KSO4,IMODE ) + CONDSO4( IMODE ) * DELT * MW_RATIO_INV(KH2SO4)
         END IF

         CFINAL( KNA,IMODE ) = CINORG( KNA,IMODE )
         CFINAL( KMG,IMODE ) = CINORG( KMG,IMODE )
         CFINAL( KK,IMODE )  = CINORG( KK, IMODE )
         CFINAL( KCA,IMODE ) = CINORG( KCA,IMODE )

         ! Double precision vars for CALC_H2O
         DO I = 1,NAER
           ISRP = ATTR(I)%ISRP
           IF ( ISRP .GT. 0 .AND. ISRP .LE. NISRP ) 
     &          WI( ISRP ) = CFINAL( MAP_ISRP(ISRP),IMODE ) * CFAC_AER( MAP_ISRP(ISRP) )
         END DO
         
         CALL CALC_H2O( WI, RHI, TEMPI, H2O_NEW ) 
         CFINAL( KH2O,IMODE ) = H2O_NEW * CFAC_AER_INV( KH2O )
         
         ! Update All Aerosol Concentrations
         DO I = 1,NAER
           IF ( ATTR(I)%SEMIVOL ) THEN
             AEROSPC_CONC( MAP_INORGtoAERO(I),IMODE ) = 
     &                     REAL( CFINAL( I,IMODE ),4 )
           END IF  
         END DO

         ! Compute net change in 3rd moment due to dry inorganic mass transfer
         ! (includes nucleated sulfate mass). This is for projecting the
         ! change to the second moment due to dry inorganic condensation
         
         DDRYM3DT = 0.0
         DO I = 1,NGAS
            K = GTTR(I)%KAER
            DDRYM3DT = DDRYM3DT + 
     &                 ( CFINAL( K,IMODE ) - CINORG( K,IMODE ) )
     &                 * MASStoVOL_AER( K )
         END DO
         DDRYM3DT = DDRYM3DT / DELT

         ! Compute net change in 2nd moment due to dry inorganic mass transfer
         ! (including nucleation) using equation A7 of Binkowski & Shankar (1995)
         DDRYM2DT = D_TWOTHIRDS * GRFAC1( IMODE ) / GRFAC2( IMODE ) * DDRYM3DT

         ! Update dry 2nd moment for condensation/evaporation based on whether
         ! net change in dry 2nd moment is production or loss
         IF ( DDRYM2DT .LT. 0.0D0 ) THEN
            LOSS = DDRYM2DT / M2DRY_INIT( IMODE )
            Y = M2DRY_INIT( IMODE ) * EXP( LOSS * DELT )
         ELSE
            Y = M2DRY_INIT( IMODE ) + DDRYM2DT * DELT
         END IF
         moment2_conc( IMODE ) = REAL( MAX( REAL( AEROMODE_MINM2( IMODE ), 8 ), Y ),4 )

         END IF  ! Is this an equilibrium mode?
      END DO

      ! Add water and SOA to 2nd moment while preserving standard deviation
      call calcmoments( .true. )
      M3WET_FINAL = moment3_conc
      M2WET_FINAL = moment2_conc

      ! Assign H+ Concentration to each Mode based on Equilibrium or
      ! Dynamic Approach
      ! First calculate equilibrium H+ Concentration and Water
      ! concentration
      DO IMODE = 1,N_MODE
        IF ( L_MODE_EQB(IMODE) ) THEN
           HPLUS( IMODE ) = 0.0
           DO I = 1, N_AEROSPC
             IF(  AEROSPC( I )%tracer )CYCLE
             HPLUS( IMODE ) = HPLUS( IMODE )
     &            - AEROSPC( I )%CHARGE * AEROSPC_CONC( I,IMODE ) / AEROSPC_MW( I )
           END DO
        END IF
      END DO
      H2O_NEW = REAL( SUM( AEROSPC_CONC( MAP_INORGtoAERO( KH2O ),: ), 
     &                     MASK=L_MODE_EQB(:) ), 8 )
      IF (  H2O_NEW .GT. CONMIN ) H2O_NEW_INV = 1.0 / H2O_NEW

      ! Assign H3O+ concentrations for equilibrium and dynamic modes
      DO IMODE = 1,N_MODE
         IF ( L_MODE_EQB(IMODE) ) THEN
            IF (  H2O_NEW .GT. CONMIN )THEN
               AEROSPC_CONC( MAP_INORGtoAERO( KH3OP ),IMODE ) = REAL( EQLBHIJ * H2O_NEW_INV, 4 ) * 
     &                     AEROSPC_CONC( MAP_INORGtoAERO( KH2O ),IMODE ) 
            ELSE
               AEROSPC_CONC( MAP_INORGtoAERO( KH3OP ),IMODE ) = CONMIN
            END IF
         ELSE
            AEROSPC_CONC( MAP_INORGtoAERO( KH3OP ),IMODE ) = 
     &         REAL( HPLUS( IMODE ),4 ) * MW_AER( KH3OP ) ! Coarse mode H+ concentration in ug/m3

         END IF
      END DO


C *** Update the third moments, geometric mean diameters, geometric 
C     standard deviations, modal mass totals, and modal particle 
C     densities. Note that moment2_conc needs to be up to date when this
C     routine is called. Moment3_conc does not need to be up to date
C     because it will be recalculated inside GETPAR as the sum of 
C     aerospc_conc variables. This should be for the wet distribution.
      CALL GETPAR( .TRUE. )
       
! *** Propagate Concentration Changes from Coarse-Mode Mass
! *** Transfer to Budget Vectors
      DELNUM( : )  = MOMENT0_CONC( : ) - CBUDGET0_NUM( : )
      DELSRF( : )  = MOMENT2_CONC( : ) * PI - CBUDGET0_SRF( : )

      COND_BUDGET( AERONUM_MAP( : ) ) = REAL( DELNUM( : ), 4 )
      COND_BUDGET( AEROSRF_MAP( : ) ) = REAL( DELSRF( : ), 4 )
      
      ! Update Budget Aerosol Due to Condensation
      DO I = 1,NAER
        IF ( ATTR(I)%BUDGET .GT. 0 ) THEN 
          IAER = MAP_INORGtoAERO( I )
          IB   = ATTR(I)%BUDGET
          DAER( I,: ) = AEROSPC_CONC( IAER,: ) - CBUDGET0_MASS( IB,: )

          COND_BUDGET( AEROSPC_MAP( MAP_INORGtoAERO(I),: )) = REAL( DAER( I,: ),4 )
        END IF
      END DO

      ! Update Budget Gas Due to Condensation
      DO I = 1,NGAS
        IF ( MAP_IGAStoPREC( I ) .GT. 0 ) THEN
          COND_BUDGET( PRECURSOR_MAP( MAP_IGAStoPREC(I) )) = 
     &                 SUM( REAL( -DAER( GTTR(I)%KAER,: ) * MW_RATIO(I), 4 ))
        END IF
      END DO

C *** Update gas-phase concentrations from Aitken and Accumulation Mode Partitioning
      DO I = 1,NGAS
         IF ( I .EQ. KH2SO4 ) THEN
            PRECURSOR_CONC( MAP_IGAStoPREC( I ) ) = PRECURSOR_CONC( MAP_IGAStoPREC( I ) )
     &          + SUM( -DAER( GTTR(I)%KAER,:) * MW_RATIO( I ) )
         ELSE
            K = GTTR(I)%KAER
            PRECURSOR_CONC( MAP_IGAStoPREC( I ) ) = CGAS0(I) 
     &          + SUM( CINORG0( K,: ) - CFINAL( K,: ),MASK=L_MODE_EQB(:) ) * MW_RATIO( I )
         END IF
      END DO

2023  FORMAT( 1X, 'VOLINORG returning negative gas concentrations from ISOROPIA:'
     &       /10X, 'GAS(1) = NH3, GAS(2) = HNO3, GAS(3) = HCl' )
2029  FORMAT( 1X, '[see VOLINORG msg]'
     &        1X, 'at (C,R,L): ', 3I4, 1X, 'GAS Conc:', 3( 1PE11.3 ) )

      RETURN
      END SUBROUTINE VOLINORG
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE HCOND3( AM0, AM1, AM2, DV, ALPHA, CBAR, F )

C  calculates the size-dependent term in the condensational-growth rate
C  expression for the 2nd and 3rd moments of a lognormal aerosol mode
C  using the harmonic mean method.  This code follows Section A2 of
C  Binkowski & Shankar (1995).
 
C  Key Subroutines/Functions called:  none
 
C  Revision History:
C     coded November 7, 2003 by Dr. Francis S. Binkowski
C     Revised November 20, 2003 by F. Binkowski to have am1 and
C     am2 as inputs
 
C  Reference:
C   1. Binkowski, F.S. and U. Shankar, The regional particulate matter
C      model 1. Model description and preliminary results, J. Geophys.
C      Res., Vol 100, No D12, 26101-26209, 1995.

      USE CONST
      IMPLICIT NONE

C *** Arguments:

      REAL( 8 ), INTENT( IN ) :: AM0   ! zeroth moment of mode  [ #/m**3 ]
      REAL( 8 ), INTENT( IN ) :: AM1   ! first moment of mode   [ m/m**3 ]
      REAL( 8 ), INTENT( IN ) :: AM2   ! second moment of mode  [ m**2/m**3 ]
      REAL,      INTENT( IN ) :: Dv    ! molecular diffusivity of the
                                       ! condensing vapor  [ m**2/s ]
      REAL,      INTENT( IN ) :: ALPHA ! accommodation coefficient
      REAL,      INTENT( IN ) :: CBAR  ! kinetic velocity of condensing vapor [ m/s ]

      REAL( 8 ), INTENT( OUT ) :: F( 2 ) ! size-dependent term in condensational-growth
                                         ! rate: F(1) = 2nd moment [ m**2/m**3 s ]
                                         !       F(2) = 3rd moment [ m**3/m**3 s ]

C *** Local Variables:

      REAL( 8 ) :: GNC2 ! integrals used to calculate F(1) [m^2 / m^3 s]
      REAL( 8 ) :: GFM2 !

      REAL( 8 ) :: GNC3 ! integrals used to calculate F(2) [m^3 / m^3 s]
      REAL( 8 ) :: GFM3 !

      REAL( 8 ), PARAMETER :: TWOPI = 2.0D0 * PI
      REAL( 8 ), PARAMETER :: PI4 = 0.25D0 * PI

C-----------------------------------------------------------------------

C *** Implement equation A15 of Binkowski & Shankar (1995) for the
C     2nd and 3rd moments of a lognormal mode of arbitrary size.

      GNC2 = TWOPI * DV * AM0          ! 2nd moment, near-continuum
      GNC3 = TWOPI * DV * AM1          ! 3rd moment, near-continuum
      GFM2 = PI4 * ALPHA * CBAR * AM1  ! 2nd moment, free-molecular
      GFM3 = PI4 * ALPHA * CBAR * AM2  ! 3rd moment, free-molecular

C *** Implement equation A13 of Binkowski & Shankar (1995) for a
C     lognormal mode of arbitrary size.  These are the size-dependent
C     terms in the condensational-growth rate expression, given in
C     equation 7a of B&S (1995).

      F( 1 ) = GNC2 * GFM2 / ( GNC2 + GFM2 )  ! 2nd moment
      F( 2 ) = GNC3 * GFM3 / ( GNC3 + GFM3 )  ! 3rd moment

      RETURN
      END SUBROUTINE HCOND3

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE Compute_Flux ( CGAS0, CVAP, CondRate, Hplus, rate, J )
 
C Description
C   Determines the evaporative/condensational flux of volatile
C   inorganic species to aerosol modes. In cases where the resulting H+
C   flux is greater than a specified limit, the Pilinis et al. (2000)
C   AS&T approach is used to modify species vapor pressures such that
C   cond./evap. produces an H+ flux equal to the limit (which is
C   proportional to the current mode concentration of H+).
C   Routine called by VOLINORG.
 
C Arguments
C   Inputs
C     CGAS0    : gas concentration (ug/m3)
C     Ceq      : vapor concentration (mol/m3) from ISORROPIA
C     CondRate : effective condensation rate (I) of 3rd moment to mode
C              : [treat units as (1/s)]
C     Hplus    : aerosol hydrogen ion concentration (mol/m3) for mode
C     rate     : H2SO4(g) condensation rate (ug/m3/s) for mode
 
C   Output
C     Ceq      : modified vapor concentration (mol/m3)
C     J        : molar cond./evap. flux of volatile inorganics (mol/m3-s)
 
C-----------------------------------------------------------------------

      USE AERO_DATA
      USE PRECURSOR_DATA
!     USE AEROMET_DATA

      IMPLICIT NONE

C     Arguments:
      REAL( 8 ) :: CGAS0( NGAS )         ! Input gas concentrations [ug/m3]
      REAL( 8 ) :: CEQ( NGAS )
      REAL( 8 ) :: CVAP( 3 )             ! vapor concentrations [mol/m3] from ISORROPIA
      REAL( 8 ) :: CondRate              ! effective condensation rate (I) for 3rd moment
      REAL( 8 ) :: Hplus                 ! hydrogen ion concentration for mode [mol/m3]
      REAL( 8 ) :: rate
      REAL( 8 ) :: J( NGAS )             ! molar cond./evap. flux [mol/m3-s]

C     Local Variables:
      REAL( 8 ),  PARAMETER :: Afact = 1.0D-01  ! factor for H+ limiter
      REAL( 8 ),  PARAMETER :: small = 1.0D-25
      REAL( 8 ) :: Cinf( NGAS )    ! gas concentration in mol/m3
      REAL( 8 ) :: Qk              ! factor for modifying vapor press. based on H+ limit
      REAL( 8 ) :: Hflux           ! flux of H+ to mode from cond/evap
      REAL( 8 ) :: Hlim            ! maximum allowable H+ flux to mode
      REAL( 8 ) :: aa, bb, cc      ! terms in quadratic equation
      INTEGER      isp             ! inorganic species index

C-----------------------------------------------------------------------

      DO ISP = 1,NGAS
         ! Convert gas concentration from ug/m3 to mol/m3
         CINF( ISP ) = CGAS0( ISP  ) * 1.0D-6 / MW_GAS( ISP )

         ! Calculate cond/evap fluxes (no H+ limiting)
         IF ( ISP .EQ. KH2SO4 ) THEN
            J( ISP ) = rate * 1.0D-6 / MW_GAS( ISP )
         ELSE
            Ceq( ISP ) = Cvap( GTTR(ISP)%ISRP )
            J( ISP ) = CondRate * ( Cinf( ISP ) - Ceq( ISP ) )
         END IF
      END DO

C     Convert rate to mol/m3/s and get effective Cinf for H2SO4(g)
      CINF( KH2SO4 )  = J( KH2SO4 ) / CondRate

C     Limit H+ flux (Pilinis et al., 2000, AS&T). Note: J is flux
C     to entire mode, not one particle
      Hlim  = Afact * Hplus
      Hflux = 2.0D0 * J( KH2SO4 ) + J( KHNO3 ) + J( KHCL ) - J( KNH3 )

C     If Hflux is too large, limit the flux by modifying species
C     vapor pressures with Qk factor (Pilinis et al., 2000, AS&T).
      IF ( ABS( Hflux ) .GT. Hlim ) THEN
         Hlim = SIGN( Hlim, Hflux )

C        Solve quadratic for Qk: aa*Qk^2 + bb*Qk + cc = 0
         aa = Ceq( KHCL ) + Ceq( KHNO3 )

         bb = Hlim / CondRate
     &      + Cinf( KNH3 ) - Cinf( KHNO3 ) - Cinf( KHCL ) - 2.0D0 * Cinf( KH2SO4 )
         cc = -Ceq( KNH3 )

         Qk = 0.0D0 ! initialize Qk

         IF ( aa .LT. small .AND. 0.0D0 .LT. bb ) THEN ! bb*Qk + cc = 0
            Qk = -cc / bb
         ELSE IF (aa .LT. small .AND. bb .LE. 0.0D0 ) THEN
            Qk = 0.0D0
         ELSE IF (-cc .LT. small .AND. bb .LT. 0.0D0 ) THEN  ! aa*Qk^2 + bb*Qk = 0
            Qk = -bb / aa
         ELSE IF (-cc .LT. small .AND. 0.0D0 .LE. bb ) THEN
            Qk = 0.0D0
         ELSE
            Qk = ( -bb + SQRT ( bb**2 - 4.0D0 * aa * cc ) ) / ( 2.0D0 * aa )
            IF ( bb ** 2 - 4.0D0 * aa * cc .LT. 0.0D0 ) THEN
               PRINT *, 'Compute_Flux, sqrt<0'
               Qk = 0.0D0
            END IF
         END IF

C     Modify vapor pressures and get new fluxes
         IF ( Qk .GT. small ) THEN
            Ceq( KH2SO4 )= 0.0
            Ceq( KNH3 )  = Ceq( KNH3 ) / Qk
            Ceq( KHNO3 ) = Ceq( KHNO3 ) * Qk
            Ceq( KHCl )  = Ceq( KHCl )  * Qk
            DO ISP = 1,NGAS
               J( ISP ) = CondRate * ( Cinf( ISP ) - Ceq( ISP ) )
            END DO
         END IF

      END IF   ! |Hflux| > Hlim

      END SUBROUTINE Compute_Flux

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CALC_H2O ( WI, RH, T, H2O_NEW )

C Description
C   Calculate the water content of aerosol at the new time step.  Water
C   calculations use the ZSR mixing rule with salts determined by the
C   ISORROPIA approach.
C   Routine called by VOLINORG.
 
C Arguments
C   Input
C     WI      : Concentration of components [mol/m^3] at new step
C     RH      : Relative humidity [0-1]
C     T       : Temperature [K]
 
C   Output
C     H2O_NEW : Water [mol/m^3] content at new time step
 
C-----------------------------------------------------------------------

      IMPLICIT NONE

C Parameters:
      INTEGER, PARAMETER :: NCMP = 8, NPAIR = 23
      REAL( 8 ),  PARAMETER :: SMALL = 1.0D-20
      REAL( 8 ),  PARAMETER :: Mw = 0.018D0   ! molar mass H2O (kg/mol)

C Arguments:
      REAL( 8 ), INTENT( IN )  :: WI( NCMP )
      REAL( 8 ), INTENT( IN )  :: RH, T
      REAL( 8 ), INTENT( OUT ) :: H2O_NEW

C Local Variables:
      CHARACTER( 3 ) :: SC ! subcase for composition
      REAL( 8 ) :: FSO4, FNH4, FNA, FNO3, FCL ! "free" ion amounts
      REAL( 8 ) :: FCA, FK, FMG            
      REAL( 8 ) :: CASO4         ! amount of calcium sulfate, does not participate in ZSR calc
      REAL( 8 ) :: WATER         ! kg of water for new time step 
      REAL( 8 ) :: X, Y
      REAL( 8 ) :: CONC( NCMP )    ! concentration (mol/m^3)
      REAL( 8 ) :: CONCR( NPAIR )  ! concentration (mol/m^3) ion "pairs" 
      REAL( 8 ) :: M0I( NPAIR )    ! single-solute molalities
      INTEGER :: J

C-----------------------------------------------------------------------

C     Return if small concentration
      IF ( WI( 1 ) + WI( 2 ) + WI( 3 ) + WI( 4 )
     &   + WI( 5 ) + WI( 6 ) + WI( 7 ) + WI( 8 ) .LE. SMALL) THEN
         H2O_NEW = SMALL
         RETURN
      END IF

C     Set component array (mol/m^3) for determining salts
      CONC = WI   ! array assignment

C     Get the sub-case to use in determining salts
      CALL GETSC ( CONC, RH, T, SC )

#ifdef verbose_aero
!     write( logdev,* ) 'CALC_H2O -SC: ', sc
#endif

C     Initialize ion "pairs" (i.e., salts) used in ZSR
      CONCR( : ) = 0.0D0

C     Depending on case, determine moles of salts in solution (i.e., CONCR)
C     for ZSR calculation below

      IF ( SC .EQ. 'S2' ) THEN    ! sulfate poor (NH4-SO4 system), old K2
         CONCR( 4 )= MIN ( CONC( 2 ), 0.5D0 * CONC( 3 ) )  ! (NH4)2SO4

      ELSE IF ( SC .EQ. 'B4' ) THEN  ! sulfate rich (no acid), old L4, O4
         X = 2.0D0 * CONC( 2 ) - CONC( 3 )     ! 2SO4 - NH4
         Y = CONC( 3 ) - CONC( 2 )            ! NH4 - SO4
         IF ( X .LE. Y ) THEN
            CONCR( 13 ) = X      ! (NH4)3H(SO4)2 is MIN (X,Y)
            CONCR(  4 ) = Y - X  ! (NH4)2SO4
         ELSE
            CONCR( 13 ) = Y      ! (NH4)3H(SO4)2 is MIN (X,Y)
            CONCR(  9 ) = X - Y  ! NH4HSO4
         END IF

      ELSE IF ( SC .EQ. 'C2' ) THEN  ! sulfate rich (free acid), old M2, P2
         CONCR( 9 ) = CONC( 3 )                      ! NH4HSO4
         CONCR( 7 ) = MAX( CONC( 2 ) - CONC( 3 ), 0.0D0 )   ! H2SO4

      ELSE IF ( SC .EQ. 'N3' ) THEN    ! sulfate poor (NH4-SO4-NO3 system)
         CONCR( 4 ) = MIN ( CONC( 2 ), 0.5D0 * CONC( 3 ) )           ! (NH4)2SO4
         FNH4       = MAX ( CONC( 3 ) - 2.0D0 * CONCR( 4 ), 0.0D0 )  ! available NH4
         CONCR( 5 ) = MAX ( MIN ( FNH4, CONC( 4 ) ), 0.0D0 )         ! NH4NO3=MIN(NH4,NO3)

      ELSE IF ( SC .EQ. 'Q5' ) THEN    ! sulfate poor, sodium poor (NH4-SO4-NO3-Cl-Na)
         CONCR( 2 ) = 0.5D0 * CONC( 1 )                              ! Na2SO4
         FSO4       = MAX ( CONC( 2 ) - CONCR( 2 ), 0.0D0 )          ! available SO4
         CONCR( 4 ) = MAX ( MIN ( FSO4, 0.5D0 * CONC( 3 ) ), SMALL ) ! NH42S4=MIN(NH4,S4)
         FNH4       = MAX ( CONC( 3 ) - 2.0D0 * CONCR( 4 ), 0.0D0 )  ! available NH4
         CONCR( 5 ) = MIN ( FNH4, CONC( 4 ) )                        ! NH4NO3=MIN(NH4,NO3)
         FNH4       = MAX ( FNH4 - CONCR( 5 ), 0.0D0 )               ! avaialable NH4
         CONCR( 6 ) = MIN ( FNH4, CONC( 5 ) )                        ! NH4Cl=MIN(NH4,Cl)

      ELSE IF ( SC .EQ. 'R6' ) THEN   ! sulfate poor, sodium rich (NH4-SO4-NO3-Cl-Na)
         CONCR( 2 ) = CONC( 2 )                            ! Na2SO4
         FNA        = MAX ( CONC( 1 ) - 2.0D0 * CONCR( 2 ), 0.0D0 )

         CONCR( 3 ) = MIN ( FNA, CONC( 4 ) )               ! NaNO3
         FNO3       = MAX ( CONC( 4 ) - CONCR( 3 ), 0.0D0 )
         FNA        = MAX ( FNA - CONCR( 3 ), 0.0D0 )

         CONCR( 1 ) = MIN ( FNA, CONC( 5 ) )               ! NaCl
         FCL        = MAX ( CONC( 5 ) - CONCR( 1 ), 0.0D0 )
         FNA        = MAX ( FNA - CONCR( 1 ), 0.0D0 )

         CONCR( 5 ) = MIN ( FNO3, CONC( 3 ) )              ! NH4NO3
         FNO3       = MAX ( FNO3 - CONCR( 5 ), 0.0D0 )
         FNH4       = MAX ( CONC( 3 ) - CONCR( 5 ), 0.0D0 )

         CONCR( 6 ) = MIN (FCL, FNH4 )                     ! NH4Cl

      ELSE IF ( SC .EQ. 'I6' ) THEN   ! sulfate rich (no acid) (NH4-SO4-NO3-Cl-Na)
         CONCR(  2 ) = 0.5D0 * CONC( 1 )                          ! Na2SO4
         FSO4        = MAX ( CONC( 2 ) - CONCR( 2 ), 0.0D0 )
         CONCR( 13 ) = MIN ( CONC( 3 ) / 3.0D0, FSO4 / 2.0D0 )    ! (NH4)3H(SO4)2
         FSO4        = MAX ( FSO4 - 2.0D0 * CONCR( 13 ), 0.0D0 )
         FNH4        = MAX ( CONC( 3 ) - 3.0D0 * CONCR( 13 ), 0.0D0 )

         IF ( FSO4 .LE. SMALL ) THEN    ! reduce (NH4)3H(SO4)2, add (NH4)2SO4
            CONCR( 13 ) = MAX ( CONCR( 13 ) - FNH4, 0.0D0 )   ! (NH4)3H(SO4)2
            CONCR(  4 ) = 2.0D0 * FNH4                  ! (NH4)2SO4
         ELSE IF ( FNH4 .LE. SMALL ) THEN ! reduce (NH4)3H(SO4)2, add NH4HSO4
            CONCR(  9 ) = 3.0D0 * MIN ( FSO4, CONCR( 13 ) ) ! NH4HSO4
            CONCR( 13 ) = MAX ( CONCR( 13 ) - FSO4, 0.0D0 )
            IF ( CONCR( 2 ) .GT. SMALL ) THEN ! reduce Na2SO4, add NaHSO4
               FSO4        = MAX ( FSO4 - CONCR( 9 ) / 3.0D0, 0.0D0 )
               CONCR( 12 ) = 2.0D0 * FSO4                ! NaHSO4
               CONCR(  2 ) = MAX ( CONCR( 2 ) - FSO4, 0.0D0 )  ! Na2SO4
             END IF
         END IF

      ELSE IF ( SC .EQ. 'J3' ) THEN   ! sulfate rich (free acid) (NH4-SO4-NO3-Cl-Na)
         CONCR(  9 ) = CONC( 3 )                             ! NH4HSO4
         CONCR( 12 ) = CONC( 1 )                             ! NAHSO4
         CONCR(  7 ) = MAX ( CONC( 2 ) - CONC( 3 ) - CONC( 1 ), 0.0D0 ) ! H2SO4

      ! Crustal cases
      ELSE IF ( SC .EQ. 'V7' ) THEN  ! sulfate poor, sodium+crustal poor
         CASO4     = MIN ( CONC( 6 ), CONC( 2 ) )            ! CCASO4
         FSO4      = MAX ( CONC( 2 ) - CASO4, 0.0D0 )
         FCA       = MAX ( CONC( 6 ) - CASO4, 0.0D0 )

         CONCR( 17 ) = MIN ( 0.5D0 * CONC( 7 ), FSO4 )       ! CK2SO4
         FK          = MAX ( CONC( 7 ) - 2.D0 * CONCR( 17 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 17 ), 0.0D0 )

         CONCR( 2 )  = MIN ( 0.5D0 * CONC( 1 ), FSO4 )       ! CNA2SO4
         FNA         = MAX ( CONC( 1 ) - 2.0D0 * CONCR( 2 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 2 ), 0.0D0 )

         CONCR( 21 ) = MIN ( CONC( 8 ), FSO4 )               ! CMGSO4
         FMG         = MAX ( CONC( 8 ) - CONCR( 21 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 21 ), 0.0D0 )

         CONCR( 4 )  = MAX ( MIN ( FSO4 , 0.5D0 * CONC( 3 ) ) , SMALL ) ! CNH42S4
         FNH4        = MAX ( CONC( 3 ) - 2.0D0 * CONCR( 4 ), 0.0D0 )

         CONCR( 5 )  = MIN ( FNH4, CONC( 4 ) )               ! CNH4NO3
         FNH4        = MAX ( FNH4 - CONCR( 5 ), 0.0D0 )

         CONCR( 6 )  = MIN ( FNH4, CONC( 5 ) )               ! CNH4CL

      ELSE IF ( SC .EQ. 'U8' ) THEN  ! sulfate poor, crustal+sodium rich, crustal poor
         CASO4       = MIN ( CONC( 6 ), CONC( 2 ) )          ! CCASO4
         FSO4        = MAX ( CONC( 2 ) - CASO4, 0.0D0 )
         FCA         = MAX ( CONC( 6 ) - CASO4, 0.0D0 )

         CONCR( 17 ) = MIN ( 0.5D0 * CONC( 7 ), FSO4 )       ! CK2SO4
         FK          = MAX ( CONC( 7 ) - 2.0D0 * CONCR( 17 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 17 ), 0.0D0 )

         CONCR( 21 ) = MIN ( CONC( 8 ), FSO4 )               ! CMGSO4
         FMG         = MAX ( CONC( 8 ) - CONCR( 21 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 21 ), 0.0D0 )

         CONCR( 2 )  = MAX ( FSO4, 0.0D0 )                   ! CNA2SO4
         FNA         = MAX ( CONC( 1 ) - 2.0D0 * CONCR( 2 ), 0.0D0 )

         CONCR( 3 )  = MIN ( FNA, CONC( 4 ) )                ! NaNO3
         FNO3        = MAX ( CONC( 4 ) - CONCR( 3 ), 0.0D0 )
         FNA         = MAX ( FNA - CONCR( 3 ), 0.0D0 )

         CONCR( 1 )  = MIN ( FNA, CONC( 5 ) )                ! NaCl
         FCL         = MAX ( CONC( 5 ) - CONCR( 1 ), 0.0D0 )
         FNA         = MAX ( FNA - CONCR( 1 ), 0.0D0 )

         CONCR( 5 )  = MIN ( FNO3, CONC( 3 ) )               ! NH4NO3
         FNO3        = MAX ( FNO3 - CONCR( 5 ), 0.0D0 )
         FNH4        = MAX ( CONC( 3 ) - CONCR( 5 ), 0.0D0 )

         CONCR( 6 )  = MIN ( FCL, FNH4 )                     ! NH4Cl
         FCL         = MAX ( FCL - CONCR( 6 ), 0.0D0 )
         FNH4        = MAX ( FNH4 - CONCR( 6 ), 0.0D0 )

      ELSE IF ( SC .EQ. 'W13' ) THEN  ! sulfate poor, crustal+sodium rich
         CASO4       = MIN ( CONC( 2 ), CONC( 6 ) )          ! CASO4
         FCA         = MAX ( CONC( 6 ) - CASO4, 0.0D0 )
         FSO4        = MAX ( CONC( 2 ) - CASO4, 0.0D0 )

         CONCR( 17 ) = MIN ( FSO4, 0.5D0 * CONC( 7 ) )       ! K2SO4
         FK          = MAX ( CONC( 7 ) - 2.0D0 * CONCR( 17 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 17 ), 0.0D0 )

         CONCR( 21 ) = FSO4                                  ! MGSO4
         FMG         = MAX ( CONC( 8 ) - CONCR( 21 ), 0.0D0 )

         CONCR( 1 )  = MIN ( CONC( 1 ), CONC( 5 ) )          ! NACL
         FNA         = MAX ( CONC( 1 ) - CONCR( 1 ), 0.0D0 )
         FCL         = MAX ( CONC( 5 ) - CONCR( 1 ), 0.0D0 )

         CONCR( 16 ) = MIN ( FCA, 0.5D0 * FCL )              ! CACL2
         FCA         = MAX ( FCA - CONCR( 16 ), 0.0D0 )
         FCL         = MAX ( CONC( 5 ) - 2.0D0 * CONCR( 16 ), 0.0D0 )

         CONCR( 15 ) = MIN ( FCA, 0.5D0 * CONC( 4 ) )        ! CA(NO3)2
         FCA         = MAX ( FCA - CONCR( 15 ), 0.0D0 )
         FNO3        = MAX ( CONC( 4 ) - 2.0D0 * CONCR( 15 ), 0.0D0 )

         CONCR( 23 ) = MIN ( FMG, 0.5D0 * FCL )              ! MGCL2
         FMG         = MAX ( FMG - CONCR( 23 ), 0.0D0 )
         FCL         = MAX ( FCL - 2.0D0 * CONCR( 23 ), 0.0D0 )

         CONCR( 22 ) = MIN ( FMG, 0.5D0 * FNO3 )             ! MG(NO3)2
         FMG         = MAX ( FMG - CONCR( 22 ), 0.0D0 )
         FNO3        = MAX ( FNO3 - 2.0D0 * CONCR( 22 ), 0.0D0 )

         CONCR( 3 )  = MIN ( FNA, FNO3 )                     ! NANO3
         FNA         = MAX ( FNA - CONCR( 3 ), 0.0D0 )
         FNO3        = MAX ( FNO3 - CONCR( 3 ), 0.0D0 )

         CONCR( 20 ) = MIN ( FK, FCL )                       ! KCL
         FK          = MAX ( FK - CONCR( 20 ), 0.0D0 )
         FCL         = MAX ( FCL - CONCR( 20 ), 0.0D0 )

         CONCR( 19 ) = MIN ( FK, FNO3 )                      ! KNO3
         FK          = MAX ( FK - CONCR( 19 ), 0.0D0 )
         FNO3        = MAX ( FNO3 - CONCR( 19 ), 0.0D0 )

      ELSE IF ( SC .EQ. 'L9' ) THEN  ! sulfate rich, no free acid
         CASO4       = MIN ( CONC( 6 ), CONC( 2 ) )          ! CCASO4
         FSO4        = MAX ( CONC( 2 ) - CASO4, 0.0D0 )
         FCA         = MAX ( CONC( 6 ) - CASO4, 0.0D0 )

         CONCR( 17 ) = MIN ( 0.5D0 * CONC( 7 ), FSO4 )       ! CK2SO4
         FK          = MAX ( CONC( 7 ) - 2.0D0 * CONCR( 17 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 17 ), 0.0D0 )

         CONCR( 2 )  = MIN ( 0.5D0 * CONC( 1 ), FSO4 )       ! CNA2SO4
         FNA         = MAX ( CONC( 1 ) - 2.0D0 * CONCR( 2 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 2 ), 0.0D0 )

         CONCR( 21 ) = MIN ( CONC( 8 ), FSO4 )               ! CMGSO4
         FMG         = MAX ( CONC( 8 ) - CONCR( 21 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 21 ), 0.0D0 )

         CONCR( 13 ) = MIN ( CONC( 3 ) / 3.0D0, FSO4 / 2.0D0 ) ! CLC
         FSO4        = MAX ( FSO4 - 2.0D0 * CONCR( 13 ), 0.0D0 )
         FNH4        = MAX ( CONC( 3 )- 3.0D0 * CONCR( 13 ),  0.0D0 )

         IF ( FSO4 .LE. SMALL ) THEN                           ! convert (NH4)3H(SO4)2 to (NH4)2SO4
            CONCR( 13 ) = MAX( CONCR( 13 ) - FNH4, 0.0D0 )
            CONCR(  4 ) = 2.0D0 * FNH4                         ! CNH42S4 

         ELSE IF ( FNH4 .LE. SMALL ) THEN                      ! convert (NH4)3H(SO4)2 to NH4HSO4
            CONCR(  9 ) = 3.0D0 * MIN( FSO4, CONCR( 13 ) )     ! CNH4HS4
            CONCR( 13 ) = MAX( CONCR( 13 ) - FSO4, 0.0D0 )     ! CLC, (NH4)3H(SO4)2
            IF ( CONCR( 2 ) .GT. SMALL ) THEN                  ! convert Na2SO4 to NaHSO4
               FSO4        = MAX( FSO4 - CONCR( 9 ) / 3.0D0, 0.0D0 )
               CONCR( 12 ) = 2.0D0 * FSO4                      ! CNAHSO4
               CONCR(  2 ) = MAX( CONCR( 2 ) - FSO4, 0.0D0 )   ! CNA2SO4
            END IF
            IF ( CONCR( 17 ) .GT. SMALL ) THEN                 ! convert K2SO4 to KHSO4
               FSO4        = MAX( FSO4 - CONCR( 9 ) / 3.0D0, 0.0D0 )
               CONCR( 18 ) = 2.0D0 * FSO4                      ! CKHSO4
               CONCR( 17 ) = MAX( CONCR( 17 ) - FSO4, 0.0D0 )  ! CK2SO4
            END IF
         END IF
         
      ELSE IF ( SC .EQ. 'K4' ) THEN ! sulfate super rich, free acid
         CONCR(  9 ) = CONC( 3 )                               ! NH4HSO4 = NH3
         CONCR( 12 ) = CONC( 1 )                               ! NaHSO4  = Na
         CONCR( 18 ) = CONC( 7 )                               ! KHSO4   = K
         CONCR( 21 ) = CONC( 8 )                               ! MgSO4   = Mg
         CONCR(  7 ) = MAX( CONC( 2 ) - CONC( 3 ) - CONC( 1 )
     &                    - CONC( 6 ) - CONC( 7 ) - CONC( 8 ), 0.0D0 ) ! H2SO4 = SO4 - NH4 - Na - Ca - K - Mg

      ELSE
         PRINT*, 'aero_subs.f: case not supported ',
     &          '(metastable reverse only)'
      END IF

C     Get single-solute molalities for ZSR calculation
      CALL GETM0I ( RH, M0I )

C     Calculate H2O with ZSR and determine delta water
      WATER = 0.0D0
      DO J = 1, NPAIR
         WATER = WATER + CONCR( J ) / M0I( J )
      END DO

      WATER = MAX ( WATER, SMALL )
      H2O_NEW = WATER / Mw

      END SUBROUTINE CALC_H2O  


C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETSC ( CONC, RH, T, SC )
 
C Description
C   Determines the sub-case to use for water uptake calculations.
C   Follows the procedure of ISORROPIA.
C   Routine called by CALC_H2O.
 
C ArgumentS
C   Inputs
C     CONC : Concentration [mol/m^3] of aerosol components. This routine
C            sets minimum CONC to 1.0D-20
C     RH   : Relative humidity
C     T    : Temperature (K)
     
C   Output
C     SC   : Sub-case for aerosol composition
 
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, PARAMETER :: NCMP = 8    ! was NCMP  = 5    ! number of aerosol components
      REAL( 8 ), PARAMETER :: SMALL = 1.0D-20

C Arguments:
!     REAL( 8 ), INTENT( IN )    :: CONC(  NCMP )
      REAL( 8 ), INTENT( INOUT ) :: CONC(  NCMP )
      REAL( 8 ), INTENT( IN )    :: RH, T
      CHARACTER( 3 ), INTENT( OUT ) :: SC
            
C Local Variables:
      REAL( 8 ) :: T0, TCF                     ! DRH(T) factor
      REAL( 8 ) :: S4RAT, S4RATW, NaRAT, SRI   ! sulfate & sodium ratios
      REAL( 8 ) :: CRAT                        ! crustals ratio
      REAL( 8 ) :: FSO4                        ! "free" sulfate
      REAL( 8 ) :: DNACL, DNH4CL, DNANO3, DNH4NO3, DNH42S4 ! DRH values

      REAL( 8 ) :: GETASR    ! ISORROPIA function for sulfate ratio

      LOGICAL :: SCP1R, SCP2R, SCP3R, SCP4R ! concentration regime

C-----------------------------------------------------------------------

      SCP1R = .FALSE.
      SCP2R = .FALSE.
      SCP3R = .FALSE.
      SCP4R = .FALSE.

C     See if any components are negligible (see isocom.for)
      IF ( CONC( 1 ) + CONC( 4 ) + CONC( 5 ) + 
     &     CONC( 6 ) + CONC( 7 ) + CONC( 8 ) .LE. SMALL ) THEN       ! Ca,K,Mg,Na,Cl,NO3=0
         SCP1R = .TRUE.                                    
      ELSE IF ( CONC( 1 ) +        CONC( 5 ) +
     &          CONC( 6 ) + CONC( 7 ) + CONC( 8 ) .LE. SMALL ) THEN  ! Ca,K,Mg,Na,Cl=0
         SCP2R = .TRUE.                                     
      ELSE IF ( CONC( 6 ) + CONC( 7 ) + CONC( 8 ) .LE. SMALL ) THEN  ! Ca,K,Mg=0
         SCP3R = .TRUE.                                     
      ELSE                                                           ! all species
         SCP4R = .TRUE.
      END IF

      CONC( : ) = MAX ( CONC( : ), SMALL )

C     Deliquescence RH calculations
      DNH42S4 = 0.7997D0
      DNH4NO3 = 0.6183D0
      IF ( INT( T ) .NE. 298 ) THEN
         T0      = 298.15D0
         TCF     = 1.0D0 / T - 1.0D0 / T0
         DNH4NO3 = DNH4NO3 * EXP( 852.0D0 * TCF )
         DNH42S4 = DNH42S4 * EXP(  80.0D0 * TCF )
         DNH4NO3 = MIN ( DNH4NO3, DNH42S4 ) ! adjust for curves crossing T<271K
      END IF

C     Find sub-case "SC"
      IF ( SCP1R ) THEN ! NH4-S04 system

         IF ( RH .GE. DNH42S4 ) THEN
            S4RATW = GETASR( CONC( 2 ), RH ) ! aerosol sulfate ratio
         ELSE
            S4RATW = 2.0D0                ! dry aerosol sulfate ratio
         END IF
         S4RAT  = CONC( 3 ) / CONC( 2 )     ! sulfate ratio (NH4/SO4)

         IF ( S4RATW .LE. S4RAT ) THEN      ! sulfate poor
            SC = 'S2'
         ELSE IF ( 1.0D0 .LE. S4RAT .AND. S4RAT .LT. S4RATW ) THEN ! sulfate rich (no acid)
            SC = 'B4'
         ELSE IF ( S4RAT .LT. 1.0D0 ) THEN   ! sulfate rich (free acid)
            SC = 'C2'
         END IF

      ELSE IF ( SCP2R ) THEN ! NH4-SO4-NO3 system

         IF ( RH .GE. DNH4NO3 ) THEN
            S4RATW = GETASR( CONC( 2 ), RH )
         ELSE
            S4RATW = 2.0D0               ! dry aerosol ratio
         END IF
         S4RAT = CONC( 3 ) / CONC( 2 )

         IF ( S4RATW .LE. S4RAT ) THEN     ! sulfate poor
            SC = 'N3'
         ELSE IF ( 1.0D0 .LE. S4RAT .AND. S4RAT .LT. S4RATW ) THEN  ! sulfate rich (no acid)
            SC = 'B4'
         ELSE IF ( S4RAT .LT. 1.0D0 ) THEN    ! sulfate rich (free acid)
            SC = 'C2'
         END IF

      ELSE IF ( SCP3R )  THEN ! NH4-SO4-NO3-Na-Cl system

C        Adjust DRH of NH4NO3 for low temperature
         DNACL  = 0.7528D0
         DNANO3 = 0.7379D0
         DNH4CL = 0.7710D0
         IF ( INT( T ) .NE. 298 ) THEN
            DNACL   = DNACL  * EXP(  25.0D0 * TCF )
            DNANO3  = DNANO3 * EXP( 304.0D0 * TCF )
            DNH4CL  = DNH4Cl * EXP( 239.0D0 * TCF )
            DNH4NO3 = MIN ( DNH4NO3, DNH4CL, DNANO3, DNACL )
         END IF

         IF ( RH .GE. DNH4NO3 ) THEN
            FSO4   = CONC( 2 ) - CONC( 1 ) / 2.0D0   ! sulfate unbound by Na+
            FSO4   = MAX ( FSO4, SMALL )
            SRI    = GETASR ( FSO4, RH )
            S4RATW = ( CONC( 1 ) + FSO4 * SRI ) / CONC( 2 )
            S4RATW = MIN ( S4RATW, 2.0D0 )
         ELSE
            S4RATW = 2.0D0                       ! ratio for dry aerosol
         END IF
         S4RAT = ( CONC( 1 ) + CONC( 3 ) ) / CONC( 2 )
         NaRAT = CONC( 1 ) / CONC( 2 )

         IF ( S4RATW .LE. S4RAT .AND. NaRAT .LT. 2.0D0 ) THEN ! sulfate poor, sodium poor
            SC = 'Q5'
         ELSE IF ( S4RAT .GE. S4RATW .AND. NaRAT .GE. 2.0D0 ) THEN ! SO4 poor, Na rich
            SC = 'R6'
         ELSE IF ( 1.0D0 .LE. S4RAT .AND. S4RAT .LT. S4RATW ) THEN ! SO4 rich, no acid
            SC = 'I6'
         ELSE IF ( S4RAT .LT. 1.0D0 ) THEN ! sulfate rich, free acid
            SC = 'J3'
         END IF

      ELSE IF ( SCP4R ) THEN ! NH4-SO4-Na-Cl-Ca-K-Mg system

         ! Do I need an RH if check here????
         FSO4   = CONC( 2 ) - CONC( 1 ) / 2.0D0
     &          - CONC( 6 ) - CONC( 7 ) / 2.0D0 - CONC( 8 )  ! sulfate unbound by sodium,calcium,pottasium,magnesium
         FSO4   = MAX ( FSO4, SMALL )
         SRI    = GETASR( FSO4, RH )                         ! sulfate ratio for NH4+
         S4RATW = ( CONC( 1 ) + FSO4 * SRI + CONC( 6 )
     &            + CONC( 7 ) + CONC( 8 ) ) / CONC( 2 )      ! limiting sulfate ratio
         S4RATW = MIN ( S4RATW, 2.0D0 )
         S4RAT = ( CONC( 1 ) + CONC( 3 ) + CONC( 6 ) + CONC( 7 ) + CONC( 8 ) ) / CONC( 2 ) ! sulfate ratio
         NaRAT = ( CONC( 1 ) + CONC( 6 ) + CONC( 7 ) + CONC( 8 ) ) / CONC( 2 ) ! crustals+sodium ratio
         CRAT  = ( CONC( 6 ) + CONC( 7 ) + CONC( 8 ) ) / CONC( 2 )             ! crustals ratio

         IF ( S4RATW .LE. S4RAT .AND. NaRAT .LT. 2.0D0 ) THEN ! sulfate, sodium, crustal poor
            SC = 'V7'
         ELSE IF ( S4RAT .GE. S4RATW .AND. NaRAT .GE. 2.0D0 ) THEN
            IF ( CRAT .LE. 2.0D0 ) THEN       ! sulfate poor, dust+sodium rich, dust poor
               SC = 'U8'
            ELSE                              ! sulfate poor, dust+sodium rich, dust rich
               SC = 'W13'
            END IF
         ELSE IF ( 1.0D0 .LE. S4RAT .AND. S4RAT .LT. S4RATW ) THEN ! sulfate rich, no acid
            SC = 'L9'
         ELSE IF ( S4RAT .LT. 1.0D0 ) THEN     ! sulfate rich, free acid
            SC = 'K4'
         END IF
      END IF

      !print*,'SUBCASE identified in calc_h2o', SC

      END SUBROUTINE GETSC

      END MODULE MASS_TRANSFER

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETM0I ( RHIN, M0I )

!!!!!!!!!!!!!!! We want to get rid of this dependency !!!!!!!!!!!!!!!!

C Description
C   Determines single-solute molalities for the 13 possible salts at
C   the ambient RH.  These molalities are used in the ZSR calculation
C   in CALC_H2O. Note that the molalities were determined at the beginning
C   of the time step, and so they are available in the IONS common block
C   of isrpia.inc.
C   Routine called by CALC_H2O.

C Revision History
C   ??? ???? Prakash Bhave(?), Jim Kelly(?): initial revision
C   Apr 2011 Havala Pye: Removed use of IONS common block since it requires
C                        ISORROPIA to be called to setup the initial array values.
C                        Now uses single-solute molalities from ZSR common block
C                        in isrpia.inc that is defined in BLKISO in isocom.f
C                        BE VERY CAREFUL ABOUT IMPLICIT VARIABLES HERE!
 
C Arguments
C   Output
C     M0I : Single-solute molalities (mol/kg-H2O) for 13 salts
 
C-----------------------------------------------------------------------

!     implicit none

      INCLUDE 'isrpia.inc'

C Arguments
      REAL( 8 ), INTENT( IN )  :: RHIN
      REAL( 8 ), INTENT( OUT ) :: M0I( NPAIR )

      INTEGER IZ

C Location in pure molality array (function of RH)
      IZ = MIN( INT( RHIN * REAL( NZSR, 8 ) + 0.5D0 ), NZSR )
      IZ = MAX( IZ, 1 )

C Default value
      M0I = 1.0D+5   ! array assignment

C Actual values (10,11 not provided)
      M0I( 01 ) = AWSC( IZ )   ! NACl
      M0I( 02 ) = AWSS( IZ )   ! (NA)2SO4
      M0I( 03 ) = AWSN( IZ )   ! NANO3
      M0I( 04 ) = AWAS( IZ )   ! (NH4)2SO4
      M0I( 05 ) = AWAN( IZ )   ! NH4NO3
      M0I( 06 ) = AWAC( IZ )   ! NH4CL
      M0I( 07 ) = AWSA( IZ )   ! 2H-SO4
      M0I( 08 ) = AWSA( IZ )   ! H-HSO4
      M0I( 09 ) = AWAB( IZ )   ! NH4HSO4


      M0I( 12 ) = AWSB( IZ )   ! NAHSO4
      M0I( 13 ) = AWLC( IZ )   ! (NH4)3H(SO4)2

      M0I( 15 ) = AWCN( IZ )   ! CA(NO3)2
      M0I( 16 ) = AWCC( IZ )   ! CACl2
      M0I( 17 ) = AWPS( IZ )   ! K2SO4
      M0I( 18 ) = AWPB( IZ )   ! KHSO4
      M0I( 19 ) = AWPN( IZ )   ! KNO3
      M0I( 20 ) = AWPC( IZ )   ! KCl
      M0I( 21 ) = AWMS( IZ )   ! MGSO4
      M0I( 22 ) = AWMN( IZ )   ! MG(NO3)2
      M0I( 23 ) = AWMC( IZ )   ! MGCL2
    
      END SUBROUTINE GETM0I               
 
