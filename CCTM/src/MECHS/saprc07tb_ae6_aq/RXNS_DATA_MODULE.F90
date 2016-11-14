       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: /home/hutzellb/CCTM_git_repository/CCTM/src/MECHS//saprc07tb_ae6_aq/mech_saprc07tb_ae6_aq.def
! for Mechanism Name: SAPRC07TB_AE6_AQ                

! This file is used to create mechanism data and functions

! The following are reserved symbols declared in this file:
!    MECHNAME        = Mechanism name
!    N_GAS_CHEM_SPC  = Total number of gas species in chemical mechanism
!    NUMB_CHEM_SPC   = Total number of species in chemical mechanism
!    N_ACT_SP        = Number of active (determined by ODE solver) species in mechanism
!    GAS_CHEM_SPC    = Names of gas species in chemical mechanism
!    CHEMISTRY_SPC   = Names of species in chemical mechanism
!    CGRID_INDEX     = CGRID Index of species in chemical mechanism
!    SPECIES_TYPE    = Group or type of species in chemical mechanism
!    SPECIES_MOLWT   = Molecular Weight of species (gm/mole)
!    NRXNS           = Number of mechanism reactions
!    ZERO_REACT_REACTIONS  = number zero reactant reactions
!    ONE_REACT_REACTIONS   = number one reactant reactions
!    TWO_REACT_REACTIONS   = number second order reactions
!    THREE_REACT_REACTIONS = number three reactant reactions
!    NSUNLIGHT_RXNS  = Number of mechanism reactions requiring sunlight
!    NTHERMAL_RXNS   = Number of mechanism reactions not requiring sunlight
!    IRXBITS         = Bit test mask vector for selected reactions
!    IORDER          = Order of the reaction
!    NTERMS_JACOB    = Maximum number of nonzero terms in day/night Jacobian
!    NSTEPS_JACOB    = Maximum number of LU Decomposition steps to solve each Jacobian

!    NWM       = Number of air 3-body reactions
!    NRXWM     = Reactions list pointer to air 3-body reactions
!    ATM_AIR   = air 3-body reactions concentration
!    NWW       = Number of H2O 3-body reactions
!    NRXWW     = Reactions list pointer to H2O 3-body reactions
!    NWO2      = Number of reactions with O2
!    NRXWO2    = Reactions list pointer to O2 reactions
!    ATM_O2    = Oxygen reactions concentration
!    NWN2      = Number of N2 3-body reactions
!    NRXWN2    = Reactions list pointer to N2 3-body reactions
!    ATM_N2    = Nitrogen 3-body reactions concentration
!    NWCH4     = Number of reactions with CH4
!    NRXWCH4   = Reactions list pointer to CH4 reactions
!    ATM_CH4   = Methane reactions concentration
!    NWH2      = Number of reactions with H2
!    NRXWH2    = Reactions list pointer to H2 reactions
!    ATM_H2    = Hydrogen reactions concentration

!    MXPRD     = Maximum number of mechanism reaction products
!    IRR       = Reactions list pointer to reactants and products
!    SC        = Stoichiometric coefficients
!    NREACT    = Number of reactants in each mechanism reaction
!    NPRDCT    = Number of products in each mechanism reaction
!    RXLABEL   = Character label list for mechanism reactions
!    NMPHOT    = Number of mechanism photolytic reactions
!    NPHOTAB   = Number of photolytic reactions tables
!    IPH       = Reactions list pointer to photolytic reactions and tables
!    MHETERO   = Number of mechanism heteorogenous reactions
!    NHETERO   = Number of unique heteorogenous rate constants
!    IHETERO   = Reactions list pointer to heteorogenous reactions and tables

      CHARACTER( 32 ), PARAMETER :: MECHNAME = 'SAPRC07TB_AE6_AQ'

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 173
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 194

      CHARACTER( 16 ) :: GAS_CHEM_SPC( N_GAS_CHEM_SPC )
      CHARACTER( 16 ) :: CHEMISTRY_SPC( NUMB_MECH_SPC )
      CHARACTER( 16 ) :: SPECIES_TYPE(  NUMB_MECH_SPC )
      INTEGER         :: CGRID_INDEX (  NUMB_MECH_SPC )
      INTEGER         :: TYPE_INDEX  (  NUMB_MECH_SPC )
      LOGICAL         :: CONVERT_CONC(  NUMB_MECH_SPC )
      REAL            :: SPECIES_MOLWT( NUMB_MECH_SPC )

! The below character and integer arrays list the model species names used in the 
! chemical mechanism. The gas species and their order should agree with 
! the GC_SPC array for the gas phase chemistry to work correctly. 
! If present, the CHEMISTRY_SPC names and species type should agree with the CGRID_SPCS module

      DATA GAS_CHEM_SPC(   1 ) / 'O1D             ' /
      DATA GAS_CHEM_SPC(   2 ) / 'SO2             ' /
      DATA GAS_CHEM_SPC(   3 ) / 'SULF            ' /
      DATA GAS_CHEM_SPC(   4 ) / 'SULRXN          ' /
      DATA GAS_CHEM_SPC(   5 ) / 'BACL            ' /
      DATA GAS_CHEM_SPC(   6 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC(   7 ) / 'BENZENE         ' /
      DATA GAS_CHEM_SPC(   8 ) / 'SOAALK          ' /
      DATA GAS_CHEM_SPC(   9 ) / 'ALKRXN          ' /
      DATA GAS_CHEM_SPC(  10 ) / 'CLACET          ' /
      DATA GAS_CHEM_SPC(  11 ) / 'CLCHO           ' /
      DATA GAS_CHEM_SPC(  12 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC(  13 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC(  14 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC(  15 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC(  16 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC(  17 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC(  18 ) / 'PAHNRXN         ' /
      DATA GAS_CHEM_SPC(  19 ) / 'PAHHRXN         ' /
      DATA GAS_CHEM_SPC(  20 ) / 'PCVOC           ' /
      DATA GAS_CHEM_SPC(  21 ) / 'PCSOARXN        ' /
      DATA GAS_CHEM_SPC(  22 ) / 'N2O5            ' /
      DATA GAS_CHEM_SPC(  23 ) / 'HO2H            ' /
      DATA GAS_CHEM_SPC(  24 ) / 'PAN             ' /
      DATA GAS_CHEM_SPC(  25 ) / 'PAN2            ' /
      DATA GAS_CHEM_SPC(  26 ) / 'PBZN            ' /
      DATA GAS_CHEM_SPC(  27 ) / 'MAPAN           ' /
      DATA GAS_CHEM_SPC(  28 ) / 'TBUO            ' /
      DATA GAS_CHEM_SPC(  29 ) / 'ISOPOOH         ' /
      DATA GAS_CHEM_SPC(  30 ) / 'IEPOX           ' /
      DATA GAS_CHEM_SPC(  31 ) / 'TOLUENE         ' /
      DATA GAS_CHEM_SPC(  32 ) / 'MXYL            ' /
      DATA GAS_CHEM_SPC(  33 ) / 'OXYL            ' /
      DATA GAS_CHEM_SPC(  34 ) / 'PXYL            ' /
      DATA GAS_CHEM_SPC(  35 ) / 'TMBENZ124       ' /
      DATA GAS_CHEM_SPC(  36 ) / 'ETOH            ' /
      DATA GAS_CHEM_SPC(  37 ) / 'ALK1            ' /
      DATA GAS_CHEM_SPC(  38 ) / 'ALK2            ' /
      DATA GAS_CHEM_SPC(  39 ) / 'ALK3            ' /
      DATA GAS_CHEM_SPC(  40 ) / 'ALK4            ' /
      DATA GAS_CHEM_SPC(  41 ) / 'ALK5            ' /
      DATA GAS_CHEM_SPC(  42 ) / 'ARO1            ' /
      DATA GAS_CHEM_SPC(  43 ) / 'NAPHTHAL        ' /
      DATA GAS_CHEM_SPC(  44 ) / 'CLNO            ' /
      DATA GAS_CHEM_SPC(  45 ) / 'CLONO           ' /
      DATA GAS_CHEM_SPC(  46 ) / 'HOCL            ' /
      DATA GAS_CHEM_SPC(  47 ) / 'H2NO3PK         ' /
      DATA GAS_CHEM_SPC(  48 ) / 'VIVPO1          ' /
      DATA GAS_CHEM_SPC(  49 ) / 'HNO4            ' /
      DATA GAS_CHEM_SPC(  50 ) / 'COOH            ' /
      DATA GAS_CHEM_SPC(  51 ) / 'CCOOOH          ' /
      DATA GAS_CHEM_SPC(  52 ) / 'NPHE            ' /
      DATA GAS_CHEM_SPC(  53 ) / 'xTBUO           ' /
      DATA GAS_CHEM_SPC(  54 ) / 'ROOH            ' /
      DATA GAS_CHEM_SPC(  55 ) / 'R6OOH           ' /
      DATA GAS_CHEM_SPC(  56 ) / 'RAOOH           ' /
      DATA GAS_CHEM_SPC(  57 ) / 'yISOPOOH        ' /
      DATA GAS_CHEM_SPC(  58 ) / 'ARO2MN          ' /
      DATA GAS_CHEM_SPC(  59 ) / 'ACETYLENE       ' /
      DATA GAS_CHEM_SPC(  60 ) / 'CL2             ' /
      DATA GAS_CHEM_SPC(  61 ) / 'CLONO2          ' /
      DATA GAS_CHEM_SPC(  62 ) / 'CLCCHO          ' /
      DATA GAS_CHEM_SPC(  63 ) / 'HCHO_PRIMARY    ' /
      DATA GAS_CHEM_SPC(  64 ) / 'CCHO_PRIMARY    ' /
      DATA GAS_CHEM_SPC(  65 ) / 'H2NO3PIJ        ' /
      DATA GAS_CHEM_SPC(  66 ) / 'HONO            ' /
      DATA GAS_CHEM_SPC(  67 ) / 'BALD            ' /
      DATA GAS_CHEM_SPC(  68 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC(  69 ) / 'PAHRO2          ' /
      DATA GAS_CHEM_SPC(  70 ) / 'SESQRXN         ' /
      DATA GAS_CHEM_SPC(  71 ) / 'VSVPO3          ' /
      DATA GAS_CHEM_SPC(  72 ) / 'RCOOOH          ' /
      DATA GAS_CHEM_SPC(  73 ) / 'ETHENE          ' /
      DATA GAS_CHEM_SPC(  74 ) / 'PROPENE         ' /
      DATA GAS_CHEM_SPC(  75 ) / 'BUTADIENE13     ' /
      DATA GAS_CHEM_SPC(  76 ) / 'ISOPRENE        ' /
      DATA GAS_CHEM_SPC(  77 ) / 'APIN            ' /
      DATA GAS_CHEM_SPC(  78 ) / 'TRPRXN          ' /
      DATA GAS_CHEM_SPC(  79 ) / 'TOLRO2          ' /
      DATA GAS_CHEM_SPC(  80 ) / 'OLE1            ' /
      DATA GAS_CHEM_SPC(  81 ) / 'OLE2            ' /
      DATA GAS_CHEM_SPC(  82 ) / 'TERP            ' /
      DATA GAS_CHEM_SPC(  83 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC(  84 ) / 'ACRO_PRIMARY    ' /
      DATA GAS_CHEM_SPC(  85 ) / 'VLVPO1          ' /
      DATA GAS_CHEM_SPC(  86 ) / 'VSVPO1          ' /
      DATA GAS_CHEM_SPC(  87 ) / 'VSVPO2          ' /
      DATA GAS_CHEM_SPC(  88 ) / 'VSVOO1          ' /
      DATA GAS_CHEM_SPC(  89 ) / 'MEOH            ' /
      DATA GAS_CHEM_SPC(  90 ) / 'RNO3            ' /
      DATA GAS_CHEM_SPC(  91 ) / 'ACETONE         ' /
      DATA GAS_CHEM_SPC(  92 ) / 'xMEO2           ' /
      DATA GAS_CHEM_SPC(  93 ) / 'HCOCO3          ' /
      DATA GAS_CHEM_SPC(  94 ) / 'xACROLEIN       ' /
      DATA GAS_CHEM_SPC(  95 ) / 'xCLACET         ' /
      DATA GAS_CHEM_SPC(  96 ) / 'VSVOO2          ' /
      DATA GAS_CHEM_SPC(  97 ) / 'VSVOO3          ' /
      DATA GAS_CHEM_SPC(  98 ) / 'xOH             ' /
      DATA GAS_CHEM_SPC(  99 ) / 'AFG1            ' /
      DATA GAS_CHEM_SPC( 100 ) / 'AFG2            ' /
      DATA GAS_CHEM_SPC( 101 ) / 'ACROLEIN        ' /
      DATA GAS_CHEM_SPC( 102 ) / 'yRAOOH          ' /
      DATA GAS_CHEM_SPC( 103 ) / 'IEPOXOO         ' /
      DATA GAS_CHEM_SPC( 104 ) / 'CLNO2           ' /
      DATA GAS_CHEM_SPC( 105 ) / 'CLO             ' /
      DATA GAS_CHEM_SPC( 106 ) / 'CCOOH           ' /
      DATA GAS_CHEM_SPC( 107 ) / 'xMACO3          ' /
      DATA GAS_CHEM_SPC( 108 ) / 'IPRD            ' /
      DATA GAS_CHEM_SPC( 109 ) / 'XYLRO2          ' /
      DATA GAS_CHEM_SPC( 110 ) / 'MACR            ' /
      DATA GAS_CHEM_SPC( 111 ) / 'MVK             ' /
      DATA GAS_CHEM_SPC( 112 ) / 'xAFG3           ' /
      DATA GAS_CHEM_SPC( 113 ) / 'xNO2            ' /
      DATA GAS_CHEM_SPC( 114 ) / 'xHOCCHO         ' /
      DATA GAS_CHEM_SPC( 115 ) / 'HOCCHO          ' /
      DATA GAS_CHEM_SPC( 116 ) / 'xBALD           ' /
      DATA GAS_CHEM_SPC( 117 ) / 'xMACR           ' /
      DATA GAS_CHEM_SPC( 118 ) / 'xCL             ' /
      DATA GAS_CHEM_SPC( 119 ) / 'VLVOO1          ' /
      DATA GAS_CHEM_SPC( 120 ) / 'VLVOO2          ' /
      DATA GAS_CHEM_SPC( 121 ) / 'RCOOH           ' /
      DATA GAS_CHEM_SPC( 122 ) / 'xMVK            ' /
      DATA GAS_CHEM_SPC( 123 ) / 'CCHO            ' /
      DATA GAS_CHEM_SPC( 124 ) / 'xBACL           ' /
      DATA GAS_CHEM_SPC( 125 ) / 'AFG3            ' /
      DATA GAS_CHEM_SPC( 126 ) / 'xCLCCHO         ' /
      DATA GAS_CHEM_SPC( 127 ) / 'xRNO3           ' /
      DATA GAS_CHEM_SPC( 128 ) / 'BZCO3           ' /
      DATA GAS_CHEM_SPC( 129 ) / 'CRES            ' /
      DATA GAS_CHEM_SPC( 130 ) / 'xIPRD           ' /
      DATA GAS_CHEM_SPC( 131 ) / 'xACETONE        ' /
      DATA GAS_CHEM_SPC( 132 ) / 'BZO             ' /
      DATA GAS_CHEM_SPC( 133 ) / 'xMECO3          ' /
      DATA GAS_CHEM_SPC( 134 ) / 'xRCO3           ' /
      DATA GAS_CHEM_SPC( 135 ) / 'xPROD2          ' /
      DATA GAS_CHEM_SPC( 136 ) / 'O3P             ' /
      DATA GAS_CHEM_SPC( 137 ) / 'HNO3            ' /
      DATA GAS_CHEM_SPC( 138 ) / 'xCO             ' /
      DATA GAS_CHEM_SPC( 139 ) / 'MGLY            ' /
      DATA GAS_CHEM_SPC( 140 ) / 'xMEK            ' /
      DATA GAS_CHEM_SPC( 141 ) / 'RCHO            ' /
      DATA GAS_CHEM_SPC( 142 ) / 'xAFG1           ' /
      DATA GAS_CHEM_SPC( 143 ) / 'xAFG2           ' /
      DATA GAS_CHEM_SPC( 144 ) / 'GLY             ' /
      DATA GAS_CHEM_SPC( 145 ) / 'RCO3            ' /
      DATA GAS_CHEM_SPC( 146 ) / 'MACO3           ' /
      DATA GAS_CHEM_SPC( 147 ) / 'MEK             ' /
      DATA GAS_CHEM_SPC( 148 ) / 'HCOOH           ' /
      DATA GAS_CHEM_SPC( 149 ) / 'NO              ' /
      DATA GAS_CHEM_SPC( 150 ) / 'xGLY            ' /
      DATA GAS_CHEM_SPC( 151 ) / 'xMGLY           ' /
      DATA GAS_CHEM_SPC( 152 ) / 'PRD2            ' /
      DATA GAS_CHEM_SPC( 153 ) / 'xRCHO           ' /
      DATA GAS_CHEM_SPC( 154 ) / 'O3              ' /
      DATA GAS_CHEM_SPC( 155 ) / 'MEO2            ' /
      DATA GAS_CHEM_SPC( 156 ) / 'MECO3           ' /
      DATA GAS_CHEM_SPC( 157 ) / 'xCCHO           ' /
      DATA GAS_CHEM_SPC( 158 ) / 'yR6OOH          ' /
      DATA GAS_CHEM_SPC( 159 ) / 'NO2             ' /
      DATA GAS_CHEM_SPC( 160 ) / 'xHCHO           ' /
      DATA GAS_CHEM_SPC( 161 ) / 'HCHO            ' /
      DATA GAS_CHEM_SPC( 162 ) / 'HCL             ' /
      DATA GAS_CHEM_SPC( 163 ) / 'CO              ' /
      DATA GAS_CHEM_SPC( 164 ) / 'yROOH           ' /
      DATA GAS_CHEM_SPC( 165 ) / 'zRNO3           ' /
      DATA GAS_CHEM_SPC( 166 ) / 'CO2             ' /
      DATA GAS_CHEM_SPC( 167 ) / 'NO3             ' /
      DATA GAS_CHEM_SPC( 168 ) / 'RO2XC           ' /
      DATA GAS_CHEM_SPC( 169 ) / 'xHO2            ' /
      DATA GAS_CHEM_SPC( 170 ) / 'CL              ' /
      DATA GAS_CHEM_SPC( 171 ) / 'RO2C            ' /
      DATA GAS_CHEM_SPC( 172 ) / 'HO2             ' /
      DATA GAS_CHEM_SPC( 173 ) / 'OH              ' /




      LOGICAL   :: HALOGEN_PARAMETER = .TRUE. 


! The below character and integer arrays list the model species names used in the 
! chemical mechanism. The gas species and their order should agree with 
! the GC_SPC array for the gas phase chemistry to work correctly. 
! If present, the CHEMISTRY_SPC names and species type should agree with the CGRID_SPCS module


! MAPPED_TO_CGRID declares whether CMAQ namelists were used to determine 
! the below values of CGRID_INDEX, SPECIES_TYPE, SPECIES_MOLWT, and CONVERT_CONC
      LOGICAL, PARAMETER, PRIVATE :: F = .FALSE.
      LOGICAL, PARAMETER, PRIVATE :: T = .TRUE.


      LOGICAL   :: MAPPED_TO_CGRID   = .FALSE. 

      TYPE MEMBER
         CHARACTER( 16 ) :: CHEMISTRY_SPC
         INTEGER         :: CGRID_INDEX
         CHARACTER(  2 ) :: SPECIES_TYPE
         LOGICAL         :: CONVERT_CONC
         REAL            :: SPECIES_MOLWT
      END TYPE MEMBER
      TYPE( MEMBER ) ::  SPECIES_LIST( NUMB_MECH_SPC )  = (/ &
      & MEMBER("AXYL1J          ",  183, "AE",  174.00, T), &
      & MEMBER("AXYL2J          ",  184, "AE",  185.00, T), &
      & MEMBER("ATOL1J          ",  186, "AE",  163.00, T), &
      & MEMBER("ATOL2J          ",  187, "AE",  175.00, T), &
      & MEMBER("ABNZ1J          ",  189, "AE",  161.00, T), &
      & MEMBER("ABNZ2J          ",  190, "AE",  134.00, T), &
      & MEMBER("ATRP1J          ",  195, "AE",  177.00, T), &
      & MEMBER("ATRP2J          ",  196, "AE",  198.00, T), &
      & MEMBER("AISO1J          ",  197, "AE",  132.00, T), &
      & MEMBER("AISO2J          ",  198, "AE",  133.00, T), &
      & MEMBER("ASQTJ           ",  199, "AE",  273.00, T), &
      & MEMBER("APAH1J          ",  192, "AE",  195.60, T), &
      & MEMBER("APAH2J          ",  193, "AE",  178.70, T), &
      & MEMBER("AALK1J          ",  181, "AE",  225.00, T), &
      & MEMBER("AALK2J          ",  182, "AE",  205.10, T), &
      & MEMBER("AISO3J          ",  236, "AE",  168.20, T), &
      & MEMBER("O1D             ",    8, "GC",   16.00, F), &
      & MEMBER("SO2             ",   16, "GC",   64.10, F), &
      & MEMBER("SULF            ",   17, "GC",   98.10, F), &
      & MEMBER("SULRXN          ",   18, "GC",   98.10, F), &
      & MEMBER("BACL            ",   83, "GC",   86.10, F), &
      & MEMBER("ISOPRXN         ",  104, "GC",   68.00, F), &
      & MEMBER("BENZENE         ",  108, "GC",   78.10, F), &
      & MEMBER("SOAALK          ",  123, "GC",  112.00, F), &
      & MEMBER("ALKRXN          ",  124, "GC",  112.00, F), &
      & MEMBER("CLACET          ",  147, "GC",   92.50, F), &
      & MEMBER("CLCHO           ",  148, "GC",   64.50, F), &
      & MEMBER("BNZNRXN         ",  149, "GC",  159.10, F), &
      & MEMBER("BNZHRXN         ",  150, "GC",  159.10, F), &
      & MEMBER("XYLNRXN         ",  151, "GC",  187.20, F), &
      & MEMBER("XYLHRXN         ",  152, "GC",  187.20, F), &
      & MEMBER("TOLNRXN         ",  153, "GC",  172.10, F), &
      & MEMBER("TOLHRXN         ",  154, "GC",  172.10, F), &
      & MEMBER("PAHNRXN         ",  155, "GC",  172.10, F), &
      & MEMBER("PAHHRXN         ",  156, "GC",  172.10, F), &
      & MEMBER("ACLI            ",  228, "AE",   35.50, T), &
      & MEMBER("ACLJ            ",  227, "AE",   35.50, T), &
      & MEMBER("ACLK            ",  230, "AE",   35.50, T), &
      & MEMBER("PCVOC           ",  172, "GC",  170.00, F), &
      & MEMBER("PCSOARXN        ",  173, "GC",  170.00, F), &
      & MEMBER("N2O5            ",    6, "GC",  108.00, F), &
      & MEMBER("HO2H            ",   15, "GC",   34.00, F), &
      & MEMBER("PAN             ",   26, "GC",  121.10, F), &
      & MEMBER("PAN2            ",   30, "GC",  135.10, F), &
      & MEMBER("PBZN            ",   37, "GC",  183.10, F), &
      & MEMBER("MAPAN           ",   40, "GC",  147.10, F), &
      & MEMBER("TBUO            ",   41, "GC",   73.00, F), &
      & MEMBER("ISOPOOH         ",   66, "GC",  118.20, F), &
      & MEMBER("IEPOX           ",   67, "GC",  118.13, F), &
      & MEMBER("TOLUENE         ",  110, "GC",   92.10, F), &
      & MEMBER("MXYL            ",  112, "GC",  106.20, F), &
      & MEMBER("OXYL            ",  114, "GC",  106.20, F), &
      & MEMBER("PXYL            ",  115, "GC",  106.20, F), &
      & MEMBER("TMBENZ124       ",  116, "GC",  120.20, F), &
      & MEMBER("ETOH            ",  117, "GC",   46.10, F), &
      & MEMBER("ALK1            ",  118, "GC",   30.10, F), &
      & MEMBER("ALK2            ",  119, "GC",   36.70, F), &
      & MEMBER("ALK3            ",  120, "GC",   58.60, F), &
      & MEMBER("ALK4            ",  121, "GC",   77.60, F), &
      & MEMBER("ALK5            ",  122, "GC",  118.90, F), &
      & MEMBER("ARO1            ",  127, "GC",   95.20, F), &
      & MEMBER("NAPHTHAL        ",  129, "GC",  118.70, F), &
      & MEMBER("CLNO            ",  136, "GC",   65.50, F), &
      & MEMBER("CLONO           ",  137, "GC",   81.50, F), &
      & MEMBER("HOCL            ",  142, "GC",   52.50, F), &
      & MEMBER("H2NO3PK         ",  161, "GC",   64.00, F), &
      & MEMBER("VIVPO1          ",  166, "GC",  266.00, F), &
      & MEMBER("HNO4            ",   14, "GC",   79.00, F), &
      & MEMBER("COOH            ",   21, "GC",   48.00, F), &
      & MEMBER("CCOOOH          ",   27, "GC",   76.00, F), &
      & MEMBER("NPHE            ",   44, "GC",  139.10, F), &
      & MEMBER("xTBUO           ",   52, "GC",   73.00, F), &
      & MEMBER("ROOH            ",   63, "GC",   76.10, F), &
      & MEMBER("R6OOH           ",   65, "GC",  118.20, F), &
      & MEMBER("RAOOH           ",   72, "GC",  188.20, F), &
      & MEMBER("yISOPOOH        ",   71, "GC",  118.20, F), &
      & MEMBER("ARO2MN          ",  128, "GC",  118.70, F), &
      & MEMBER("ACETYLENE       ",  107, "GC",   26.00, F), &
      & MEMBER("CL2             ",  134, "GC",   70.00, F), &
      & MEMBER("CLONO2          ",  141, "GC",   97.50, F), &
      & MEMBER("CLCCHO          ",  146, "GC",   78.50, F), &
      & MEMBER("HCHO_PRIMARY    ",  157, "GC",   30.00, F), &
      & MEMBER("CCHO_PRIMARY    ",  158, "GC",   44.10, F), &
      & MEMBER("H2NO3PIJ        ",  160, "GC",   64.00, F), &
      & MEMBER("HONO            ",   10, "GC",   47.00, F), &
      & MEMBER("BALD            ",   84, "GC",  106.10, F), &
      & MEMBER("BENZRO2         ",  109, "GC",  159.10, F), &
      & MEMBER("PAHRO2          ",  130, "GC",  187.20, F), &
      & MEMBER("SESQRXN         ",  133, "GC",  204.40, F), &
      & MEMBER("AOLGBJ          ",  238, "AE",  248.00, T), &
      & MEMBER("VSVPO3          ",  165, "GC",  253.00, F), &
      & MEMBER("RCOOOH          ",   34, "GC",   74.10, F), &
      & MEMBER("ETHENE          ",  100, "GC",   28.10, F), &
      & MEMBER("PROPENE         ",  101, "GC",   42.10, F), &
      & MEMBER("BUTADIENE13     ",  102, "GC",   54.10, F), &
      & MEMBER("ISOPRENE        ",  103, "GC",   68.10, F), &
      & MEMBER("APIN            ",  105, "GC",  136.20, F), &
      & MEMBER("TRPRXN          ",  106, "GC",  136.20, F), &
      & MEMBER("TOLRO2          ",  111, "GC",  172.10, F), &
      & MEMBER("OLE1            ",  125, "GC",   72.30, F), &
      & MEMBER("OLE2            ",  126, "GC",   75.80, F), &
      & MEMBER("TERP            ",  131, "GC",  136.20, F), &
      & MEMBER("SESQ            ",  132, "GC",  204.40, F), &
      & MEMBER("ACRO_PRIMARY    ",  159, "GC",   56.10, F), &
      & MEMBER("VLVPO1          ",  162, "GC",  218.00, F), &
      & MEMBER("VSVPO1          ",  163, "GC",  230.00, F), &
      & MEMBER("VSVPO2          ",  164, "GC",  241.00, F), &
      & MEMBER("VSVOO1          ",  169, "GC",  135.00, F), &
      & MEMBER("MEOH            ",   22, "GC",   32.00, F), &
      & MEMBER("RNO3            ",   42, "GC",  147.20, F), &
      & MEMBER("ACETONE         ",   43, "GC",   58.10, F), &
      & MEMBER("xMEO2           ",   48, "GC",   47.00, F), &
      & MEMBER("HCOCO3          ",   82, "GC",   89.00, F), &
      & MEMBER("xACROLEIN       ",   99, "GC",   56.10, F), &
      & MEMBER("xCLACET         ",  145, "GC",   92.50, F), &
      & MEMBER("VSVOO2          ",  170, "GC",  135.00, F), &
      & MEMBER("VSVOO3          ",  171, "GC",  134.00, F), &
      & MEMBER("xOH             ",   46, "GC",   17.00, F), &
      & MEMBER("AFG1            ",   80, "GC",   98.10, F), &
      & MEMBER("AFG2            ",   81, "GC",   98.10, F), &
      & MEMBER("ACROLEIN        ",   93, "GC",   56.10, F), &
      & MEMBER("yRAOOH          ",   98, "GC",  188.20, F), &
      & MEMBER("IEPOXOO         ",   68, "GC",  149.12, F), &
      & MEMBER("CLNO2           ",  138, "GC",   81.50, F), &
      & MEMBER("CLO             ",  140, "GC",   51.50, F), &
      & MEMBER("CCOOH           ",   28, "GC",   60.10, F), &
      & MEMBER("xMACO3          ",   51, "GC",  101.10, F), &
      & MEMBER("IPRD            ",   74, "GC",  100.10, F), &
      & MEMBER("XYLRO2          ",  113, "GC",  187.20, F), &
      & MEMBER("MACR            ",   87, "GC",   70.10, F), &
      & MEMBER("MVK             ",   88, "GC",   70.10, F), &
      & MEMBER("xAFG3           ",   95, "GC",  124.70, F), &
      & MEMBER("AOLGAJ          ",  237, "AE",  206.00, T), &
      & MEMBER("xNO2            ",   47, "GC",   46.00, F), &
      & MEMBER("xHOCCHO         ",   89, "GC",   60.10, F), &
      & MEMBER("HOCCHO          ",   91, "GC",   60.10, F), &
      & MEMBER("xBALD           ",   94, "GC",  106.10, F), &
      & MEMBER("xMACR           ",   96, "GC",   70.10, F), &
      & MEMBER("xCL             ",  143, "GC",   35.50, F), &
      & MEMBER("VLVOO1          ",  167, "GC",  136.00, F), &
      & MEMBER("VLVOO2          ",  168, "GC",  136.00, F), &
      & MEMBER("RCOOH           ",   35, "GC",   74.10, F), &
      & MEMBER("xMVK            ",   97, "GC",   70.10, F), &
      & MEMBER("CCHO            ",   54, "GC",   44.10, F), &
      & MEMBER("xBACL           ",   62, "GC",   86.10, F), &
      & MEMBER("AFG3            ",   85, "GC",  124.10, F), &
      & MEMBER("xCLCCHO         ",  144, "GC",   78.50, F), &
      & MEMBER("xRNO3           ",   90, "GC",  147.20, F), &
      & MEMBER("BZCO3           ",   36, "GC",  137.10, F), &
      & MEMBER("CRES            ",   45, "GC",  108.10, F), &
      & MEMBER("xIPRD           ",   86, "GC",  100.10, F), &
      & MEMBER("xACETONE        ",   92, "GC",   58.10, F), &
      & MEMBER("BZO             ",   38, "GC",   93.00, F), &
      & MEMBER("xMECO3          ",   49, "GC",   75.00, F), &
      & MEMBER("xRCO3           ",   50, "GC",   89.10, F), &
      & MEMBER("xPROD2          ",   64, "GC",  116.20, F), &
      & MEMBER("O3P             ",    3, "GC",   16.00, F), &
      & MEMBER("HNO3            ",    7, "GC",   63.00, F), &
      & MEMBER("xCO             ",   53, "GC",   28.00, F), &
      & MEMBER("MGLY            ",   73, "GC",   72.10, F), &
      & MEMBER("xMEK            ",   76, "GC",   72.10, F), &
      & MEMBER("RCHO            ",   55, "GC",   58.10, F), &
      & MEMBER("xAFG1           ",   77, "GC",   98.10, F), &
      & MEMBER("xAFG2           ",   78, "GC",   98.10, F), &
      & MEMBER("GLY             ",   79, "GC",   58.00, F), &
      & MEMBER("RCO3            ",   29, "GC",   89.10, F), &
      & MEMBER("MACO3           ",   39, "GC",  101.10, F), &
      & MEMBER("MEK             ",   57, "GC",   72.10, F), &
      & MEMBER("HCOOH           ",   60, "GC",   46.00, F), &
      & MEMBER("NO              ",    2, "GC",   30.00, F), &
      & MEMBER("xGLY            ",   75, "GC",   58.00, F), &
      & MEMBER("xMGLY           ",   61, "GC",   72.10, F), &
      & MEMBER("PRD2            ",   69, "GC",  116.20, F), &
      & MEMBER("xRCHO           ",   59, "GC",   58.10, F), &
      & MEMBER("O3              ",    4, "GC",   48.00, F), &
      & MEMBER("MEO2            ",   19, "GC",   47.00, F), &
      & MEMBER("MECO3           ",   25, "GC",   75.00, F), &
      & MEMBER("xCCHO           ",   33, "GC",   44.10, F), &
      & MEMBER("yR6OOH          ",   70, "GC",  118.20, F), &
      & MEMBER("NO2             ",    1, "GC",   46.00, F), &
      & MEMBER("xHCHO           ",   56, "GC",   30.00, F), &
      & MEMBER("HCHO            ",   20, "GC",   30.00, F), &
      & MEMBER("HCL             ",  139, "GC",   36.50, F), &
      & MEMBER("CO              ",   12, "GC",   28.00, F), &
      & MEMBER("yROOH           ",   32, "GC",   76.10, F), &
      & MEMBER("zRNO3           ",   58, "GC",  147.20, F), &
      & MEMBER("CO2             ",   13, "GC",   44.00, F), &
      & MEMBER("NO3             ",    5, "GC",   62.00, F), &
      & MEMBER("RO2XC           ",   24, "GC",    1.00, F), &
      & MEMBER("xHO2            ",   31, "GC",   33.00, F), &
      & MEMBER("CL              ",  135, "GC",   35.50, F), &
      & MEMBER("RO2C            ",   23, "GC",    1.00, F), &
      & MEMBER("HO2             ",   11, "GC",   33.00, F), &
      & MEMBER("OH              ",    9, "GC",   17.00, F) /)


      DATA CHEMISTRY_SPC(   1 ), SPECIES_MOLWT(   1 ) / 'AXYL1J          ',  174.00 /
      DATA CHEMISTRY_SPC(   2 ), SPECIES_MOLWT(   2 ) / 'AXYL2J          ',  185.00 /
      DATA CHEMISTRY_SPC(   3 ), SPECIES_MOLWT(   3 ) / 'ATOL1J          ',  163.00 /
      DATA CHEMISTRY_SPC(   4 ), SPECIES_MOLWT(   4 ) / 'ATOL2J          ',  175.00 /
      DATA CHEMISTRY_SPC(   5 ), SPECIES_MOLWT(   5 ) / 'ABNZ1J          ',  161.00 /
      DATA CHEMISTRY_SPC(   6 ), SPECIES_MOLWT(   6 ) / 'ABNZ2J          ',  134.00 /
      DATA CHEMISTRY_SPC(   7 ), SPECIES_MOLWT(   7 ) / 'ATRP1J          ',  177.00 /
      DATA CHEMISTRY_SPC(   8 ), SPECIES_MOLWT(   8 ) / 'ATRP2J          ',  198.00 /
      DATA CHEMISTRY_SPC(   9 ), SPECIES_MOLWT(   9 ) / 'AISO1J          ',  132.00 /
      DATA CHEMISTRY_SPC(  10 ), SPECIES_MOLWT(  10 ) / 'AISO2J          ',  133.00 /
      DATA CHEMISTRY_SPC(  11 ), SPECIES_MOLWT(  11 ) / 'ASQTJ           ',  273.00 /
      DATA CHEMISTRY_SPC(  12 ), SPECIES_MOLWT(  12 ) / 'APAH1J          ',  195.60 /
      DATA CHEMISTRY_SPC(  13 ), SPECIES_MOLWT(  13 ) / 'APAH2J          ',  178.70 /
      DATA CHEMISTRY_SPC(  14 ), SPECIES_MOLWT(  14 ) / 'AALK1J          ',  225.00 /
      DATA CHEMISTRY_SPC(  15 ), SPECIES_MOLWT(  15 ) / 'AALK2J          ',  205.10 /
      DATA CHEMISTRY_SPC(  16 ), SPECIES_MOLWT(  16 ) / 'AISO3J          ',  168.20 /
      DATA CHEMISTRY_SPC(  17 ), SPECIES_MOLWT(  17 ) / 'O1D             ',   16.00 /
      DATA CHEMISTRY_SPC(  18 ), SPECIES_MOLWT(  18 ) / 'SO2             ',   64.10 /
      DATA CHEMISTRY_SPC(  19 ), SPECIES_MOLWT(  19 ) / 'SULF            ',   98.10 /
      DATA CHEMISTRY_SPC(  20 ), SPECIES_MOLWT(  20 ) / 'SULRXN          ',   98.10 /
      DATA CHEMISTRY_SPC(  21 ), SPECIES_MOLWT(  21 ) / 'BACL            ',   86.10 /
      DATA CHEMISTRY_SPC(  22 ), SPECIES_MOLWT(  22 ) / 'ISOPRXN         ',   68.00 /
      DATA CHEMISTRY_SPC(  23 ), SPECIES_MOLWT(  23 ) / 'BENZENE         ',   78.10 /
      DATA CHEMISTRY_SPC(  24 ), SPECIES_MOLWT(  24 ) / 'SOAALK          ',  112.00 /
      DATA CHEMISTRY_SPC(  25 ), SPECIES_MOLWT(  25 ) / 'ALKRXN          ',  112.00 /
      DATA CHEMISTRY_SPC(  26 ), SPECIES_MOLWT(  26 ) / 'CLACET          ',   92.50 /
      DATA CHEMISTRY_SPC(  27 ), SPECIES_MOLWT(  27 ) / 'CLCHO           ',   64.50 /
      DATA CHEMISTRY_SPC(  28 ), SPECIES_MOLWT(  28 ) / 'BNZNRXN         ',  159.10 /
      DATA CHEMISTRY_SPC(  29 ), SPECIES_MOLWT(  29 ) / 'BNZHRXN         ',  159.10 /
      DATA CHEMISTRY_SPC(  30 ), SPECIES_MOLWT(  30 ) / 'XYLNRXN         ',  187.20 /
      DATA CHEMISTRY_SPC(  31 ), SPECIES_MOLWT(  31 ) / 'XYLHRXN         ',  187.20 /
      DATA CHEMISTRY_SPC(  32 ), SPECIES_MOLWT(  32 ) / 'TOLNRXN         ',  172.10 /
      DATA CHEMISTRY_SPC(  33 ), SPECIES_MOLWT(  33 ) / 'TOLHRXN         ',  172.10 /
      DATA CHEMISTRY_SPC(  34 ), SPECIES_MOLWT(  34 ) / 'PAHNRXN         ',  172.10 /
      DATA CHEMISTRY_SPC(  35 ), SPECIES_MOLWT(  35 ) / 'PAHHRXN         ',  172.10 /
      DATA CHEMISTRY_SPC(  36 ), SPECIES_MOLWT(  36 ) / 'ACLI            ',   35.50 /
      DATA CHEMISTRY_SPC(  37 ), SPECIES_MOLWT(  37 ) / 'ACLJ            ',   35.50 /
      DATA CHEMISTRY_SPC(  38 ), SPECIES_MOLWT(  38 ) / 'ACLK            ',   35.50 /
      DATA CHEMISTRY_SPC(  39 ), SPECIES_MOLWT(  39 ) / 'PCVOC           ',  170.00 /
      DATA CHEMISTRY_SPC(  40 ), SPECIES_MOLWT(  40 ) / 'PCSOARXN        ',  170.00 /
      DATA CHEMISTRY_SPC(  41 ), SPECIES_MOLWT(  41 ) / 'N2O5            ',  108.00 /
      DATA CHEMISTRY_SPC(  42 ), SPECIES_MOLWT(  42 ) / 'HO2H            ',   34.00 /
      DATA CHEMISTRY_SPC(  43 ), SPECIES_MOLWT(  43 ) / 'PAN             ',  121.10 /
      DATA CHEMISTRY_SPC(  44 ), SPECIES_MOLWT(  44 ) / 'PAN2            ',  135.10 /
      DATA CHEMISTRY_SPC(  45 ), SPECIES_MOLWT(  45 ) / 'PBZN            ',  183.10 /
      DATA CHEMISTRY_SPC(  46 ), SPECIES_MOLWT(  46 ) / 'MAPAN           ',  147.10 /
      DATA CHEMISTRY_SPC(  47 ), SPECIES_MOLWT(  47 ) / 'TBUO            ',   73.00 /
      DATA CHEMISTRY_SPC(  48 ), SPECIES_MOLWT(  48 ) / 'ISOPOOH         ',  118.20 /
      DATA CHEMISTRY_SPC(  49 ), SPECIES_MOLWT(  49 ) / 'IEPOX           ',  118.13 /
      DATA CHEMISTRY_SPC(  50 ), SPECIES_MOLWT(  50 ) / 'TOLUENE         ',   92.10 /
      DATA CHEMISTRY_SPC(  51 ), SPECIES_MOLWT(  51 ) / 'MXYL            ',  106.20 /
      DATA CHEMISTRY_SPC(  52 ), SPECIES_MOLWT(  52 ) / 'OXYL            ',  106.20 /
      DATA CHEMISTRY_SPC(  53 ), SPECIES_MOLWT(  53 ) / 'PXYL            ',  106.20 /
      DATA CHEMISTRY_SPC(  54 ), SPECIES_MOLWT(  54 ) / 'TMBENZ124       ',  120.20 /
      DATA CHEMISTRY_SPC(  55 ), SPECIES_MOLWT(  55 ) / 'ETOH            ',   46.10 /
      DATA CHEMISTRY_SPC(  56 ), SPECIES_MOLWT(  56 ) / 'ALK1            ',   30.10 /
      DATA CHEMISTRY_SPC(  57 ), SPECIES_MOLWT(  57 ) / 'ALK2            ',   36.70 /
      DATA CHEMISTRY_SPC(  58 ), SPECIES_MOLWT(  58 ) / 'ALK3            ',   58.60 /
      DATA CHEMISTRY_SPC(  59 ), SPECIES_MOLWT(  59 ) / 'ALK4            ',   77.60 /
      DATA CHEMISTRY_SPC(  60 ), SPECIES_MOLWT(  60 ) / 'ALK5            ',  118.90 /
      DATA CHEMISTRY_SPC(  61 ), SPECIES_MOLWT(  61 ) / 'ARO1            ',   95.20 /
      DATA CHEMISTRY_SPC(  62 ), SPECIES_MOLWT(  62 ) / 'NAPHTHAL        ',  118.70 /
      DATA CHEMISTRY_SPC(  63 ), SPECIES_MOLWT(  63 ) / 'CLNO            ',   65.50 /
      DATA CHEMISTRY_SPC(  64 ), SPECIES_MOLWT(  64 ) / 'CLONO           ',   81.50 /
      DATA CHEMISTRY_SPC(  65 ), SPECIES_MOLWT(  65 ) / 'HOCL            ',   52.50 /
      DATA CHEMISTRY_SPC(  66 ), SPECIES_MOLWT(  66 ) / 'H2NO3PK         ',   64.00 /
      DATA CHEMISTRY_SPC(  67 ), SPECIES_MOLWT(  67 ) / 'VIVPO1          ',  266.00 /
      DATA CHEMISTRY_SPC(  68 ), SPECIES_MOLWT(  68 ) / 'HNO4            ',   79.00 /
      DATA CHEMISTRY_SPC(  69 ), SPECIES_MOLWT(  69 ) / 'COOH            ',   48.00 /
      DATA CHEMISTRY_SPC(  70 ), SPECIES_MOLWT(  70 ) / 'CCOOOH          ',   76.00 /
      DATA CHEMISTRY_SPC(  71 ), SPECIES_MOLWT(  71 ) / 'NPHE            ',  139.10 /
      DATA CHEMISTRY_SPC(  72 ), SPECIES_MOLWT(  72 ) / 'xTBUO           ',   73.00 /
      DATA CHEMISTRY_SPC(  73 ), SPECIES_MOLWT(  73 ) / 'ROOH            ',   76.10 /
      DATA CHEMISTRY_SPC(  74 ), SPECIES_MOLWT(  74 ) / 'R6OOH           ',  118.20 /
      DATA CHEMISTRY_SPC(  75 ), SPECIES_MOLWT(  75 ) / 'RAOOH           ',  188.20 /
      DATA CHEMISTRY_SPC(  76 ), SPECIES_MOLWT(  76 ) / 'yISOPOOH        ',  118.20 /
      DATA CHEMISTRY_SPC(  77 ), SPECIES_MOLWT(  77 ) / 'ARO2MN          ',  118.70 /
      DATA CHEMISTRY_SPC(  78 ), SPECIES_MOLWT(  78 ) / 'ACETYLENE       ',   26.00 /
      DATA CHEMISTRY_SPC(  79 ), SPECIES_MOLWT(  79 ) / 'CL2             ',   70.00 /
      DATA CHEMISTRY_SPC(  80 ), SPECIES_MOLWT(  80 ) / 'CLONO2          ',   97.50 /
      DATA CHEMISTRY_SPC(  81 ), SPECIES_MOLWT(  81 ) / 'CLCCHO          ',   78.50 /
      DATA CHEMISTRY_SPC(  82 ), SPECIES_MOLWT(  82 ) / 'HCHO_PRIMARY    ',   30.00 /
      DATA CHEMISTRY_SPC(  83 ), SPECIES_MOLWT(  83 ) / 'CCHO_PRIMARY    ',   44.10 /
      DATA CHEMISTRY_SPC(  84 ), SPECIES_MOLWT(  84 ) / 'H2NO3PIJ        ',   64.00 /
      DATA CHEMISTRY_SPC(  85 ), SPECIES_MOLWT(  85 ) / 'HONO            ',   47.00 /
      DATA CHEMISTRY_SPC(  86 ), SPECIES_MOLWT(  86 ) / 'BALD            ',  106.10 /
      DATA CHEMISTRY_SPC(  87 ), SPECIES_MOLWT(  87 ) / 'BENZRO2         ',  159.10 /
      DATA CHEMISTRY_SPC(  88 ), SPECIES_MOLWT(  88 ) / 'PAHRO2          ',  187.20 /
      DATA CHEMISTRY_SPC(  89 ), SPECIES_MOLWT(  89 ) / 'SESQRXN         ',  204.40 /
      DATA CHEMISTRY_SPC(  90 ), SPECIES_MOLWT(  90 ) / 'AOLGBJ          ',  248.00 /
      DATA CHEMISTRY_SPC(  91 ), SPECIES_MOLWT(  91 ) / 'VSVPO3          ',  253.00 /
      DATA CHEMISTRY_SPC(  92 ), SPECIES_MOLWT(  92 ) / 'RCOOOH          ',   74.10 /
      DATA CHEMISTRY_SPC(  93 ), SPECIES_MOLWT(  93 ) / 'ETHENE          ',   28.10 /
      DATA CHEMISTRY_SPC(  94 ), SPECIES_MOLWT(  94 ) / 'PROPENE         ',   42.10 /
      DATA CHEMISTRY_SPC(  95 ), SPECIES_MOLWT(  95 ) / 'BUTADIENE13     ',   54.10 /
      DATA CHEMISTRY_SPC(  96 ), SPECIES_MOLWT(  96 ) / 'ISOPRENE        ',   68.10 /
      DATA CHEMISTRY_SPC(  97 ), SPECIES_MOLWT(  97 ) / 'APIN            ',  136.20 /
      DATA CHEMISTRY_SPC(  98 ), SPECIES_MOLWT(  98 ) / 'TRPRXN          ',  136.20 /
      DATA CHEMISTRY_SPC(  99 ), SPECIES_MOLWT(  99 ) / 'TOLRO2          ',  172.10 /
      DATA CHEMISTRY_SPC( 100 ), SPECIES_MOLWT( 100 ) / 'OLE1            ',   72.30 /
      DATA CHEMISTRY_SPC( 101 ), SPECIES_MOLWT( 101 ) / 'OLE2            ',   75.80 /
      DATA CHEMISTRY_SPC( 102 ), SPECIES_MOLWT( 102 ) / 'TERP            ',  136.20 /
      DATA CHEMISTRY_SPC( 103 ), SPECIES_MOLWT( 103 ) / 'SESQ            ',  204.40 /
      DATA CHEMISTRY_SPC( 104 ), SPECIES_MOLWT( 104 ) / 'ACRO_PRIMARY    ',   56.10 /
      DATA CHEMISTRY_SPC( 105 ), SPECIES_MOLWT( 105 ) / 'VLVPO1          ',  218.00 /
      DATA CHEMISTRY_SPC( 106 ), SPECIES_MOLWT( 106 ) / 'VSVPO1          ',  230.00 /
      DATA CHEMISTRY_SPC( 107 ), SPECIES_MOLWT( 107 ) / 'VSVPO2          ',  241.00 /
      DATA CHEMISTRY_SPC( 108 ), SPECIES_MOLWT( 108 ) / 'VSVOO1          ',  135.00 /
      DATA CHEMISTRY_SPC( 109 ), SPECIES_MOLWT( 109 ) / 'MEOH            ',   32.00 /
      DATA CHEMISTRY_SPC( 110 ), SPECIES_MOLWT( 110 ) / 'RNO3            ',  147.20 /
      DATA CHEMISTRY_SPC( 111 ), SPECIES_MOLWT( 111 ) / 'ACETONE         ',   58.10 /
      DATA CHEMISTRY_SPC( 112 ), SPECIES_MOLWT( 112 ) / 'xMEO2           ',   47.00 /
      DATA CHEMISTRY_SPC( 113 ), SPECIES_MOLWT( 113 ) / 'HCOCO3          ',   89.00 /
      DATA CHEMISTRY_SPC( 114 ), SPECIES_MOLWT( 114 ) / 'xACROLEIN       ',   56.10 /
      DATA CHEMISTRY_SPC( 115 ), SPECIES_MOLWT( 115 ) / 'xCLACET         ',   92.50 /
      DATA CHEMISTRY_SPC( 116 ), SPECIES_MOLWT( 116 ) / 'VSVOO2          ',  135.00 /
      DATA CHEMISTRY_SPC( 117 ), SPECIES_MOLWT( 117 ) / 'VSVOO3          ',  134.00 /
      DATA CHEMISTRY_SPC( 118 ), SPECIES_MOLWT( 118 ) / 'xOH             ',   17.00 /
      DATA CHEMISTRY_SPC( 119 ), SPECIES_MOLWT( 119 ) / 'AFG1            ',   98.10 /
      DATA CHEMISTRY_SPC( 120 ), SPECIES_MOLWT( 120 ) / 'AFG2            ',   98.10 /
      DATA CHEMISTRY_SPC( 121 ), SPECIES_MOLWT( 121 ) / 'ACROLEIN        ',   56.10 /
      DATA CHEMISTRY_SPC( 122 ), SPECIES_MOLWT( 122 ) / 'yRAOOH          ',  188.20 /
      DATA CHEMISTRY_SPC( 123 ), SPECIES_MOLWT( 123 ) / 'IEPOXOO         ',  149.12 /
      DATA CHEMISTRY_SPC( 124 ), SPECIES_MOLWT( 124 ) / 'CLNO2           ',   81.50 /
      DATA CHEMISTRY_SPC( 125 ), SPECIES_MOLWT( 125 ) / 'CLO             ',   51.50 /
      DATA CHEMISTRY_SPC( 126 ), SPECIES_MOLWT( 126 ) / 'CCOOH           ',   60.10 /
      DATA CHEMISTRY_SPC( 127 ), SPECIES_MOLWT( 127 ) / 'xMACO3          ',  101.10 /
      DATA CHEMISTRY_SPC( 128 ), SPECIES_MOLWT( 128 ) / 'IPRD            ',  100.10 /
      DATA CHEMISTRY_SPC( 129 ), SPECIES_MOLWT( 129 ) / 'XYLRO2          ',  187.20 /
      DATA CHEMISTRY_SPC( 130 ), SPECIES_MOLWT( 130 ) / 'MACR            ',   70.10 /
      DATA CHEMISTRY_SPC( 131 ), SPECIES_MOLWT( 131 ) / 'MVK             ',   70.10 /
      DATA CHEMISTRY_SPC( 132 ), SPECIES_MOLWT( 132 ) / 'xAFG3           ',  124.70 /
      DATA CHEMISTRY_SPC( 133 ), SPECIES_MOLWT( 133 ) / 'AOLGAJ          ',  206.00 /
      DATA CHEMISTRY_SPC( 134 ), SPECIES_MOLWT( 134 ) / 'xNO2            ',   46.00 /
      DATA CHEMISTRY_SPC( 135 ), SPECIES_MOLWT( 135 ) / 'xHOCCHO         ',   60.10 /
      DATA CHEMISTRY_SPC( 136 ), SPECIES_MOLWT( 136 ) / 'HOCCHO          ',   60.10 /
      DATA CHEMISTRY_SPC( 137 ), SPECIES_MOLWT( 137 ) / 'xBALD           ',  106.10 /
      DATA CHEMISTRY_SPC( 138 ), SPECIES_MOLWT( 138 ) / 'xMACR           ',   70.10 /
      DATA CHEMISTRY_SPC( 139 ), SPECIES_MOLWT( 139 ) / 'xCL             ',   35.50 /
      DATA CHEMISTRY_SPC( 140 ), SPECIES_MOLWT( 140 ) / 'VLVOO1          ',  136.00 /
      DATA CHEMISTRY_SPC( 141 ), SPECIES_MOLWT( 141 ) / 'VLVOO2          ',  136.00 /
      DATA CHEMISTRY_SPC( 142 ), SPECIES_MOLWT( 142 ) / 'RCOOH           ',   74.10 /
      DATA CHEMISTRY_SPC( 143 ), SPECIES_MOLWT( 143 ) / 'xMVK            ',   70.10 /
      DATA CHEMISTRY_SPC( 144 ), SPECIES_MOLWT( 144 ) / 'CCHO            ',   44.10 /
      DATA CHEMISTRY_SPC( 145 ), SPECIES_MOLWT( 145 ) / 'xBACL           ',   86.10 /
      DATA CHEMISTRY_SPC( 146 ), SPECIES_MOLWT( 146 ) / 'AFG3            ',  124.10 /
      DATA CHEMISTRY_SPC( 147 ), SPECIES_MOLWT( 147 ) / 'xCLCCHO         ',   78.50 /
      DATA CHEMISTRY_SPC( 148 ), SPECIES_MOLWT( 148 ) / 'xRNO3           ',  147.20 /
      DATA CHEMISTRY_SPC( 149 ), SPECIES_MOLWT( 149 ) / 'BZCO3           ',  137.10 /
      DATA CHEMISTRY_SPC( 150 ), SPECIES_MOLWT( 150 ) / 'CRES            ',  108.10 /
      DATA CHEMISTRY_SPC( 151 ), SPECIES_MOLWT( 151 ) / 'xIPRD           ',  100.10 /
      DATA CHEMISTRY_SPC( 152 ), SPECIES_MOLWT( 152 ) / 'xACETONE        ',   58.10 /
      DATA CHEMISTRY_SPC( 153 ), SPECIES_MOLWT( 153 ) / 'BZO             ',   93.00 /
      DATA CHEMISTRY_SPC( 154 ), SPECIES_MOLWT( 154 ) / 'xMECO3          ',   75.00 /
      DATA CHEMISTRY_SPC( 155 ), SPECIES_MOLWT( 155 ) / 'xRCO3           ',   89.10 /
      DATA CHEMISTRY_SPC( 156 ), SPECIES_MOLWT( 156 ) / 'xPROD2          ',  116.20 /
      DATA CHEMISTRY_SPC( 157 ), SPECIES_MOLWT( 157 ) / 'O3P             ',   16.00 /
      DATA CHEMISTRY_SPC( 158 ), SPECIES_MOLWT( 158 ) / 'HNO3            ',   63.00 /
      DATA CHEMISTRY_SPC( 159 ), SPECIES_MOLWT( 159 ) / 'xCO             ',   28.00 /
      DATA CHEMISTRY_SPC( 160 ), SPECIES_MOLWT( 160 ) / 'MGLY            ',   72.10 /
      DATA CHEMISTRY_SPC( 161 ), SPECIES_MOLWT( 161 ) / 'xMEK            ',   72.10 /
      DATA CHEMISTRY_SPC( 162 ), SPECIES_MOLWT( 162 ) / 'RCHO            ',   58.10 /
      DATA CHEMISTRY_SPC( 163 ), SPECIES_MOLWT( 163 ) / 'xAFG1           ',   98.10 /
      DATA CHEMISTRY_SPC( 164 ), SPECIES_MOLWT( 164 ) / 'xAFG2           ',   98.10 /
      DATA CHEMISTRY_SPC( 165 ), SPECIES_MOLWT( 165 ) / 'GLY             ',   58.00 /
      DATA CHEMISTRY_SPC( 166 ), SPECIES_MOLWT( 166 ) / 'RCO3            ',   89.10 /
      DATA CHEMISTRY_SPC( 167 ), SPECIES_MOLWT( 167 ) / 'MACO3           ',  101.10 /
      DATA CHEMISTRY_SPC( 168 ), SPECIES_MOLWT( 168 ) / 'MEK             ',   72.10 /
      DATA CHEMISTRY_SPC( 169 ), SPECIES_MOLWT( 169 ) / 'HCOOH           ',   46.00 /
      DATA CHEMISTRY_SPC( 170 ), SPECIES_MOLWT( 170 ) / 'NO              ',   30.00 /
      DATA CHEMISTRY_SPC( 171 ), SPECIES_MOLWT( 171 ) / 'xGLY            ',   58.00 /
      DATA CHEMISTRY_SPC( 172 ), SPECIES_MOLWT( 172 ) / 'xMGLY           ',   72.10 /
      DATA CHEMISTRY_SPC( 173 ), SPECIES_MOLWT( 173 ) / 'PRD2            ',  116.20 /
      DATA CHEMISTRY_SPC( 174 ), SPECIES_MOLWT( 174 ) / 'xRCHO           ',   58.10 /
      DATA CHEMISTRY_SPC( 175 ), SPECIES_MOLWT( 175 ) / 'O3              ',   48.00 /
      DATA CHEMISTRY_SPC( 176 ), SPECIES_MOLWT( 176 ) / 'MEO2            ',   47.00 /
      DATA CHEMISTRY_SPC( 177 ), SPECIES_MOLWT( 177 ) / 'MECO3           ',   75.00 /
      DATA CHEMISTRY_SPC( 178 ), SPECIES_MOLWT( 178 ) / 'xCCHO           ',   44.10 /
      DATA CHEMISTRY_SPC( 179 ), SPECIES_MOLWT( 179 ) / 'yR6OOH          ',  118.20 /
      DATA CHEMISTRY_SPC( 180 ), SPECIES_MOLWT( 180 ) / 'NO2             ',   46.00 /
      DATA CHEMISTRY_SPC( 181 ), SPECIES_MOLWT( 181 ) / 'xHCHO           ',   30.00 /
      DATA CHEMISTRY_SPC( 182 ), SPECIES_MOLWT( 182 ) / 'HCHO            ',   30.00 /
      DATA CHEMISTRY_SPC( 183 ), SPECIES_MOLWT( 183 ) / 'HCL             ',   36.50 /
      DATA CHEMISTRY_SPC( 184 ), SPECIES_MOLWT( 184 ) / 'CO              ',   28.00 /
      DATA CHEMISTRY_SPC( 185 ), SPECIES_MOLWT( 185 ) / 'yROOH           ',   76.10 /
      DATA CHEMISTRY_SPC( 186 ), SPECIES_MOLWT( 186 ) / 'zRNO3           ',  147.20 /
      DATA CHEMISTRY_SPC( 187 ), SPECIES_MOLWT( 187 ) / 'CO2             ',   44.00 /
      DATA CHEMISTRY_SPC( 188 ), SPECIES_MOLWT( 188 ) / 'NO3             ',   62.00 /
      DATA CHEMISTRY_SPC( 189 ), SPECIES_MOLWT( 189 ) / 'RO2XC           ',    1.00 /
      DATA CHEMISTRY_SPC( 190 ), SPECIES_MOLWT( 190 ) / 'xHO2            ',   33.00 /
      DATA CHEMISTRY_SPC( 191 ), SPECIES_MOLWT( 191 ) / 'CL              ',   35.50 /
      DATA CHEMISTRY_SPC( 192 ), SPECIES_MOLWT( 192 ) / 'RO2C            ',    1.00 /
      DATA CHEMISTRY_SPC( 193 ), SPECIES_MOLWT( 193 ) / 'HO2             ',   33.00 /
      DATA CHEMISTRY_SPC( 194 ), SPECIES_MOLWT( 194 ) / 'OH              ',   17.00 /


      DATA CGRID_INDEX(   1 ), SPECIES_TYPE(   1 ), CONVERT_CONC(   1 ) /  183, 'AE', T /  ! AXYL1J
      DATA CGRID_INDEX(   2 ), SPECIES_TYPE(   2 ), CONVERT_CONC(   2 ) /  184, 'AE', T /  ! AXYL2J
      DATA CGRID_INDEX(   3 ), SPECIES_TYPE(   3 ), CONVERT_CONC(   3 ) /  186, 'AE', T /  ! ATOL1J
      DATA CGRID_INDEX(   4 ), SPECIES_TYPE(   4 ), CONVERT_CONC(   4 ) /  187, 'AE', T /  ! ATOL2J
      DATA CGRID_INDEX(   5 ), SPECIES_TYPE(   5 ), CONVERT_CONC(   5 ) /  189, 'AE', T /  ! ABNZ1J
      DATA CGRID_INDEX(   6 ), SPECIES_TYPE(   6 ), CONVERT_CONC(   6 ) /  190, 'AE', T /  ! ABNZ2J
      DATA CGRID_INDEX(   7 ), SPECIES_TYPE(   7 ), CONVERT_CONC(   7 ) /  195, 'AE', T /  ! ATRP1J
      DATA CGRID_INDEX(   8 ), SPECIES_TYPE(   8 ), CONVERT_CONC(   8 ) /  196, 'AE', T /  ! ATRP2J
      DATA CGRID_INDEX(   9 ), SPECIES_TYPE(   9 ), CONVERT_CONC(   9 ) /  197, 'AE', T /  ! AISO1J
      DATA CGRID_INDEX(  10 ), SPECIES_TYPE(  10 ), CONVERT_CONC(  10 ) /  198, 'AE', T /  ! AISO2J
      DATA CGRID_INDEX(  11 ), SPECIES_TYPE(  11 ), CONVERT_CONC(  11 ) /  199, 'AE', T /  ! ASQTJ
      DATA CGRID_INDEX(  12 ), SPECIES_TYPE(  12 ), CONVERT_CONC(  12 ) /  192, 'AE', T /  ! APAH1J
      DATA CGRID_INDEX(  13 ), SPECIES_TYPE(  13 ), CONVERT_CONC(  13 ) /  193, 'AE', T /  ! APAH2J
      DATA CGRID_INDEX(  14 ), SPECIES_TYPE(  14 ), CONVERT_CONC(  14 ) /  181, 'AE', T /  ! AALK1J
      DATA CGRID_INDEX(  15 ), SPECIES_TYPE(  15 ), CONVERT_CONC(  15 ) /  182, 'AE', T /  ! AALK2J
      DATA CGRID_INDEX(  16 ), SPECIES_TYPE(  16 ), CONVERT_CONC(  16 ) /  236, 'AE', T /  ! AISO3J
      DATA CGRID_INDEX(  17 ), SPECIES_TYPE(  17 ), CONVERT_CONC(  17 ) /    8, 'GC', F /  ! O1D
      DATA CGRID_INDEX(  18 ), SPECIES_TYPE(  18 ), CONVERT_CONC(  18 ) /   16, 'GC', F /  ! SO2
      DATA CGRID_INDEX(  19 ), SPECIES_TYPE(  19 ), CONVERT_CONC(  19 ) /   17, 'GC', F /  ! SULF
      DATA CGRID_INDEX(  20 ), SPECIES_TYPE(  20 ), CONVERT_CONC(  20 ) /   18, 'GC', F /  ! SULRXN
      DATA CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), CONVERT_CONC(  21 ) /   83, 'GC', F /  ! BACL
      DATA CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), CONVERT_CONC(  22 ) /  104, 'GC', F /  ! ISOPRXN
      DATA CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), CONVERT_CONC(  23 ) /  108, 'GC', F /  ! BENZENE
      DATA CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), CONVERT_CONC(  24 ) /  123, 'GC', F /  ! SOAALK
      DATA CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), CONVERT_CONC(  25 ) /  124, 'GC', F /  ! ALKRXN
      DATA CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), CONVERT_CONC(  26 ) /  147, 'GC', F /  ! CLACET
      DATA CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), CONVERT_CONC(  27 ) /  148, 'GC', F /  ! CLCHO
      DATA CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), CONVERT_CONC(  28 ) /  149, 'GC', F /  ! BNZNRXN
      DATA CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), CONVERT_CONC(  29 ) /  150, 'GC', F /  ! BNZHRXN
      DATA CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), CONVERT_CONC(  30 ) /  151, 'GC', F /  ! XYLNRXN
      DATA CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), CONVERT_CONC(  31 ) /  152, 'GC', F /  ! XYLHRXN
      DATA CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), CONVERT_CONC(  32 ) /  153, 'GC', F /  ! TOLNRXN
      DATA CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), CONVERT_CONC(  33 ) /  154, 'GC', F /  ! TOLHRXN
      DATA CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), CONVERT_CONC(  34 ) /  155, 'GC', F /  ! PAHNRXN
      DATA CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), CONVERT_CONC(  35 ) /  156, 'GC', F /  ! PAHHRXN
      DATA CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), CONVERT_CONC(  36 ) /  228, 'AE', T /  ! ACLI
      DATA CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), CONVERT_CONC(  37 ) /  227, 'AE', T /  ! ACLJ
      DATA CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), CONVERT_CONC(  38 ) /  230, 'AE', T /  ! ACLK
      DATA CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), CONVERT_CONC(  39 ) /  172, 'GC', F /  ! PCVOC
      DATA CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), CONVERT_CONC(  40 ) /  173, 'GC', F /  ! PCSOARXN
      DATA CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), CONVERT_CONC(  41 ) /    6, 'GC', F /  ! N2O5
      DATA CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), CONVERT_CONC(  42 ) /   15, 'GC', F /  ! HO2H
      DATA CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), CONVERT_CONC(  43 ) /   26, 'GC', F /  ! PAN
      DATA CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), CONVERT_CONC(  44 ) /   30, 'GC', F /  ! PAN2
      DATA CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), CONVERT_CONC(  45 ) /   37, 'GC', F /  ! PBZN
      DATA CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), CONVERT_CONC(  46 ) /   40, 'GC', F /  ! MAPAN
      DATA CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), CONVERT_CONC(  47 ) /   41, 'GC', F /  ! TBUO
      DATA CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), CONVERT_CONC(  48 ) /   66, 'GC', F /  ! ISOPOOH
      DATA CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), CONVERT_CONC(  49 ) /   67, 'GC', F /  ! IEPOX
      DATA CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), CONVERT_CONC(  50 ) /  110, 'GC', F /  ! TOLUENE
      DATA CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), CONVERT_CONC(  51 ) /  112, 'GC', F /  ! MXYL
      DATA CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), CONVERT_CONC(  52 ) /  114, 'GC', F /  ! OXYL
      DATA CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), CONVERT_CONC(  53 ) /  115, 'GC', F /  ! PXYL
      DATA CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), CONVERT_CONC(  54 ) /  116, 'GC', F /  ! TMBENZ124
      DATA CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), CONVERT_CONC(  55 ) /  117, 'GC', F /  ! ETOH
      DATA CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), CONVERT_CONC(  56 ) /  118, 'GC', F /  ! ALK1
      DATA CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), CONVERT_CONC(  57 ) /  119, 'GC', F /  ! ALK2
      DATA CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), CONVERT_CONC(  58 ) /  120, 'GC', F /  ! ALK3
      DATA CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), CONVERT_CONC(  59 ) /  121, 'GC', F /  ! ALK4
      DATA CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), CONVERT_CONC(  60 ) /  122, 'GC', F /  ! ALK5
      DATA CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), CONVERT_CONC(  61 ) /  127, 'GC', F /  ! ARO1
      DATA CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), CONVERT_CONC(  62 ) /  129, 'GC', F /  ! NAPHTHAL
      DATA CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), CONVERT_CONC(  63 ) /  136, 'GC', F /  ! CLNO
      DATA CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), CONVERT_CONC(  64 ) /  137, 'GC', F /  ! CLONO
      DATA CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), CONVERT_CONC(  65 ) /  142, 'GC', F /  ! HOCL
      DATA CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), CONVERT_CONC(  66 ) /  161, 'GC', F /  ! H2NO3PK
      DATA CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), CONVERT_CONC(  67 ) /  166, 'GC', F /  ! VIVPO1
      DATA CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), CONVERT_CONC(  68 ) /   14, 'GC', F /  ! HNO4
      DATA CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), CONVERT_CONC(  69 ) /   21, 'GC', F /  ! COOH
      DATA CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), CONVERT_CONC(  70 ) /   27, 'GC', F /  ! CCOOOH
      DATA CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), CONVERT_CONC(  71 ) /   44, 'GC', F /  ! NPHE
      DATA CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), CONVERT_CONC(  72 ) /   52, 'GC', F /  ! xTBUO
      DATA CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), CONVERT_CONC(  73 ) /   63, 'GC', F /  ! ROOH
      DATA CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), CONVERT_CONC(  74 ) /   65, 'GC', F /  ! R6OOH
      DATA CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), CONVERT_CONC(  75 ) /   72, 'GC', F /  ! RAOOH
      DATA CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), CONVERT_CONC(  76 ) /   71, 'GC', F /  ! yISOPOOH
      DATA CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), CONVERT_CONC(  77 ) /  128, 'GC', F /  ! ARO2MN
      DATA CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), CONVERT_CONC(  78 ) /  107, 'GC', F /  ! ACETYLENE
      DATA CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), CONVERT_CONC(  79 ) /  134, 'GC', F /  ! CL2
      DATA CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), CONVERT_CONC(  80 ) /  141, 'GC', F /  ! CLONO2
      DATA CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), CONVERT_CONC(  81 ) /  146, 'GC', F /  ! CLCCHO
      DATA CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), CONVERT_CONC(  82 ) /  157, 'GC', F /  ! HCHO_PRIMARY
      DATA CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), CONVERT_CONC(  83 ) /  158, 'GC', F /  ! CCHO_PRIMARY
      DATA CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), CONVERT_CONC(  84 ) /  160, 'GC', F /  ! H2NO3PIJ
      DATA CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), CONVERT_CONC(  85 ) /   10, 'GC', F /  ! HONO
      DATA CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), CONVERT_CONC(  86 ) /   84, 'GC', F /  ! BALD
      DATA CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), CONVERT_CONC(  87 ) /  109, 'GC', F /  ! BENZRO2
      DATA CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), CONVERT_CONC(  88 ) /  130, 'GC', F /  ! PAHRO2
      DATA CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), CONVERT_CONC(  89 ) /  133, 'GC', F /  ! SESQRXN
      DATA CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), CONVERT_CONC(  90 ) /  238, 'AE', T /  ! AOLGBJ
      DATA CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), CONVERT_CONC(  91 ) /  165, 'GC', F /  ! VSVPO3
      DATA CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), CONVERT_CONC(  92 ) /   34, 'GC', F /  ! RCOOOH
      DATA CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), CONVERT_CONC(  93 ) /  100, 'GC', F /  ! ETHENE
      DATA CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), CONVERT_CONC(  94 ) /  101, 'GC', F /  ! PROPENE
      DATA CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), CONVERT_CONC(  95 ) /  102, 'GC', F /  ! BUTADIENE13
      DATA CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), CONVERT_CONC(  96 ) /  103, 'GC', F /  ! ISOPRENE
      DATA CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), CONVERT_CONC(  97 ) /  105, 'GC', F /  ! APIN
      DATA CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), CONVERT_CONC(  98 ) /  106, 'GC', F /  ! TRPRXN
      DATA CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), CONVERT_CONC(  99 ) /  111, 'GC', F /  ! TOLRO2
      DATA CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), CONVERT_CONC( 100 ) /  125, 'GC', F /  ! OLE1
      DATA CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), CONVERT_CONC( 101 ) /  126, 'GC', F /  ! OLE2
      DATA CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), CONVERT_CONC( 102 ) /  131, 'GC', F /  ! TERP
      DATA CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), CONVERT_CONC( 103 ) /  132, 'GC', F /  ! SESQ
      DATA CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), CONVERT_CONC( 104 ) /  159, 'GC', F /  ! ACRO_PRIMARY
      DATA CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), CONVERT_CONC( 105 ) /  162, 'GC', F /  ! VLVPO1
      DATA CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), CONVERT_CONC( 106 ) /  163, 'GC', F /  ! VSVPO1
      DATA CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), CONVERT_CONC( 107 ) /  164, 'GC', F /  ! VSVPO2
      DATA CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), CONVERT_CONC( 108 ) /  169, 'GC', F /  ! VSVOO1
      DATA CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), CONVERT_CONC( 109 ) /   22, 'GC', F /  ! MEOH
      DATA CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), CONVERT_CONC( 110 ) /   42, 'GC', F /  ! RNO3
      DATA CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), CONVERT_CONC( 111 ) /   43, 'GC', F /  ! ACETONE
      DATA CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), CONVERT_CONC( 112 ) /   48, 'GC', F /  ! xMEO2
      DATA CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), CONVERT_CONC( 113 ) /   82, 'GC', F /  ! HCOCO3
      DATA CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), CONVERT_CONC( 114 ) /   99, 'GC', F /  ! xACROLEIN
      DATA CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), CONVERT_CONC( 115 ) /  145, 'GC', F /  ! xCLACET
      DATA CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), CONVERT_CONC( 116 ) /  170, 'GC', F /  ! VSVOO2
      DATA CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), CONVERT_CONC( 117 ) /  171, 'GC', F /  ! VSVOO3
      DATA CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), CONVERT_CONC( 118 ) /   46, 'GC', F /  ! xOH
      DATA CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), CONVERT_CONC( 119 ) /   80, 'GC', F /  ! AFG1
      DATA CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), CONVERT_CONC( 120 ) /   81, 'GC', F /  ! AFG2
      DATA CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), CONVERT_CONC( 121 ) /   93, 'GC', F /  ! ACROLEIN
      DATA CGRID_INDEX( 122 ), SPECIES_TYPE( 122 ), CONVERT_CONC( 122 ) /   98, 'GC', F /  ! yRAOOH
      DATA CGRID_INDEX( 123 ), SPECIES_TYPE( 123 ), CONVERT_CONC( 123 ) /   68, 'GC', F /  ! IEPOXOO
      DATA CGRID_INDEX( 124 ), SPECIES_TYPE( 124 ), CONVERT_CONC( 124 ) /  138, 'GC', F /  ! CLNO2
      DATA CGRID_INDEX( 125 ), SPECIES_TYPE( 125 ), CONVERT_CONC( 125 ) /  140, 'GC', F /  ! CLO
      DATA CGRID_INDEX( 126 ), SPECIES_TYPE( 126 ), CONVERT_CONC( 126 ) /   28, 'GC', F /  ! CCOOH
      DATA CGRID_INDEX( 127 ), SPECIES_TYPE( 127 ), CONVERT_CONC( 127 ) /   51, 'GC', F /  ! xMACO3
      DATA CGRID_INDEX( 128 ), SPECIES_TYPE( 128 ), CONVERT_CONC( 128 ) /   74, 'GC', F /  ! IPRD
      DATA CGRID_INDEX( 129 ), SPECIES_TYPE( 129 ), CONVERT_CONC( 129 ) /  113, 'GC', F /  ! XYLRO2
      DATA CGRID_INDEX( 130 ), SPECIES_TYPE( 130 ), CONVERT_CONC( 130 ) /   87, 'GC', F /  ! MACR
      DATA CGRID_INDEX( 131 ), SPECIES_TYPE( 131 ), CONVERT_CONC( 131 ) /   88, 'GC', F /  ! MVK
      DATA CGRID_INDEX( 132 ), SPECIES_TYPE( 132 ), CONVERT_CONC( 132 ) /   95, 'GC', F /  ! xAFG3
      DATA CGRID_INDEX( 133 ), SPECIES_TYPE( 133 ), CONVERT_CONC( 133 ) /  237, 'AE', T /  ! AOLGAJ
      DATA CGRID_INDEX( 134 ), SPECIES_TYPE( 134 ), CONVERT_CONC( 134 ) /   47, 'GC', F /  ! xNO2
      DATA CGRID_INDEX( 135 ), SPECIES_TYPE( 135 ), CONVERT_CONC( 135 ) /   89, 'GC', F /  ! xHOCCHO
      DATA CGRID_INDEX( 136 ), SPECIES_TYPE( 136 ), CONVERT_CONC( 136 ) /   91, 'GC', F /  ! HOCCHO
      DATA CGRID_INDEX( 137 ), SPECIES_TYPE( 137 ), CONVERT_CONC( 137 ) /   94, 'GC', F /  ! xBALD
      DATA CGRID_INDEX( 138 ), SPECIES_TYPE( 138 ), CONVERT_CONC( 138 ) /   96, 'GC', F /  ! xMACR
      DATA CGRID_INDEX( 139 ), SPECIES_TYPE( 139 ), CONVERT_CONC( 139 ) /  143, 'GC', F /  ! xCL
      DATA CGRID_INDEX( 140 ), SPECIES_TYPE( 140 ), CONVERT_CONC( 140 ) /  167, 'GC', F /  ! VLVOO1
      DATA CGRID_INDEX( 141 ), SPECIES_TYPE( 141 ), CONVERT_CONC( 141 ) /  168, 'GC', F /  ! VLVOO2
      DATA CGRID_INDEX( 142 ), SPECIES_TYPE( 142 ), CONVERT_CONC( 142 ) /   35, 'GC', F /  ! RCOOH
      DATA CGRID_INDEX( 143 ), SPECIES_TYPE( 143 ), CONVERT_CONC( 143 ) /   97, 'GC', F /  ! xMVK
      DATA CGRID_INDEX( 144 ), SPECIES_TYPE( 144 ), CONVERT_CONC( 144 ) /   54, 'GC', F /  ! CCHO
      DATA CGRID_INDEX( 145 ), SPECIES_TYPE( 145 ), CONVERT_CONC( 145 ) /   62, 'GC', F /  ! xBACL
      DATA CGRID_INDEX( 146 ), SPECIES_TYPE( 146 ), CONVERT_CONC( 146 ) /   85, 'GC', F /  ! AFG3
      DATA CGRID_INDEX( 147 ), SPECIES_TYPE( 147 ), CONVERT_CONC( 147 ) /  144, 'GC', F /  ! xCLCCHO
      DATA CGRID_INDEX( 148 ), SPECIES_TYPE( 148 ), CONVERT_CONC( 148 ) /   90, 'GC', F /  ! xRNO3
      DATA CGRID_INDEX( 149 ), SPECIES_TYPE( 149 ), CONVERT_CONC( 149 ) /   36, 'GC', F /  ! BZCO3
      DATA CGRID_INDEX( 150 ), SPECIES_TYPE( 150 ), CONVERT_CONC( 150 ) /   45, 'GC', F /  ! CRES
      DATA CGRID_INDEX( 151 ), SPECIES_TYPE( 151 ), CONVERT_CONC( 151 ) /   86, 'GC', F /  ! xIPRD
      DATA CGRID_INDEX( 152 ), SPECIES_TYPE( 152 ), CONVERT_CONC( 152 ) /   92, 'GC', F /  ! xACETONE
      DATA CGRID_INDEX( 153 ), SPECIES_TYPE( 153 ), CONVERT_CONC( 153 ) /   38, 'GC', F /  ! BZO
      DATA CGRID_INDEX( 154 ), SPECIES_TYPE( 154 ), CONVERT_CONC( 154 ) /   49, 'GC', F /  ! xMECO3
      DATA CGRID_INDEX( 155 ), SPECIES_TYPE( 155 ), CONVERT_CONC( 155 ) /   50, 'GC', F /  ! xRCO3
      DATA CGRID_INDEX( 156 ), SPECIES_TYPE( 156 ), CONVERT_CONC( 156 ) /   64, 'GC', F /  ! xPROD2
      DATA CGRID_INDEX( 157 ), SPECIES_TYPE( 157 ), CONVERT_CONC( 157 ) /    3, 'GC', F /  ! O3P
      DATA CGRID_INDEX( 158 ), SPECIES_TYPE( 158 ), CONVERT_CONC( 158 ) /    7, 'GC', F /  ! HNO3
      DATA CGRID_INDEX( 159 ), SPECIES_TYPE( 159 ), CONVERT_CONC( 159 ) /   53, 'GC', F /  ! xCO
      DATA CGRID_INDEX( 160 ), SPECIES_TYPE( 160 ), CONVERT_CONC( 160 ) /   73, 'GC', F /  ! MGLY
      DATA CGRID_INDEX( 161 ), SPECIES_TYPE( 161 ), CONVERT_CONC( 161 ) /   76, 'GC', F /  ! xMEK
      DATA CGRID_INDEX( 162 ), SPECIES_TYPE( 162 ), CONVERT_CONC( 162 ) /   55, 'GC', F /  ! RCHO
      DATA CGRID_INDEX( 163 ), SPECIES_TYPE( 163 ), CONVERT_CONC( 163 ) /   77, 'GC', F /  ! xAFG1
      DATA CGRID_INDEX( 164 ), SPECIES_TYPE( 164 ), CONVERT_CONC( 164 ) /   78, 'GC', F /  ! xAFG2
      DATA CGRID_INDEX( 165 ), SPECIES_TYPE( 165 ), CONVERT_CONC( 165 ) /   79, 'GC', F /  ! GLY
      DATA CGRID_INDEX( 166 ), SPECIES_TYPE( 166 ), CONVERT_CONC( 166 ) /   29, 'GC', F /  ! RCO3
      DATA CGRID_INDEX( 167 ), SPECIES_TYPE( 167 ), CONVERT_CONC( 167 ) /   39, 'GC', F /  ! MACO3
      DATA CGRID_INDEX( 168 ), SPECIES_TYPE( 168 ), CONVERT_CONC( 168 ) /   57, 'GC', F /  ! MEK
      DATA CGRID_INDEX( 169 ), SPECIES_TYPE( 169 ), CONVERT_CONC( 169 ) /   60, 'GC', F /  ! HCOOH
      DATA CGRID_INDEX( 170 ), SPECIES_TYPE( 170 ), CONVERT_CONC( 170 ) /    2, 'GC', F /  ! NO
      DATA CGRID_INDEX( 171 ), SPECIES_TYPE( 171 ), CONVERT_CONC( 171 ) /   75, 'GC', F /  ! xGLY
      DATA CGRID_INDEX( 172 ), SPECIES_TYPE( 172 ), CONVERT_CONC( 172 ) /   61, 'GC', F /  ! xMGLY
      DATA CGRID_INDEX( 173 ), SPECIES_TYPE( 173 ), CONVERT_CONC( 173 ) /   69, 'GC', F /  ! PRD2
      DATA CGRID_INDEX( 174 ), SPECIES_TYPE( 174 ), CONVERT_CONC( 174 ) /   59, 'GC', F /  ! xRCHO
      DATA CGRID_INDEX( 175 ), SPECIES_TYPE( 175 ), CONVERT_CONC( 175 ) /    4, 'GC', F /  ! O3
      DATA CGRID_INDEX( 176 ), SPECIES_TYPE( 176 ), CONVERT_CONC( 176 ) /   19, 'GC', F /  ! MEO2
      DATA CGRID_INDEX( 177 ), SPECIES_TYPE( 177 ), CONVERT_CONC( 177 ) /   25, 'GC', F /  ! MECO3
      DATA CGRID_INDEX( 178 ), SPECIES_TYPE( 178 ), CONVERT_CONC( 178 ) /   33, 'GC', F /  ! xCCHO
      DATA CGRID_INDEX( 179 ), SPECIES_TYPE( 179 ), CONVERT_CONC( 179 ) /   70, 'GC', F /  ! yR6OOH
      DATA CGRID_INDEX( 180 ), SPECIES_TYPE( 180 ), CONVERT_CONC( 180 ) /    1, 'GC', F /  ! NO2
      DATA CGRID_INDEX( 181 ), SPECIES_TYPE( 181 ), CONVERT_CONC( 181 ) /   56, 'GC', F /  ! xHCHO
      DATA CGRID_INDEX( 182 ), SPECIES_TYPE( 182 ), CONVERT_CONC( 182 ) /   20, 'GC', F /  ! HCHO
      DATA CGRID_INDEX( 183 ), SPECIES_TYPE( 183 ), CONVERT_CONC( 183 ) /  139, 'GC', F /  ! HCL
      DATA CGRID_INDEX( 184 ), SPECIES_TYPE( 184 ), CONVERT_CONC( 184 ) /   12, 'GC', F /  ! CO
      DATA CGRID_INDEX( 185 ), SPECIES_TYPE( 185 ), CONVERT_CONC( 185 ) /   32, 'GC', F /  ! yROOH
      DATA CGRID_INDEX( 186 ), SPECIES_TYPE( 186 ), CONVERT_CONC( 186 ) /   58, 'GC', F /  ! zRNO3
      DATA CGRID_INDEX( 187 ), SPECIES_TYPE( 187 ), CONVERT_CONC( 187 ) /   13, 'GC', F /  ! CO2
      DATA CGRID_INDEX( 188 ), SPECIES_TYPE( 188 ), CONVERT_CONC( 188 ) /    5, 'GC', F /  ! NO3
      DATA CGRID_INDEX( 189 ), SPECIES_TYPE( 189 ), CONVERT_CONC( 189 ) /   24, 'GC', F /  ! RO2XC
      DATA CGRID_INDEX( 190 ), SPECIES_TYPE( 190 ), CONVERT_CONC( 190 ) /   31, 'GC', F /  ! xHO2
      DATA CGRID_INDEX( 191 ), SPECIES_TYPE( 191 ), CONVERT_CONC( 191 ) /  135, 'GC', F /  ! CL
      DATA CGRID_INDEX( 192 ), SPECIES_TYPE( 192 ), CONVERT_CONC( 192 ) /   23, 'GC', F /  ! RO2C
      DATA CGRID_INDEX( 193 ), SPECIES_TYPE( 193 ), CONVERT_CONC( 193 ) /   11, 'GC', F /  ! HO2
      DATA CGRID_INDEX( 194 ), SPECIES_TYPE( 194 ), CONVERT_CONC( 194 ) /    9, 'GC', F /  ! OH

! The below integers define the locations of mechanism species in the solver
! concentration array.

      INTEGER ::  INDEX_AXYL1J           =    1
      INTEGER ::  INDEX_AXYL2J           =    2
      INTEGER ::  INDEX_ATOL1J           =    3
      INTEGER ::  INDEX_ATOL2J           =    4
      INTEGER ::  INDEX_ABNZ1J           =    5
      INTEGER ::  INDEX_ABNZ2J           =    6
      INTEGER ::  INDEX_ATRP1J           =    7
      INTEGER ::  INDEX_ATRP2J           =    8
      INTEGER ::  INDEX_AISO1J           =    9
      INTEGER ::  INDEX_AISO2J           =   10
      INTEGER ::  INDEX_ASQTJ            =   11
      INTEGER ::  INDEX_APAH1J           =   12
      INTEGER ::  INDEX_APAH2J           =   13
      INTEGER ::  INDEX_AALK1J           =   14
      INTEGER ::  INDEX_AALK2J           =   15
      INTEGER ::  INDEX_AISO3J           =   16
      INTEGER ::  INDEX_O1D              =   17
      INTEGER ::  INDEX_SO2              =   18
      INTEGER ::  INDEX_SULF             =   19
      INTEGER ::  INDEX_SULRXN           =   20
      INTEGER ::  INDEX_BACL             =   21
      INTEGER ::  INDEX_ISOPRXN          =   22
      INTEGER ::  INDEX_BENZENE          =   23
      INTEGER ::  INDEX_SOAALK           =   24
      INTEGER ::  INDEX_ALKRXN           =   25
      INTEGER ::  INDEX_CLACET           =   26
      INTEGER ::  INDEX_CLCHO            =   27
      INTEGER ::  INDEX_BNZNRXN          =   28
      INTEGER ::  INDEX_BNZHRXN          =   29
      INTEGER ::  INDEX_XYLNRXN          =   30
      INTEGER ::  INDEX_XYLHRXN          =   31
      INTEGER ::  INDEX_TOLNRXN          =   32
      INTEGER ::  INDEX_TOLHRXN          =   33
      INTEGER ::  INDEX_PAHNRXN          =   34
      INTEGER ::  INDEX_PAHHRXN          =   35
      INTEGER ::  INDEX_ACLI             =   36
      INTEGER ::  INDEX_ACLJ             =   37
      INTEGER ::  INDEX_ACLK             =   38
      INTEGER ::  INDEX_PCVOC            =   39
      INTEGER ::  INDEX_PCSOARXN         =   40
      INTEGER ::  INDEX_N2O5             =   41
      INTEGER ::  INDEX_HO2H             =   42
      INTEGER ::  INDEX_PAN              =   43
      INTEGER ::  INDEX_PAN2             =   44
      INTEGER ::  INDEX_PBZN             =   45
      INTEGER ::  INDEX_MAPAN            =   46
      INTEGER ::  INDEX_TBUO             =   47
      INTEGER ::  INDEX_ISOPOOH          =   48
      INTEGER ::  INDEX_IEPOX            =   49
      INTEGER ::  INDEX_TOLUENE          =   50
      INTEGER ::  INDEX_MXYL             =   51
      INTEGER ::  INDEX_OXYL             =   52
      INTEGER ::  INDEX_PXYL             =   53
      INTEGER ::  INDEX_TMBENZ124        =   54
      INTEGER ::  INDEX_ETOH             =   55
      INTEGER ::  INDEX_ALK1             =   56
      INTEGER ::  INDEX_ALK2             =   57
      INTEGER ::  INDEX_ALK3             =   58
      INTEGER ::  INDEX_ALK4             =   59
      INTEGER ::  INDEX_ALK5             =   60
      INTEGER ::  INDEX_ARO1             =   61
      INTEGER ::  INDEX_NAPHTHAL         =   62
      INTEGER ::  INDEX_CLNO             =   63
      INTEGER ::  INDEX_CLONO            =   64
      INTEGER ::  INDEX_HOCL             =   65
      INTEGER ::  INDEX_H2NO3PK          =   66
      INTEGER ::  INDEX_VIVPO1           =   67
      INTEGER ::  INDEX_HNO4             =   68
      INTEGER ::  INDEX_COOH             =   69
      INTEGER ::  INDEX_CCOOOH           =   70
      INTEGER ::  INDEX_NPHE             =   71
      INTEGER ::  INDEX_xTBUO            =   72
      INTEGER ::  INDEX_ROOH             =   73
      INTEGER ::  INDEX_R6OOH            =   74
      INTEGER ::  INDEX_RAOOH            =   75
      INTEGER ::  INDEX_yISOPOOH         =   76
      INTEGER ::  INDEX_ARO2MN           =   77
      INTEGER ::  INDEX_ACETYLENE        =   78
      INTEGER ::  INDEX_CL2              =   79
      INTEGER ::  INDEX_CLONO2           =   80
      INTEGER ::  INDEX_CLCCHO           =   81
      INTEGER ::  INDEX_HCHO_PRIMARY     =   82
      INTEGER ::  INDEX_CCHO_PRIMARY     =   83
      INTEGER ::  INDEX_H2NO3PIJ         =   84
      INTEGER ::  INDEX_HONO             =   85
      INTEGER ::  INDEX_BALD             =   86
      INTEGER ::  INDEX_BENZRO2          =   87
      INTEGER ::  INDEX_PAHRO2           =   88
      INTEGER ::  INDEX_SESQRXN          =   89
      INTEGER ::  INDEX_AOLGBJ           =   90
      INTEGER ::  INDEX_VSVPO3           =   91
      INTEGER ::  INDEX_RCOOOH           =   92
      INTEGER ::  INDEX_ETHENE           =   93
      INTEGER ::  INDEX_PROPENE          =   94
      INTEGER ::  INDEX_BUTADIENE13      =   95
      INTEGER ::  INDEX_ISOPRENE         =   96
      INTEGER ::  INDEX_APIN             =   97
      INTEGER ::  INDEX_TRPRXN           =   98
      INTEGER ::  INDEX_TOLRO2           =   99
      INTEGER ::  INDEX_OLE1             =  100
      INTEGER ::  INDEX_OLE2             =  101
      INTEGER ::  INDEX_TERP             =  102
      INTEGER ::  INDEX_SESQ             =  103
      INTEGER ::  INDEX_ACRO_PRIMARY     =  104
      INTEGER ::  INDEX_VLVPO1           =  105
      INTEGER ::  INDEX_VSVPO1           =  106
      INTEGER ::  INDEX_VSVPO2           =  107
      INTEGER ::  INDEX_VSVOO1           =  108
      INTEGER ::  INDEX_MEOH             =  109
      INTEGER ::  INDEX_RNO3             =  110
      INTEGER ::  INDEX_ACETONE          =  111
      INTEGER ::  INDEX_xMEO2            =  112
      INTEGER ::  INDEX_HCOCO3           =  113
      INTEGER ::  INDEX_xACROLEIN        =  114
      INTEGER ::  INDEX_xCLACET          =  115
      INTEGER ::  INDEX_VSVOO2           =  116
      INTEGER ::  INDEX_VSVOO3           =  117
      INTEGER ::  INDEX_xOH              =  118
      INTEGER ::  INDEX_AFG1             =  119
      INTEGER ::  INDEX_AFG2             =  120
      INTEGER ::  INDEX_ACROLEIN         =  121
      INTEGER ::  INDEX_yRAOOH           =  122
      INTEGER ::  INDEX_IEPOXOO          =  123
      INTEGER ::  INDEX_CLNO2            =  124
      INTEGER ::  INDEX_CLO              =  125
      INTEGER ::  INDEX_CCOOH            =  126
      INTEGER ::  INDEX_xMACO3           =  127
      INTEGER ::  INDEX_IPRD             =  128
      INTEGER ::  INDEX_XYLRO2           =  129
      INTEGER ::  INDEX_MACR             =  130
      INTEGER ::  INDEX_MVK              =  131
      INTEGER ::  INDEX_xAFG3            =  132
      INTEGER ::  INDEX_AOLGAJ           =  133
      INTEGER ::  INDEX_xNO2             =  134
      INTEGER ::  INDEX_xHOCCHO          =  135
      INTEGER ::  INDEX_HOCCHO           =  136
      INTEGER ::  INDEX_xBALD            =  137
      INTEGER ::  INDEX_xMACR            =  138
      INTEGER ::  INDEX_xCL              =  139
      INTEGER ::  INDEX_VLVOO1           =  140
      INTEGER ::  INDEX_VLVOO2           =  141
      INTEGER ::  INDEX_RCOOH            =  142
      INTEGER ::  INDEX_xMVK             =  143
      INTEGER ::  INDEX_CCHO             =  144
      INTEGER ::  INDEX_xBACL            =  145
      INTEGER ::  INDEX_AFG3             =  146
      INTEGER ::  INDEX_xCLCCHO          =  147
      INTEGER ::  INDEX_xRNO3            =  148
      INTEGER ::  INDEX_BZCO3            =  149
      INTEGER ::  INDEX_CRES             =  150
      INTEGER ::  INDEX_xIPRD            =  151
      INTEGER ::  INDEX_xACETONE         =  152
      INTEGER ::  INDEX_BZO              =  153
      INTEGER ::  INDEX_xMECO3           =  154
      INTEGER ::  INDEX_xRCO3            =  155
      INTEGER ::  INDEX_xPROD2           =  156
      INTEGER ::  INDEX_O3P              =  157
      INTEGER ::  INDEX_HNO3             =  158
      INTEGER ::  INDEX_xCO              =  159
      INTEGER ::  INDEX_MGLY             =  160
      INTEGER ::  INDEX_xMEK             =  161
      INTEGER ::  INDEX_RCHO             =  162
      INTEGER ::  INDEX_xAFG1            =  163
      INTEGER ::  INDEX_xAFG2            =  164
      INTEGER ::  INDEX_GLY              =  165
      INTEGER ::  INDEX_RCO3             =  166
      INTEGER ::  INDEX_MACO3            =  167
      INTEGER ::  INDEX_MEK              =  168
      INTEGER ::  INDEX_HCOOH            =  169
      INTEGER ::  INDEX_NO               =  170
      INTEGER ::  INDEX_xGLY             =  171
      INTEGER ::  INDEX_xMGLY            =  172
      INTEGER ::  INDEX_PRD2             =  173
      INTEGER ::  INDEX_xRCHO            =  174
      INTEGER ::  INDEX_O3               =  175
      INTEGER ::  INDEX_MEO2             =  176
      INTEGER ::  INDEX_MECO3            =  177
      INTEGER ::  INDEX_xCCHO            =  178
      INTEGER ::  INDEX_yR6OOH           =  179
      INTEGER ::  INDEX_NO2              =  180
      INTEGER ::  INDEX_xHCHO            =  181
      INTEGER ::  INDEX_HCHO             =  182
      INTEGER ::  INDEX_HCL              =  183
      INTEGER ::  INDEX_CO               =  184
      INTEGER ::  INDEX_yROOH            =  185
      INTEGER ::  INDEX_zRNO3            =  186
      INTEGER ::  INDEX_CO2              =  187
      INTEGER ::  INDEX_NO3              =  188
      INTEGER ::  INDEX_RO2XC            =  189
      INTEGER ::  INDEX_xHO2             =  190
      INTEGER ::  INDEX_CL               =  191
      INTEGER ::  INDEX_RO2C             =  192
      INTEGER ::  INDEX_HO2              =  193
      INTEGER ::  INDEX_OH               =  194

      INTEGER, PARAMETER :: N_ACT_SP = 194

      INTEGER, PARAMETER :: NRXNS = 464

      INTEGER, PARAMETER ::    ZERO_REACT_REACTIONS =   0

      INTEGER, PARAMETER ::     ONE_REACT_REACTIONS = 171

      INTEGER, PARAMETER ::     TWO_REACT_REACTIONS = 293

      INTEGER, PARAMETER ::   THREE_REACT_REACTIONS =   0

      INTEGER, PARAMETER ::         ONE_REACT_START =   1

      INTEGER, PARAMETER ::        ZERO_REACT_START = 171

      INTEGER, PARAMETER ::         TWO_REACT_START = 172

      INTEGER, PARAMETER ::       THREE_REACT_START = 464

      INTEGER, PARAMETER :: NSUNLIGHT_RXNS =  55

      INTEGER, PARAMETER :: NTHERMAL_RXNS  = 409

      INTEGER            :: KUNITS

      DATA  KUNITS /   2 /

      INTEGER IRXXN

      INTEGER, PARAMETER :: NMPHOT =  54
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33,   34,   35,   36,   37,   38,   39,   40, & 
     &     41,   42,   43,   44,   45,   46,   47,   48,   49,   50, & 
     &     51,   52,   53,   54/

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     10,   10,   10,   11,   12,   13,   14,   15,   16,   17, & 
     &     17,   17,   17,   18,   19,   20,   21,    1,    1,   22, & 
     &     23,   23,   24,   25,   24,   16,   26,   27,   28,   29, & 
     &     17,   30,   31,   32,   33,   34,   35,   36,   37,   38, & 
     &     11,   12,   13,   28/

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33,   34,   35,   36,   37,   38,   39,   40, & 
     &     41,   42,   43,   44,   45,   46,   47,   48,   49,   50, & 
     &     51,   52,   53,   54/

      INTEGER            :: IRXBITS( NRXNS )

      DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,   20,    1,    8,    8,    8, & ! 5   
     &      4,    1,  128,    1,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &     64,    1,  128,    0,    0,    0,    0,    0,    0,   64, & ! 4   
     &      1,    1,    1,    1,    1,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      1,    0,    1,    0,    1,    0,    0,    0,   16,    1, & ! 7   
     &      0,    1,    0,    1,    0,    0,    0,    0,    0,    1, & ! 8   
     &      0,    0,    0,    8,    0,    0,    0,    0,    1,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    1,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    1,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    1, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    4,    1,    1,    0,    0,    0,    0, & ! 7   
     &      0,    1,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    1,    0,    0, & ! O   
     &      0,    0,    1,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      1,    1,    1,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      INTEGER, PARAMETER :: NTERMS_JACOB =  2984

      INTEGER, PARAMETER :: NSTEPS_JACOB =  22315

      INTEGER            :: IORDER( NRXNS )

      DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    3,    1,    2,    3,    2, & ! 5   
     &      2,    1,    2,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 9   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      2,    1,    2,    1,    1,    1,    1,    1,    1,    2, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    2,    2,    2,    2,    2,    2,    2,    3,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    3,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    3,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2/     !  6   

      INTEGER, PARAMETER :: NWM =   3
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &     56,   61,  374/
      REAL,    PARAMETER :: ATM_AIR = 1.00000E+06

      INTEGER, PARAMETER :: NWW =   5
      INTEGER            :: NRXWW( NWW )

      DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) / & 
     &     58,   59,   59,   60,  194/

      INTEGER, PARAMETER :: NWO2 =   2
      INTEGER            :: NRXWO2( NWO2 )

      DATA ( NRXWO2( IRXXN ), IRXXN = 1, NWO2 ) / & 
     &     56,  179/
      REAL,    PARAMETER :: ATM_O2 = 2.09500E+05

      INTEGER, PARAMETER :: NWN2 =   0
      INTEGER            :: NRXWN2( 1 )

      DATA   NRXWN2( 1 ) / 0 /
      REAL,    PARAMETER :: ATM_N2 = 7.80800E+05

      INTEGER, PARAMETER :: NWCH4 =   2
      INTEGER            :: NRXWCH4( NWCH4 )

      DATA ( NRXWCH4( IRXXN ), IRXXN = 1, NWCH4 ) / & 
     &    141,  150/
      REAL,    PARAMETER :: ATM_CH4 = 1.85000E+00

      INTEGER, PARAMETER :: NWH2 =   2
      INTEGER            :: NRXWH2( NWH2 )

      DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & 
     &     63,  143/
      REAL,    PARAMETER :: ATM_H2 = 5.60000E-01

      INTEGER, PARAMETER :: MXPRD =  26
      INTEGER            :: IRR( NRXNS,MXPRD+3 )

      DATA ( IRR( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &    180,  188,  188,  175,  175,   85,  158,   68,   42,   43, & ! O   
     &     44,   45,   46,  182,  182,  144,  162,  111,  168,   69, & ! 1   
     &     73,   74,   75,  165,  165,  160,   21,   71,   71,   86, & ! 2   
     &    119,  120,  130,  131,  128,  173,  110,  136,  121,   70, & ! 3   
     &     48,   79,   63,   64,  124,   80,   80,   65,   81,   26, & ! 4   
     &     82,   82,   83,  104,  175,  157,   41,   41,   41,   17, & ! 5   
     &     17,   68,  194,   43,   44,   45,   46,   47,  153,  190, & ! 6   
     &    190,  118,  118,  134,  134,  112,  112,  154,  154,  155, & ! 7   
     &    155,  127,  127,   72,   72,  159,  159,  181,  181,  178, & ! 8   
     &    178,  174,  174,  152,  152,  161,  161,  156,  156,  171, & ! 9   
     &    171,  172,  172,  145,  145,  137,  137,  163,  163,  164, & ! O   
     &    164,  132,  132,  138,  138,  143,  143,  151,  151,  148, & ! 1   
     &    148,  186,  186,  186,  185,  185,  185,  179,  179,  179, & ! 2   
     &     76,   76,   76,  122,  122,  122,  135,  135,  114,  114, & ! 3   
     &    194,   80,  191,  139,  139,  147,  147,  115,  115,  191, & ! 4   
     &    180,   41,   41,   84,   66,    1,    2,    3,    4,    5, & ! 5   
     &      6,    7,    8,    9,   10,   11,   12,   13,   14,   15, & ! 6   
     &     49,  157,  157,  157,  157,  175,  175,  170,  170,  180, & ! 7   
     &    180,  194,  194,  194,  194,  194,  194,  194,  193,  193, & ! 8   
     &     68,  193,  193,  193,  188,  188,   42,  194,  194,  176, & ! 9   
     &    176,  176,  176,  176,  176,  192,  192,  192,  192,  192, & ! O   
     &    189,  189,  189,  189,  189,  189,  177,  177,  177,  177, & ! 1   
     &    177,  177,  177,  177,  166,  166,  166,  166,  166,  166, & ! 2   
     &    166,  166,  166,  149,  149,  149,  149,  149,  149,  149, & ! 3   
     &    149,  149,  149,  167,  167,  167,  167,  167,  167,  167, & ! 4   
     &    167,  167,  167,  167,   47,  153,  153,  182,  182,  144, & ! 5   
     &    144,  162,  162,  111,  168,  109,  169,  126,  142,   69, & ! 6   
     &     73,   74,   75,  165,  165,  160,  160,  150,  150,   71, & ! 7   
     &     86,   86,  119,  119,  120,  120,  146,  146,  130,  130, & ! 8   
     &    130,  130,  131,  131,  131,  128,  128,  128,  173,  110, & ! 9   
     &    136,  136,  121,  121,  121,  121,   70,   92,  113,  113, & ! O   
     &    113,   93,   93,   93,   93,   94,   94,   94,   94,   95, & ! 1   
     &     95,   95,   95,   96,   96,   96,   96,   48,   48,   49, & ! 2   
     &    123,  123,  123,  123,  123,   97,   97,   97,   97,   78, & ! 3   
     &     78,   23,   50,   51,   52,   53,   54,   55,   56,   57, & ! 4   
     &     58,   59,   60,   24,  100,  100,  100,  100,  101,  101, & ! 5   
     &    101,  101,   61,   77,   62,  102,  102,  102,  102,  103, & ! 6   
     &    103,  103,  103,  191,  191,  191,  191,  191,  191,  191, & ! 7   
     &    125,  125,  191,  125,  125,  194,  182,  144,  109,  162, & ! 8   
     &    111,  168,  110,  173,  165,  160,  150,   86,   73,   74, & ! 9   
     &     75,  121,  130,  131,  128,   81,   81,   93,   94,   95, & ! O   
     &     96,   97,   78,   50,   51,   52,   53,   54,   55,   56, & ! 1   
     &     57,   58,   59,   60,  100,  101,   61,   77,   62,  102, & ! 2   
     &    103,   87,   87,  129,  129,   99,   99,   88,   88,   82, & ! 3   
     &     82,   82,   83,   83,   83,  104,  104,  104,  104,  104, & ! 4   
     &     84,   84,   66,   39,  105,  106,  107,   91,   67,  140, & ! 5   
     &    141,  108,  116,  117/     !  6   

      DATA ( IRR( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,  175,  170,  180,  180,  170,  180,  188,  170,  188, & ! 7   
     &    188,  170,   85,  180,  188,  158,  184,  175,  170,  180, & ! 8   
     &    194,  175,  193,  193,  193,  188,  194,  193,   18,  170, & ! 9   
     &    193,  193,  188,  176,  176,  170,  193,  188,  176,  192, & ! O   
     &    170,  193,  188,  176,  192,  189,  180,  170,  193,  188, & ! 1   
     &    176,  192,  189,  177,  180,  170,  193,  188,  176,  192, & ! 2   
     &    189,  177,  166,  180,  170,  193,  188,  176,  192,  189, & ! 3   
     &    177,  166,  149,  180,  170,  193,  188,  176,  192,  189, & ! 4   
     &    177,  166,  149,  167,  180,  180,  193,  194,  188,  194, & ! 5   
     &    188,  194,  188,  194,  194,  194,  194,  194,  194,  194, & ! 6   
     &    194,  194,  194,  194,  188,  194,  188,  194,  188,  194, & ! 7   
     &    194,  188,  194,  175,  194,  175,  194,  175,  194,  175, & ! 8   
     &    188,  157,  194,  175,  157,  194,  175,  188,  194,  194, & ! 9   
     &    194,  188,  194,  175,  188,  157,  194,  194,  170,  180, & ! O   
     &    193,  194,  175,  188,  157,  194,  175,  188,  157,  194, & ! 1   
     &    175,  188,  157,  194,  175,  188,  157,  194,  194,  194, & ! 2   
     &    193,  170,  176,  192,  177,  194,  175,  188,  157,  194, & ! 3   
     &    175,  194,  194,  194,  194,  194,  194,  194,  194,  194, & ! 4   
     &    194,  194,  194,  194,  194,  175,  188,  157,  194,  175, & ! 5   
     &    188,  157,  194,  194,  194,  194,  175,  188,  157,  194, & ! 6   
     &    175,  188,  157,  170,  180,  180,  193,  193,  175,  188, & ! 7   
     &    170,  180,   80,  193,  125,  183,  191,  191,  191,  191, & ! 8   
     &    191,  191,  191,  191,  191,  191,  191,  191,  191,  191, & ! 9   
     &    191,  191,  191,  191,  191,  194,  191,  191,  191,  191, & ! O   
     &    191,  191,  191,  191,  191,  191,  191,  191,  191,  191, & ! 1   
     &    191,  191,  191,  191,  191,  191,  191,  191,  191,  191, & ! 2   
     &    191,  170,  193,  170,  193,  170,  193,  170,  193,  194, & ! 3   
     &    188,  191,  194,  188,  191,  194,  175,  188,  157,  191, & ! 4   
     &     36,   37,   38,  194,  194,  194,  194,  194,  194,  194, & ! 5   
     &    194,  194,  194,  194/     !  6   

      DATA ( IRR( IRXXN,  3 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &    170,  170,  180,   17,  157,  194,  194,  193,  194,  177, & ! O   
     &    166,  149,  167,  193,  184,  184,  192,  177,  177,  182, & ! 1   
     &    162,  194,  194,  184,  182,  193,  177,   85,    0,    0, & ! 2   
     &    193,  173,  194,  176,  193,  190,  193,  184,  193,  176, & ! 3   
     &    194,  191,  191,  191,  191,  125,  191,  194,  193,  177, & ! 4   
     &      0,    0,    0,    0,    0,  175,  180,  158,  158,  194, & ! 5   
     &    157,  193,  193,  177,  166,  149,  167,  111,  150,  193, & ! 6   
     &      0,  194,    0,  180,    0,  176,    0,  177,    0,  166, & ! 7   
     &      0,  167,    0,   47,    0,  184,    0,  182,    0,  144, & ! 8   
     &      0,  162,    0,  111,    0,  168,    0,  173,    0,  165, & ! 9   
     &      0,  160,    0,   21,    0,   86,    0,  119,    0,  120, & ! O   
     &      0,  146,    0,  130,    0,  131,    0,  128,    0,  110, & ! 1   
     &      0,  110,  173,    0,   73,  168,    0,   74,  173,    0, & ! 2   
     &     48,  173,    0,   75,  173,    0,  136,    0,  121,    0, & ! 3   
     &    176,  125,  183,  191,    0,   81,    0,   26,    0,  183, & ! 4   
     &     85,  158,  158,  158,  158,  133,  133,  133,  133,  133, & ! 5   
     &    133,   90,   90,   90,   90,   90,  133,  133,  133,  133, & ! 6   
     &     16,    0,  180,  170,  188,  180,  188,  180,  180,   41, & ! 7   
     &    170,   85,  180,  158,  193,  188,  193,  193,  194,   68, & ! 8   
     &    180,  194,   42,   42,  194,  180,  193,    0,  193,  180, & ! 9   
     &     69,  182,  182,  109,  182,  180,    0,  180,  193,    0, & ! O   
     &      0,    0,  180,  193,    0,    0,   43,  176,   70,  176, & ! 1   
     &    126,  176,  176,  176,   44,  180,   92,  180,  182,  192, & ! 2   
     &    192,  187,  192,   45,  180,   92,  180,  182,  192,  192, & ! 3   
     &    187,  187,  153,   46,  180,   92,  180,  182,  187,  187, & ! 4   
     &    187,  182,  182,  182,  110,   71,  150,  193,  158,  177, & ! 5   
     &    158,  166,  158,  192,  192,  182,  193,  176,  192,  182, & ! 6   
     &    194,  194,  194,  193,  158,  184,  158,  153,  158,  153, & ! 7   
     &    149,  158,  167,  194,  167,  194,  167,  194,  167,  194, & ! 8   
     &    167,  162,  192,  194,  162,  167,  194,  167,  193,  193, & ! 9   
     &    177,  158,  190,  193,  190,  162,  177,  166,  193,  193, & ! O   
     &    194,  190,  193,  190,  193,  190,  193,  190,  162,  190, & ! 1   
     &    193,  190,  193,  190,  193,  190,  176,   49,  143,  123, & ! 2   
     &    168,  168,  168,  168,  168,  190,  193,  190,  173,  193, & ! 3   
     &    193,  193,  193,  193,  193,  193,  193,  193,  190,  190, & ! 4   
     &    190,  190,  190,  194,  190,  193,  190,  162,  190,  193, & ! 5   
     &    190,  162,  193,  193,  193,  190,  193,  190,  162,  190, & ! 6   
     &    193,  190,  162,   63,   64,  124,  183,  125,  125,  125, & ! 7   
     &    191,   80,   79,   65,   79,  191,  183,  183,  183,  183, & ! 8   
     &    183,  183,  183,  183,  183,  183,  183,  183,  183,  183, & ! 9   
     &    183,  190,  183,  192,  183,  166,  183,  190,  183,  190, & ! O   
     &    183,  183,  193,  190,  190,  190,  190,  190,  183,  183, & ! 1   
     &    183,  183,  183,  183,  183,  183,  190,  190,  190,  183, & ! 2   
     &    190,  170,  193,  170,  193,  170,  193,  170,  193,  194, & ! 3   
     &    188,  191,  194,  188,  191,  194,  175,  188,  157,  191, & ! 4   
     &    124,  124,  124,  194,  194,  194,  194,  194,  194,  194, & ! 5   
     &    194,  194,  194,  194/     !  6   

      DATA ( IRR( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &    157,    0,  157,    0,    0,  170,  180,  180,    0,  180, & ! O   
     &    180,  180,  180,  184,    0,  193,  190,  176,  192,  193, & ! 1   
     &    193,  193,  193,  193,  184,  184,    0,    0,    0,    0, & ! 2   
     &    176,    0,  193,  184,  177,  177,  190,  193,  194,  187, & ! 3   
     &    193,    0,  170,  180,  180,  180,  188,  191,  184,  192, & ! 4   
     &      0,    0,    0,    0,    0,    0,  188,    0,    0,    0, & ! 5   
     &      0,  180,    0,  180,  180,  180,  180,  176,  192,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,  193,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,  180,  193,    0,    0,    0,    0,    0,    0,  176, & ! 4   
     &    158,   84,   66,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &    180,    0,    0,    0,  180,    0,  187,    0,  180,    0, & ! 8   
     &      0,    0,    0,    0,  180,    0,    0,    0,   19,  182, & ! 9   
     &      0,    0,  193,  182,  193,    0,    0,    0,  182,    0, & ! O   
     &      0,    0,    0,  182,    0,    0,    0,  187,  126,  187, & ! 1   
     &    182,  187,  187,  187,    0,  192,  142,  192,  193,  190, & ! 2   
     &    190,  176,  190,    0,  187,  142,  187,  193,  153,  153, & ! 3   
     &    176,  192,  192,    0,  187,  142,  187,  193,  182,  182, & ! 4   
     &    176,  177,  177,  177,    0,    0,    0,  184,  193,    0, & ! 5   
     &    177,  192,  166,  154,  189,  193,  187,  192,  190,  194, & ! 6   
     &    192,  192,  193,  184,  193,  177,  184,  192,  153,    0, & ! 7   
     &      0,  149,  192,  193,  192,  193,  192,  193,  192,  193, & ! 8   
     &    192,    0,  189,  193,  168,  192,  193,  158,  190,  190, & ! 9   
     &      0,  177,  167,  194,  167,    0,  192,  192,  184,  184, & ! O   
     &    193,  192,  194,  192,  190,  192,  194,  192,  168,  192, & ! 1   
     &    194,  134,  190,  192,  194,  134,  127,  194,  138,    0, & ! 2   
     &    136,  136,  136,  136,  136,  155,  190,  134,   98,  194, & ! 3   
     &    194,  190,  190,  190,  190,  190,  190,  190,  192,  192, & ! 4   
     &     72,  112,  192,   25,  112,  190,  192,  168,  192,  190, & ! 5   
     &    134,  168,  190,  190,  190,  155,  190,  134,  173,  155, & ! 6   
     &    190,  134,  173,    0,    0,    0,    0,  194,    0,  180, & ! 7   
     &    180,    0,  188,    0,  191,    0,  193,  177,  182,  166, & ! 8   
     &    192,  192,  180,  193,  193,  184,  190,  149,  194,  194, & ! 9   
     &    194,  139,  167,  189,  193,    0,  166,  192,  190,  139, & ! O   
     &    190,  190,  184,  192,  192,  192,  192,  192,  193,  190, & ! 1   
     &    190,  190,  190,  190,  190,  190,  192,  192,  192,  190, & ! 2   
     &    139,   28,   29,   30,   31,   32,   33,   34,   35,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,   40,  105,  105,  105,  105,  105,  140, & ! 5   
     &    140,  140,  140,  140/     !  6   

      DATA ( IRR( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,  194,    0,  176, & ! O   
     &    192,  187,  187,    0,    0,  176,  185,  184,  190,  194, & ! 1   
     &    194,  192,  165,    0,    0,  177,    0,    0,    0,    0, & ! 2   
     &    177,    0,  177,  173,  166,  166,  180,  182,  176,  194, & ! 3   
     &    182,    0,    0,    0,    0,    0,    0,    0,  192,  139, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,  190,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,  158,    0,    0,    0,   20,  193, & ! 9   
     &      0,    0,  180,    0,    0,    0,    0,    0,  109,    0, & ! O   
     &      0,    0,    0,  109,    0,    0,    0,  180,  175,  180, & ! 1   
     &    193,    0,    0,    0,    0,  190,  175,  190,  192,  178, & ! 2   
     &    178,  192,  178,    0,  153,  175,  153,  192,  187,  187, & ! 3   
     &    153,  190,  187,    0,  182,  175,  182,  187,  177,  177, & ! 4   
     &    182,  192,  153,  187,    0,    0,    0,    0,  184,    0, & ! 5   
     &      0,  190,    0,  181,  186,    0,    0,  187,  187,  176, & ! 6   
     &    189,  189,  192,  113,  184,    0,  177,  190,    0,    0, & ! 7   
     &      0,    0,  189,  192,  189,  192,  189,  177,  190,  192, & ! 8   
     &    158,    0,  186,  192,    0,  190,  192,  192,  154,  180, & ! 9   
     &      0,    0,  192,  184,  192,    0,  187,  185,  187,  187, & ! O   
     &    184,  181,  184,  174,  176,  189,  176,  189,    0,  189, & ! 1   
     &    184,  192,  127,  189,  127,  192,  192,    0,  192,    0, & ! 2   
     &    165,  165,  165,  165,  165,  192,  194,  155,    0,  184, & ! 3   
     &    184,  194,  194,  194,  194,  194,  194,  192,  178,  189, & ! 4   
     &    192,  154,  189,    0,  192,  194,  189,  173,  189,  194, & ! 5   
     &    112,  173,  194,  194,  194,  192,  194,  155,   98,  192, & ! 6   
     &    194,  155,   89,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,  184,    0,  193,  192, & ! 8   
     &    181,  189,  193,  192,  184,  177,  137,    0,  192,  192, & ! 9   
     &    193,  167,  192,  186,  167,    0,    0,  181,  192,  192, & ! O   
     &    139,  139,    0,  189,  189,  189,  189,  189,  190,  192, & ! 1   
     &    192,   72,  112,  192,  192,  139,  189,  189,  189,  139, & ! 2   
     &    154,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,  106,  106,  106,  106,  106,  141, & ! 5   
     &    141,  141,  141,  141/     !  6   

      DATA ( IRR( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,  188,    0,  187, & ! O   
     &    190,  153,  182,    0,    0,    0,  178,    0,  178,    0, & ! 1   
     &      0,  189,  160,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &    167,    0,  167,  167,  184,  192,  192,    0,  167,    0, & ! 3   
     &    131,    0,    0,    0,    0,    0,    0,    0,  139,  181, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,  194,    0, & ! 1   
     &    176,    0,    0,    0,    0,  185,  194,  185,  190,  185, & ! 2   
     &    185,  190,  185,    0,  192,  194,  192,  153,    0,    0, & ! 3   
     &    192,  185,    0,    0,  177,  194,  177,  177,    0,    0, & ! 4   
     &    177,  190,  192,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,  159,    0,  185,  190,    0,    0,  190,  178,    0, & ! 6   
     &    186,  186,  189,    0,  113,    0,    0,  179,    0,    0, & ! 7   
     &      0,    0,  186,  184,  186,  184,  186,  192,  159,  184, & ! 8   
     &    190,    0,  190,  190,    0,  189,  155,  190,  155,  134, & ! 9   
     &      0,    0,  159,  187,  189,    0,  118,  187,  180,  188, & ! O   
     &    187,  135,  187,  185,  192,  186,  184,  186,    0,  186, & ! 1   
     &    187,  189,  192,  186,  192,  189,  189,    0,  148,    0, & ! 2   
     &    160,  160,  160,  160,  160,  189,  154,  192,    0,  169, & ! 3   
     &    187,  192,  192,  192,  192,  192,  192,  181,  185,  186, & ! 4   
     &    189,  192,  186,    0,  189,  192,  186,    0,  186,  176, & ! 5   
     &    192,    0,  192,  192,  192,  189,  154,  192,    0,  189, & ! 6   
     &    154,  192,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  178, & ! 8   
     &    154,  186,  192,  189,  166,    0,  179,    0,  162,  189, & ! 9   
     &    192,  192,  189,  190,  192,    0,    0,   27,  189,  189, & ! O   
     &    192,  154,    0,  186,  186,  186,  186,  186,  192,  178, & ! 1   
     &    189,  192,  154,  189,  189,  112,  186,  186,  186,  154, & ! 2   
     &    155,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,  107,  107,  107,  107,  107,  108, & ! 5   
     &    108,  108,  108,  108/     !  6   

      DATA ( IRR( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  188, & ! O   
     &    185,  192,  177,    0,    0,    0,  184,    0,  185,    0, & ! 1   
     &      0,  186,  119,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &    184,    0,  192,    0,  182,  189,  189,    0,  184,    0, & ! 3   
     &    130,    0,    0,    0,    0,    0,    0,    0,  181,  185, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,  176,    0, & ! 1   
     &    187,    0,    0,    0,    0,  178,  190,  178,  178,  187, & ! 2   
     &    187,  185,  187,    0,    0,  153,    0,  187,    0,    0, & ! 3   
     &      0,  178,    0,    0,    0,  182,    0,    0,    0,    0, & ! 4   
     &      0,  185,  187,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,  178,    0,    0,  154,    0,    0,  172,  174,    0, & ! 6   
     &    162,  173,  186,    0,    0,    0,    0,  172,    0,    0, & ! 7   
     &      0,    0,  190,  187,  190,  187,  190,  189,  181,  187, & ! 8   
     &    159,    0,  154,  184,    0,  186,  184,  189,  192,  192, & ! 9   
     &      0,    0,  181,  182,  186,    0,  181,  118,    0,    0, & ! O   
     &    165,  185,  182,    0,  184,  181,  187,  185,    0,  181, & ! 1   
     &    182,  186,  189,  181,  189,  186,  186,    0,  181,    0, & ! 2   
     &    194,  194,  194,  194,  194,  186,  155,  189,    0,  165, & ! 3   
     &      0,  189,  189,  189,  189,  189,  189,  144,    0,  174, & ! 4   
     &    186,  189,  181,    0,  186,  189,  178,    0,  181,  154, & ! 5   
     &    189,    0,  189,  189,  189,  186,  155,  189,    0,  186, & ! 6   
     &    155,  189,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  159, & ! 8   
     &    185,  190,  189,  186,    0,    0,    0,    0,  118,  186, & ! 9   
     &    189,  189,  186,  154,  189,    0,    0,    0,  186,  186, & ! O   
     &    189,  155,    0,  137,  137,  137,  137,  137,  181,  185, & ! 1   
     &    186,  189,  192,  186,  186,  192,  156,  137,  137,  155, & ! 2   
     &    127,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,   91,   91,   91,   91,  140,  116, & ! 5   
     &    116,  116,  116,  116/     !  6   

      DATA ( IRR( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &    178,  188,  188,    0,    0,    0,  193,    0,    0,    0, & ! 1   
     &      0,  162,  120,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &    165,    0,  184,    0,  136,  186,  186,    0,  187,    0, & ! 3   
     &    192,    0,    0,    0,    0,    0,    0,    0,  185,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,  187,    0, & ! 1   
     &      0,    0,    0,    0,    0,  187,  192,  187,  185,    0, & ! 2   
     &      0,  178,    0,    0,    0,  192,    0,    0,    0,    0, & ! 3   
     &      0,  153,    0,    0,    0,  177,    0,    0,    0,    0, & ! 4   
     &      0,  178,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,  185,    0,    0,  155,    0,    0,  185,  145,    0, & ! 6   
     &    190,  190,  173,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,  154,  165,  154,  165,  154,  186,  161,  182, & ! 8   
     &    185,    0,  181,  187,    0,  159,  187,  186,  189,  189, & ! 9   
     &      0,    0,  178,  169,  158,    0,  185,  178,    0,    0, & ! O   
     &    175,    0,  169,    0,  159,  178,  182,    0,    0,  114, & ! 1   
     &    169,  181,  186,  138,  186,  151,  181,    0,  174,    0, & ! 2   
     &    193,  193,  193,  193,  193,  159,  192,  186,    0,    0, & ! 3   
     &      0,  186,  186,  186,  186,  186,  186,  135,    0,  152, & ! 4   
     &    181,  186,  178,    0,  181,  186,  174,    0,  178,  155, & ! 5   
     &    186,    0,  186,  186,  186,  159,  192,  186,    0,  159, & ! 6   
     &    192,  186,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  190, & ! 8   
     &      0,  154,  186,  162,    0,    0,    0,    0,  190,  173, & ! 9   
     &    186,  186,  190,  147,  186,    0,    0,    0,  114,  181, & ! O   
     &    186,  127,    0,    0,    0,    0,    0,    0,  144,    0, & ! 1   
     &    174,  186,  189,  181,  181,  189,    0,  156,  156,  127, & ! 2   
     &    192,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,   67,  140,  140,  140,  141,  117, & ! 5   
     &    117,  117,  117,  117/     !  6   

      DATA ( IRR( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &    187,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,  173,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &    160,    0,  182,    0,  168,  181,  182,    0,  182,    0, & ! 3   
     &    162,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,  187,    0,  187,    0, & ! 2   
     &      0,    0,    0,    0,    0,  187,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,  187,    0,    0,    0,    0, & ! 4   
     &      0,  187,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,  181,    0,    0,    0,  185,    0, & ! 6   
     &    118,  118,  160,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,  159,  160,  159,  160,  159,  184,  172,  160, & ! 8   
     &      0,    0,  135,  182,    0,  181,  182,  159,  186,  186, & ! 9   
     &      0,    0,  171,  165,  159,    0,    0,  190,    0,    0, & ! O   
     &      0,    0,    0,    0,  181,  185,  144,    0,    0,  151, & ! 1   
     &    121,  143,  159,  143,  184,  179,  173,    0,  190,    0, & ! 2   
     &    187,  187,  187,  187,  187,  181,  189,  159,    0,    0, & ! 3   
     &      0,  171,  171,  171,  171,  171,  171,  185,    0,  185, & ! 4   
     &    178,  159,  174,    0,  178,  184,  152,    0,  174,  192, & ! 5   
     &    181,    0,  171,  171,  171,  181,  189,  159,    0,  181, & ! 6   
     &    189,  159,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  185, & ! 8   
     &      0,  155,  162,  173,    0,    0,    0,    0,  181,  118, & ! 9   
     &    173,  183,  159,  185,  119,    0,    0,    0,  147,  114, & ! O   
     &    181,  192,    0,    0,    0,    0,    0,    0,  135,    0, & ! 1   
     &    152,  181,  186,  178,  178,  186,    0,    0,    0,  192, & ! 2   
     &    189,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,  140,  141,  141,  141,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 11 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &    188,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,  190,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,  154,    0,    0,  178,  181,    0,  126,    0, & ! 3   
     &     77,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,  178,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,  178,    0,    0,    0,    0,    0, & ! 6   
     &    181,  178,  128,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,  174,  155,  174,  155,  171,  187,  185,  169, & ! 8   
     &      0,    0,  172,  160,    0,  135,  168,  181,  182,  181, & ! 9   
     &      0,    0,  185,    0,  148,    0,    0,  174,    0,    0, & ! O   
     &      0,    0,    0,    0,  144,    0,  169,    0,    0,  185, & ! 1   
     &    131,  151,  114,  151,  187,    0,  179,    0,   77,    0, & ! 2   
     &    182,  182,  182,  182,  182,  174,  186,  181,    0,    0, & ! 3   
     &      0,  150,  172,  172,  172,  172,  172,    0,    0,    0, & ! 4   
     &    174,  181,  152,    0,  135,  187,  148,    0,  152,  189, & ! 5   
     &    178,    0,  172,  172,  172,  174,  186,  181,    0,  174, & ! 6   
     &    186,  181,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,  181,  168,  190,    0,    0,    0,    0,  178,  190, & ! 9   
     &    160,  159,  151,    0,  120,    0,    0,    0,  115,  151, & ! O   
     &    143,  189,    0,    0,    0,    0,    0,    0,  185,    0, & ! 1   
     &    185,  178,  159,  174,  174,  181,    0,    0,    0,  189, & ! 2   
     &    186,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,  141,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 12 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,  178,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,  181,    0,    0,  174,  144,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,  185,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,  174,    0,    0,    0,    0,    0, & ! 6   
     &    178,  174,  190,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,  161,  181,  161,  181,  172,  165,    0,  155, & ! 8   
     &      0,    0,  185,  169,    0,  174,  165,  174,  181,  178, & ! 9   
     &      0,    0,    0,    0,  185,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,  171,    0,  126,    0,    0,    0, & ! 1   
     &    173,  148,  163,   76,  182,    0,    0,    0,   76,    0, & ! 2   
     &    169,  169,  169,  169,  169,  152,  184,  174,    0,    0, & ! 3   
     &      0,  163,  150,  150,  145,  150,  145,    0,    0,    0, & ! 4   
     &    152,  178,  161,    0,  174,  182,  185,    0,  161,  186, & ! 5   
     &    174,    0,  150,  145,  145,  152,  184,  135,    0,  152, & ! 6   
     &    184,  178,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,  178,  173,  154,    0,    0,    0,    0,  174,  178, & ! 9   
     &    128,  181,  147,    0,  190,    0,    0,    0,  185,  185, & ! O   
     &    151,  186,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,  174,  181,  152,  114,  178,    0,    0,    0,  186, & ! 2   
     &    183,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,  116,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 13 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,  174,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,  185,    0,    0,  179,  178,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,  185,    0,    0,    0,    0,    0, & ! 6   
     &    174,  156,  118,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,  171,  179,  171,  179,  145,  160,    0,  181, & ! 8   
     &      0,    0,    0,  155,    0,  161,  160,  172,  144,  162, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,  185,    0,    0,    0,    0,    0, & ! 1   
     &      0,  185,  164,   22,  181,    0,    0,    0,  162,    0, & ! 2   
     &    184,  184,  184,  184,  184,  172,  159,  152,    0,    0, & ! 3   
     &      0,  164,  137,  137,  150,  137,  150,    0,    0,    0, & ! 4   
     &    161,  174,  156,    0,  152,  178,  179,    0,  143,  184, & ! 5   
     &    152,    0,  163,  150,  150,  161,  187,  174,    0,  161, & ! 6   
     &    187,  174,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,  174,  110,  155,    0,    0,    0,    0,  161,  174, & ! 9   
     &    190,  171,  115,    0,  159,    0,    0,    0,    0,    0, & ! O   
     &    147,  159,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,  152,  178,  161,  143,  174,    0,    0,    0,  159, & ! 2   
     &    159,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,  117,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 14 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,  156,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  162,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &    156,  185,  174,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,  172,    0,  172,    0,  163,  119,    0,  185, & ! 8   
     &      0,    0,    0,  181,    0,  156,  169,  148,  178,  174, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,  173,    0,  169,    0,    0,    0,  194,    0, & ! 2   
     &      0,  180,  173,  173,  176,  145,  187,  172,    0,    0, & ! 3   
     &      0,  146,  163,  163,  137,  163,  137,    0,    0,    0, & ! 4   
     &    185,  152,  179,    0,  114,  162,    0,    0,  151,  187, & ! 5   
     &    161,    0,  164,  137,  137,  172,  182,  152,    0,  172, & ! 6   
     &    182,  152,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,  185,  134,  181,    0,    0,    0,    0,  185,  156, & ! 9   
     &    118,  147,  185,    0,  181,    0,    0,    0,    0,    0, & ! O   
     &    179,  181,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,  161,  174,  156,  151,  152,    0,    0,    0,  181, & ! 2   
     &    181,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 15 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,  179,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  174,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &    185,    0,  171,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,  179,    0,  179,    0,  164,  120,    0,    0, & ! 8   
     &      0,    0,    0,  185,    0,  171,  142,  179,  162,  152, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,  185,    0,  130,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,  109,    0,    0,  179,  181,  148,    0,    0, & ! 3   
     &      0,  122,  164,  164,  163,  164,  163,    0,    0,    0, & ! 4   
     &    179,  161,    0,    0,  138,  174,    0,    0,  156,  182, & ! 5   
     &    148,    0,  146,  163,  163,  145,  181,  138,    0,  145, & ! 6   
     &    181,  138,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,  190,  178,    0,    0,    0,    0,    0,  179, & ! 9   
     &    174,  185,    0,    0,  172,    0,    0,    0,    0,    0, & ! O   
     &      0,  174,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,  185,  152,  179,  147,  161,    0,    0,    0,  174, & ! 2   
     &    174,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 16 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  152,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,  161,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,  151,  146,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,  172,  135,    0,  174,  168, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,  131,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   98,  174,  179,    0,    0, & ! 3   
     &      0,   87,  146,  146,  164,  146,  164,    0,    0,    0, & ! 4   
     &      0,  156,    0,    0,  143,  152,    0,    0,  185,  181, & ! 5   
     &    185,    0,  156,  164,  164,  143,  174,  143,    0,  143, & ! 6   
     &    174,  143,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,  181,  174,    0,    0,    0,    0,    0,    0, & ! 9   
     &    171,    0,    0,    0,  163,    0,    0,    0,    0,    0, & ! O   
     &      0,  152,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,  179,  161,    0,  115,  138,    0,    0,    0,  152, & ! 2   
     &    152,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 17 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  168,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,  172,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,  179,  130,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,  179,  181,    0,  161,  161, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,  128,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  152,   98,    0,    0, & ! 3   
     &      0,    0,  179,  179,  146,  132,  146,    0,    0,    0, & ! 4   
     &      0,  179,    0,    0,  151,  168,    0,    0,  179,  144, & ! 5   
     &    179,    0,  179,  146,  146,  151,  152,  151,    0,  151, & ! 6   
     &    152,  151,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,  178,  161,    0,    0,    0,    0,    0,    0, & ! 9   
     &    161,    0,    0,    0,  164,    0,    0,    0,    0,    0, & ! O   
     &      0,  171,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,  156,    0,  185,  143,    0,    0,    0,  171, & ! 2   
     &    171,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 18 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  161,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,  163,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,  131,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,  179,    0,  173,  173, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,  173,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  168,    0,    0,    0, & ! 3   
     &      0,    0,  122,  122,  179,  179,  132,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,  156,  169,    0,    0,    0,  178, & ! 5   
     &      0,    0,   99,  132,  132,  156,  168,  148,    0,  156, & ! 6   
     &    168,  148,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,  174,  156,    0,    0,    0,    0,    0,    0, & ! 9   
     &    172,    0,    0,    0,  151,    0,    0,    0,    0,    0, & ! O   
     &      0,  145,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,  179,    0,  179,  151,    0,    0,    0,  145, & ! 2   
     &    145,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 19 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  173,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,  164,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,  128,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,  156,  156, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,  179,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  171,    0,    0,    0, & ! 3   
     &      0,    0,   99,  129,  122,  122,  179,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,  185,  142,    0,    0,    0,  162, & ! 5   
     &      0,    0,    0,  156,  156,  179,  169,  179,    0,  179, & ! 6   
     &    169,  179,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,  152,  179,    0,    0,    0,    0,    0,    0, & ! 9   
     &    163,    0,    0,    0,  147,    0,    0,    0,    0,    0, & ! O   
     &      0,  143,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,  147,    0,    0,    0,  143, & ! 2   
     &    143,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 20 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  156,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,  179,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,  190,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,  179,  110, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  145,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,  129,  129,  122,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,  179,  173,    0,    0,    0,  174, & ! 5   
     &      0,    0,    0,  179,  179,   98,  142,   98,    0,   89, & ! 6   
     &    142,   89,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,  161,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &    164,    0,    0,    0,  115,    0,    0,    0,    0,    0, & ! O   
     &      0,  151,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,  115,    0,    0,    0,  151, & ! 2   
     &    151,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 21 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  185,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,  155,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  148, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  173,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,  129,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,  185,    0,    0,    0,  111, & ! 5   
     &      0,    0,    0,  129,   88,    0,  172,    0,    0,    0, & ! 6   
     &    172,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,  156,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &    179,    0,    0,    0,  179,    0,    0,    0,    0,    0, & ! O   
     &      0,  163,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,  185,    0,    0,    0,  163, & ! 2   
     &    163,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 22 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  179,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,  181,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  185, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  179,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,  179,    0,    0,    0,  168, & ! 5   
     &      0,    0,    0,    0,    0,    0,  145,    0,    0,    0, & ! 6   
     &    145,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,  148,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,  164,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,  179,    0,    0,    0,  164, & ! 2   
     &    164,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 23 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,  172,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  179, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,   98,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  161, & ! 5   
     &      0,    0,    0,    0,    0,    0,  138,    0,    0,    0, & ! 6   
     &    138,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,  179,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,  132,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  132, & ! 2   
     &    132,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 24 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,  179,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  169, & ! 5   
     &      0,    0,    0,    0,    0,    0,  151,    0,    0,    0, & ! 6   
     &    151,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,  147,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  147, & ! 2   
     &    147,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 25 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  126, & ! 5   
     &      0,    0,    0,    0,    0,    0,  173,    0,    0,    0, & ! 6   
     &    173,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,  179,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  179, & ! 2   
     &    179,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 26 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  142, & ! 5   
     &      0,    0,    0,    0,    0,    0,  179,    0,    0,    0, & ! 6   
     &    179,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 27 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  173, & ! 5   
     &      0,    0,    0,    0,    0,    0,   98,    0,    0,    0, & ! 6   
     &     89,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 28 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  185, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 29 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  179, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      REAL( 8 )               :: SC( NRXNS,MXPRD )

      DATA ( SC( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 6.1000D-01, 2.0000D+00, 6.0000D-01, & ! +   
     &     6.0000D-01, 6.0000D-01, 6.0000D-01, 2.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 6.2000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 2.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0230D+00, 1.0000D+00, 3.3000D-01, 4.0000D-01, 1.2330D+00, & ! 3   
     &     9.1300D-01, 3.4400D-01, 1.0000D+00, 1.0660D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 2.0000D+00, 2.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! 7   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! 8   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! 9   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! O   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! 1   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! 4   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     5.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.1428D+00, 1.1428D+00, 1.0000D+00, 1.0000D+00, 8.5714D-01, & ! +   
     &     8.5714D-01, 1.0000D+00, 1.0000D+00, 5.0000D-01, 5.0000D-01, & ! 6   
     &     1.5000D+00, 1.4286D+00, 1.4286D+00, 1.7143D+00, 1.7143D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 2.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 8.0000D-01, & ! 9   
     &     2.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, & ! O   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 5.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 5.0000D-01, 0.0000D+00, & ! 1   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0500D-01, 1.0000D+00, & ! +   
     &     1.0000D-01, 1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 3.0750D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 2.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     3.0750D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     2.0000D+00, 2.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     3.0750D-01, 1.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 9.6500D-01, 1.0000D+00, 1.0000D+00, 9.6700D-01, & ! 6   
     &     1.0000D+00, 1.0000D+00, 5.0900D-01, 1.0000D+00, 3.0000D-01, & ! +   
     &     7.4400D-01, 8.4000D-01, 1.3900D-01, 7.0000D-01, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 2.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.1700D-01, 8.2600D-01, 2.1700D-01, & ! 8   
     &     8.2600D-01, 2.0600D-01, 4.7100D-01, 5.0000D-01, 2.0800D-01, & ! +   
     &     5.0000D-01, 1.0000D+00, 9.7500D-01, 1.6400D-01, 4.5000D-01, & ! 9   
     &     2.8900D-01, 2.8500D-01, 1.5000D-01, 4.7200D-01, 1.8900D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.5000D-01, 8.3000D-01, 3.1000D-02, & ! O   
     &     1.0000D+00, 9.8000D-01, 8.0600D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     4.4000D-01, 1.0000D+00, 1.6000D-01, 1.0000D+00, 8.0000D-01, & ! 1   
     &     9.8400D-01, 1.6500D-01, 9.4900D-01, 4.5000D-01, 9.5100D-01, & ! +   
     &     8.0000D-02, 8.1500D-01, 2.5000D-01, 9.0700D-01, 6.6000D-02, & ! 2   
     &     7.4900D-01, 2.5000D-01, 1.0000D+00, 1.6000D-01, 1.0000D+00, & ! +   
     &     7.2500D-01, 7.2500D-01, 3.6300D-01, 3.6300D-01, 7.2500D-01, & ! 3   
     &     7.9900D-01, 9.0000D-03, 5.6000D-02, 1.0000D+00, 3.0000D-01, & ! +   
     &     1.5000D+00, 5.7000D-01, 1.8100D-01, 1.5900D-01, 1.6100D-01, & ! 4   
     &     1.5900D-01, 2.2000D-02, 9.5000D-01, 1.0000D+00, 9.6500D-01, & ! +   
     &     6.9500D-01, 8.3000D-01, 6.4700D-01, 1.0000D+00, 8.7100D-01, & ! 5   
     &     9.5000D-02, 7.7200D-01, 4.5000D-01, 9.1200D-01, 9.4000D-02, & ! +   
     &     4.0000D-01, 7.9000D-02, 1.2300D-01, 7.7000D-02, 7.7000D-02, & ! 6   
     &     7.3400D-01, 7.8000D-02, 2.2700D-01, 2.3700D-01, 7.3400D-01, & ! +   
     &     7.8000D-02, 2.2700D-01, 2.3700D-01, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 2.9000D-01, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     4.0400D-01, 4.8400D-01, 2.5000D-01, 1.2830D+00, 4.0100D-01, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.2400D-01, 3.9000D-01, & ! +   
     &     1.5000D-01, 5.4800D-01, 1.0000D+00, 8.9400D-01, 8.6400D-01, & ! 1   
     &     8.6400D-01, 8.6400D-01, 8.3800D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 3.8400D-01, & ! 2   
     &     2.7900D-01, 8.4000D-01, 8.2800D-01, 8.2800D-01, 5.4800D-01, & ! +   
     &     2.5200D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00/           !6   

      DATA ( SC( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 6.1000D-01, 0.0000D+00, 6.0000D-01, & ! +   
     &     6.0000D-01, 6.0000D-01, 6.0000D-01, 1.0000D+00, 0.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.3800D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.4200D-01, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.7300D-01, 0.0000D+00, 6.7000D-01, 6.0000D-01, 4.6700D-01, & ! 3   
     &     4.0000D-01, 5.5400D-01, 2.0000D+00, 1.7800D-01, 1.0000D+00, & ! +   
     &     9.1000D-01, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     5.0000D-01, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! 8   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 8.0000D-01, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 7.5000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 7.5000D-01, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 4.5000D-02, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 0.0000D+00, & ! 2   
     &     1.0000D+00, 1.0250D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 0.0000D+00, 1.0000D+00, & ! 3   
     &     1.0250D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 2.0000D+00, 2.0000D+00, 0.0000D+00, 1.0000D+00, & ! 4   
     &     1.0250D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 3.5000D-02, 1.0000D+00, 1.0000D+00, 3.9000D-02, & ! 6   
     &     1.0000D+00, 1.0000D+00, 4.9100D-01, 1.0000D+00, 3.0000D-01, & ! +   
     &     2.5100D-01, 2.2200D-01, 1.4800D-01, 1.4000D+00, 7.0000D-01, & ! 7   
     &     1.0000D+00, 1.0000D+00, 8.0000D-01, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 7.2300D-01, 5.2200D-01, 7.2300D-01, & ! 8   
     &     5.2200D-01, 7.3300D-01, 5.5400D-01, 5.0000D-01, 1.0800D-01, & ! +   
     &     5.0000D-01, 0.0000D+00, 2.5000D-02, 6.4000D-02, 5.5000D-01, & ! 9   
     &     6.7000D-01, 4.0000D-01, 1.5000D-01, 3.7900D-01, 3.0500D-01, & ! +   
     &     0.0000D+00, 1.0000D+00, 7.5000D-01, 3.3000D-01, 9.6700D-01, & ! O   
     &     0.0000D+00, 2.0000D-02, 1.9400D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     4.4000D-01, 1.0000D+00, 1.6000D-01, 1.0000D+00, 2.9000D-01, & ! 1   
     &     9.8400D-01, 3.5000D-01, 9.4900D-01, 5.5000D-01, 1.1890D+00, & ! +   
     &     8.0000D-02, 1.2000D-01, 1.1700D-01, 9.8600D-01, 2.6600D-01, & ! 2   
     &     1.8700D-01, 2.4000D-01, 1.0000D+00, 1.0000D-01, 0.0000D+00, & ! +   
     &     2.7500D-01, 2.7500D-01, 1.3800D-01, 1.3800D-01, 2.7500D-01, & ! 3   
     &     4.0000D-03, 1.0200D-01, 6.4300D-01, 1.0000D+00, 7.0000D-01, & ! +   
     &     5.0000D-01, 2.9000D-01, 4.5400D-01, 5.2000D-01, 5.5400D-01, & ! 4   
     &     4.8700D-01, 6.2700D-01, 5.0000D-02, 1.0000D+00, 9.6500D-01, & ! +   
     &     2.3600D-01, 1.0000D-02, 1.6050D+00, 4.7000D-01, 1.0000D-03, & ! 5   
     &     5.7000D-02, 1.4630D+00, 3.9000D-01, 9.5300D-01, 4.1000D-02, & ! +   
     &     4.2600D-01, 7.5100D-01, 5.6600D-01, 6.1700D-01, 6.1700D-01, & ! 6   
     &     6.4000D-02, 4.6000D-02, 2.8700D-01, 7.6300D-01, 6.4000D-02, & ! +   
     &     4.6000D-02, 2.8700D-01, 7.6300D-01, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.4200D+00, & ! 8   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 9.0000D-01, & ! +   
     &     1.0000D+00, 9.7500D-01, 3.8000D-02, 3.1400D-01, 6.3000D-01, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 4.1400D-01, 1.4500D-01, & ! +   
     &     1.3900D-01, 2.7400D-01, 1.6500D-01, 5.3000D-02, 8.4000D-02, & ! O   
     &     0.0000D+00, 1.0000D+00, 2.0000D+00, 9.7100D-01, 5.4100D-01, & ! +   
     &     7.3800D-01, 2.5200D-01, 1.0000D+00, 8.9400D-01, 8.6400D-01, & ! 1   
     &     8.6400D-01, 8.6400D-01, 8.3800D-01, 6.8800D-01, 1.0000D+00, & ! +   
     &     9.7000D-01, 8.3500D-01, 8.2700D-01, 6.4700D-01, 8.7300D-01, & ! 2   
     &     4.5000D-01, 8.4000D-01, 8.2800D-01, 8.2800D-01, 2.5200D-01, & ! +   
     &     6.8000D-02, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 4.8570D-01, & ! 5   
     &     3.0030D-01, 3.8560D-01, 2.1810D-01, 2.4120D-01, 6.6640D-01, & ! +   
     &     2.8580D-01, 3.3030D-01, 3.4440D-01, 3.8860D-01/           !6   

      DATA ( SC( IRXXN,  3 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 3.9000D-01, 0.0000D+00, 4.0000D-01, & ! +   
     &     4.0000D-01, 4.0000D-01, 4.0000D-01, 0.0000D+00, 0.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 3.8000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 7.8200D-01, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! 2   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.0500D-01, 0.0000D+00, 3.4000D-01, 6.0000D-01, 3.0000D-01, & ! 3   
     &     6.0000D-01, 1.0000D+00, 1.0000D+00, 2.3400D-01, 1.0000D+00, & ! +   
     &     7.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D-01, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-01, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.5000D-01, 1.0000D+00, & ! +   
     &     9.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     1.0000D+00, 1.5000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 0.0000D+00, 1.0000D+00, & ! 3   
     &     1.5000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 0.0000D+00, 1.0000D+00, & ! 4   
     &     1.5000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.5000D-02, 0.0000D+00, 1.0000D+00, 3.9000D-02, & ! 6   
     &     0.0000D+00, 0.0000D+00, 5.0900D-01, 1.4300D-01, 7.0000D-01, & ! +   
     &     4.0000D-03, 2.9000D-02, 5.8900D-01, 3.0000D-01, 1.4000D+00, & ! 7   
     &     0.0000D+00, 1.0000D+00, 8.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 6.0000D-02, 6.5200D-01, 6.0000D-02, & ! 8   
     &     6.5200D-01, 1.1700D-01, 1.3000D-02, 5.0000D-01, 1.0000D-01, & ! +   
     &     5.0000D-01, 0.0000D+00, 2.5000D-02, 5.0000D-02, 0.0000D+00, & ! 9   
     &     6.7000D-01, 4.8000D-02, 7.9900D-01, 2.9000D-02, 1.9000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.5000D-01, 1.0050D+00, 3.1000D-02, & ! O   
     &     0.0000D+00, 2.0000D-02, 1.9400D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     4.4000D-01, 1.6100D+00, 5.1000D-01, 1.0000D+00, 5.1000D-01, & ! 1   
     &     1.6000D-02, 3.5500D-01, 5.1000D-02, 0.0000D+00, 4.9000D-02, & ! +   
     &     2.5500D-01, 1.0550D+00, 1.1800D-01, 9.3000D-02, 1.9200D-01, & ! 2   
     &     9.3600D-01, 2.4000D-01, 0.0000D+00, 3.5000D-01, 0.0000D+00, & ! +   
     &     2.7500D-01, 2.7500D-01, 1.3800D-01, 1.3800D-01, 2.7500D-01, & ! 3   
     &     1.0420D+00, 7.2800D-01, 7.0000D-03, 0.0000D+00, 3.0000D-01, & ! +   
     &     1.5000D+00, 1.1600D-01, 3.1200D-01, 2.3900D-01, 1.9800D-01, & ! 4   
     &     2.7800D-01, 2.3000D-01, 5.0000D-02, 1.0000D+00, 3.5000D-02, & ! +   
     &     1.2530D+00, 1.1000D-02, 3.5300D-01, 0.0000D+00, 1.2020D+00, & ! 5   
     &     1.2800D-01, 2.2800D-01, 1.6000D-01, 8.8000D-02, 4.4300D-01, & ! +   
     &     3.5000D-02, 1.7000D-01, 2.0200D-01, 1.7800D-01, 1.7800D-01, & ! 6   
     &     1.2110D+00, 4.9900D-01, 2.6000D-02, 1.0000D+00, 1.2110D+00, & ! +   
     &     4.9900D-01, 2.6000D-02, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D-01, & ! +   
     &     1.0000D+00, 3.9000D-02, 5.5000D-02, 6.8000D-01, 1.2600D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 5.8800D-01, 1.0780D+00, & ! +   
     &     1.4800D-01, 2.1600D-01, 8.0200D-01, 5.3000D-02, 1.5400D-01, & ! O   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 9.7100D-01, 1.8840D+00, & ! +   
     &     1.7700D-01, 6.8000D-02, 0.0000D+00, 1.0600D-01, 1.3600D-01, & ! 1   
     &     1.3600D-01, 1.3600D-01, 1.6200D-01, 3.1200D-01, 1.0000D+00, & ! +   
     &     9.7000D-01, 9.4000D-02, 3.0000D-03, 1.5410D+00, 1.6080D+00, & ! 2   
     &     4.4200D-01, 1.6000D-01, 1.7200D-01, 1.7200D-01, 6.8000D-02, & ! +   
     &     3.4000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 6.2000D-03, & ! 5   
     &     2.8620D-01, 9.5000D-02, 3.0630D-01, 2.0890D-01, 1.4300D-02, & ! +   
     &     3.9310D-01, 2.2720D-01, 2.7490D-01, 2.4210D-01/           !6   

      DATA ( SC( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 3.9000D-01, 0.0000D+00, 4.0000D-01, & ! +   
     &     4.0000D-01, 4.0000D-01, 4.0000D-01, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 7.7000D-02, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     5.0000D-01, 0.0000D+00, 3.3000D-01, 4.0000D-01, 1.2330D+00, & ! 3   
     &     1.5900D+00, 7.2100D-01, 0.0000D+00, 3.3000D-01, 0.0000D+00, & ! +   
     &     4.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.4000D-01, 0.0000D+00, & ! +   
     &     9.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     1.0000D+00, 4.4000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 0.0000D+00, 1.0000D+00, & ! 3   
     &     4.4000D-01, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! 4   
     &     4.4000D-01, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.5000D-02, 0.0000D+00, 1.0000D+00, 3.7600D-01, & ! 6   
     &     0.0000D+00, 0.0000D+00, 4.9100D-01, 1.4200D-01, 0.0000D+00, & ! +   
     &     4.0000D-03, 2.9000D-02, 1.2400D-01, 0.0000D+00, 3.0000D-01, & ! 7   
     &     0.0000D+00, 0.0000D+00, 8.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 6.0000D-02, 5.2200D-01, 6.0000D-02, & ! 8   
     &     5.2200D-01, 1.1700D-01, 2.5800D-01, 4.1600D-01, 4.5000D-01, & ! +   
     &     5.0000D-01, 0.0000D+00, 3.0000D-01, 5.0000D-02, 0.0000D+00, & ! 9   
     &     4.1000D-02, 4.8000D-02, 7.9900D-01, 4.9000D-02, 3.1300D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.6700D-01, 3.1000D-01, 2.0000D-03, & ! O   
     &     0.0000D+00, 2.0000D-02, 1.1000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     4.4000D-01, 1.9500D-01, 1.2000D-01, 1.0000D+00, 2.9000D-01, & ! 1   
     &     1.6000D-02, 5.2500D-01, 5.1000D-02, 0.0000D+00, 4.9000D-02, & ! +   
     &     1.8500D-01, 6.5000D-02, 2.3500D-01, 9.3000D-02, 1.9200D-01, & ! 2   
     &     6.4000D-02, 1.0000D-02, 0.0000D+00, 5.0000D-02, 0.0000D+00, & ! +   
     &     2.7500D-01, 2.7500D-01, 1.3800D-01, 1.3800D-01, 2.7500D-01, & ! 3   
     &     1.9700D-01, 1.0000D-03, 1.0500D+00, 0.0000D+00, 3.0000D-01, & ! +   
     &     5.0000D-01, 2.9000D-01, 4.5400D-01, 5.2000D-01, 5.5400D-01, & ! 4   
     &     4.8700D-01, 6.2700D-01, 8.1000D-02, 1.0000D+00, 3.5000D-02, & ! +   
     &     7.0000D-02, 1.7630D+00, 3.5300D-01, 0.0000D+00, 1.2800D-01, & ! 5   
     &     9.0000D-02, 2.2800D-01, 0.0000D+00, 8.8000D-02, 3.0700D-01, & ! +   
     &     1.1930D+00, 0.0000D+00, 5.6600D-01, 6.1700D-01, 6.1700D-01, & ! 6   
     &     2.0100D-01, 2.0200D-01, 1.7860D+00, 0.0000D+00, 2.0100D-01, & ! +   
     &     2.0200D-01, 1.7860D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-01, & ! +   
     &     1.0000D+00, 3.9000D-02, 1.2820D+00, 1.1600D-01, 3.7000D-01, & ! 9   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 4.1400D-01, 1.1700D-01, & ! +   
     &     5.8900D-01, 1.0320D+00, 3.3000D-02, 3.2200D-01, 7.3000D-01, & ! O   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 2.9000D-02, 6.9000D-02, & ! +   
     &     1.1680D+00, 3.4000D-02, 0.0000D+00, 1.0600D-01, 1.3600D-01, & ! 1   
     &     1.3600D-01, 1.3600D-01, 1.6200D-01, 3.1200D-01, 1.0000D+00, & ! +   
     &     3.0000D-02, 1.3610D+00, 4.0000D-03, 3.5200D-01, 1.2700D-01, & ! 2   
     &     1.0000D-03, 1.6000D-01, 1.7200D-01, 1.7200D-01, 3.4000D-02, & ! +   
     &     5.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-03, & ! 5   
     &     4.1000D-03, 1.3730D-01, 1.5300D-02, 3.0000D-01, 1.2300D-02, & ! +   
     &     1.3900D-02, 2.6070D-01, 4.9100D-02, 6.4000D-02/           !6   

      DATA ( SC( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.0000D-01, & ! +   
     &     4.0000D-01, 4.0000D-01, 4.0000D-01, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 7.7000D-02, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     6.9500D-01, 0.0000D+00, 3.3000D-01, 0.0000D+00, 3.0000D-01, & ! 3   
     &     8.7000D-02, 1.0200D-01, 0.0000D+00, 1.1880D+00, 0.0000D+00, & ! +   
     &     2.9000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.4000D-01, 0.0000D+00, & ! +   
     &     9.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     1.0000D+00, 4.4000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     4.4000D-01, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     4.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 2.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.5000D-02, 0.0000D+00, 0.0000D+00, 5.1000D-01, & ! 6   
     &     0.0000D+00, 0.0000D+00, 4.9100D-01, 4.0000D-01, 0.0000D+00, & ! +   
     &     7.4400D-01, 8.4000D-01, 1.2400D-01, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 2.5000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.2100D-01, 1.7400D-01, 5.2100D-01, & ! 8   
     &     1.7400D-01, 5.6100D-01, 7.0000D-03, 8.4000D-02, 1.1700D-01, & ! +   
     &     5.0000D-01, 0.0000D+00, 6.7500D-01, 4.7500D-01, 0.0000D+00, & ! 9   
     &     4.1000D-02, 4.9800D-01, 5.1000D-02, 4.7300D-01, 9.7600D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 8.3000D-02, 5.0000D-01, 2.0000D-03, & ! O   
     &     0.0000D+00, 2.0000D-02, 1.1000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     5.6000D-01, 1.0000D+00, 1.0000D+00, 0.0000D+00, 5.1000D-01, & ! 1   
     &     9.8400D-01, 2.1500D-01, 1.0000D+00, 0.0000D+00, 7.0800D-01, & ! +   
     &     5.0000D-01, 6.5000D-02, 1.5000D-02, 6.2400D-01, 8.0000D-03, & ! 2   
     &     6.4000D-02, 1.0000D-02, 0.0000D+00, 2.6000D-01, 0.0000D+00, & ! +   
     &     1.1250D+00, 1.2500D-01, 6.3000D-02, 6.3000D-02, 1.2500D-01, & ! 3   
     &     1.9700D-01, 2.9700D-01, 2.9300D-01, 0.0000D+00, 7.0000D-01, & ! +   
     &     0.0000D+00, 2.4000D-02, 5.4000D-02, 8.2000D-02, 8.7000D-02, & ! 4   
     &     7.6000D-02, 1.2100D-01, 9.5000D-01, 0.0000D+00, 2.6100D-01, & ! +   
     &     7.0000D-02, 1.4900D-01, 4.0000D-02, 0.0000D+00, 1.2800D-01, & ! 5   
     &     5.0000D-03, 1.3000D-02, 0.0000D+00, 1.7900D-01, 1.5600D-01, & ! +   
     &     1.4000D-01, 0.0000D+00, 1.1000D-01, 1.2800D-01, 1.2800D-01, & ! 6   
     &     2.0100D-01, 5.9000D-02, 4.6000D-01, 0.0000D+00, 2.0100D-01, & ! +   
     &     5.9000D-02, 4.6000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-01, & ! +   
     &     1.0000D+00, 8.4000D-01, 2.0200D-01, 1.1600D-01, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0400D-01, 1.1700D-01, & ! +   
     &     1.2400D-01, 2.6000D-02, 3.3000D-02, 6.2500D-01, 5.1000D-02, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.9000D-02, 6.9000D-02, & ! +   
     &     8.5000D-02, 5.0000D-02, 0.0000D+00, 8.9400D-01, 8.6400D-01, & ! 1   
     &     8.6400D-01, 8.6400D-01, 8.3800D-01, 5.0300D-01, 1.0000D+00, & ! +   
     &     3.0000D-02, 7.0000D-02, 1.7370D+00, 3.5200D-01, 1.2700D-01, & ! 2   
     &     1.4920D+00, 8.4000D-01, 4.6900D-01, 4.6900D-01, 5.0000D-02, & ! +   
     &     1.6000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.6000D-03, & ! 5   
     &     3.5000D-03, 5.0000D-04, 1.0430D-01, 2.0280D-01, 1.2390D-01, & ! +   
     &     1.0270D-01, 7.0200D-02, 2.5770D-01, 3.8500D-02/           !6   

      DATA ( SC( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     4.0000D-01, 4.0000D-01, 4.0000D-01, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 8.5000D-02, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.9500D-01, 0.0000D+00, 6.7000D-01, 0.0000D+00, 4.6700D-01, & ! 3   
     &     8.7000D-02, 1.0200D-01, 0.0000D+00, 1.0200D-01, 0.0000D+00, & ! +   
     &     9.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.4000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     1.0000D+00, 4.4000D-01, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     4.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     4.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.5000D-02, 0.0000D+00, 0.0000D+00, 7.4000D-02, & ! 6   
     &     0.0000D+00, 0.0000D+00, 4.9100D-01, 4.5700D-01, 0.0000D+00, & ! +   
     &     2.3900D-01, 9.0000D-02, 7.4000D-02, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.0100D-01, 4.3200D-01, 2.0100D-01, & ! 8   
     &     4.3200D-01, 1.1700D-01, 7.0000D-03, 4.1600D-01, 1.0000D-01, & ! +   
     &     5.0000D-01, 0.0000D+00, 3.0000D-01, 1.2400D-01, 0.0000D+00, & ! 9   
     &     3.3600D-01, 1.4000D-01, 5.1000D-02, 7.1000D-02, 1.7500D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.6700D-01, 1.8500D-01, 9.6700D-01, & ! O   
     &     0.0000D+00, 2.0000D-02, 1.1000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.5000D-01, 0.0000D+00, 3.7000D-01, 0.0000D+00, 2.7800D-01, & ! 1   
     &     9.8400D-01, 5.0000D-01, 0.0000D+00, 0.0000D+00, 4.8000D-01, & ! +   
     &     1.8500D-01, 1.1500D-01, 1.5000D-02, 2.3000D-01, 8.0000D-03, & ! 2   
     &     9.3600D-01, 2.4000D-01, 0.0000D+00, 4.0000D-02, 0.0000D+00, & ! +   
     &     8.2500D-01, 8.2500D-01, 9.1300D-01, 4.1300D-01, 8.2500D-01, & ! 3   
     &     2.0000D-03, 1.5110D+00, 2.9300D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.4000D-02, 5.4000D-02, 8.2000D-02, 8.7000D-02, & ! 4   
     &     7.6000D-02, 1.2100D-01, 1.0000D-02, 0.0000D+00, 7.0400D-01, & ! +   
     &     2.6000D-02, 1.4900D-01, 1.0600D-01, 0.0000D+00, 5.8200D-01, & ! 5   
     &     5.0000D-03, 3.0000D-03, 0.0000D+00, 8.3500D-01, 8.0000D-03, & ! +   
     &     1.4000D-01, 0.0000D+00, 1.1000D-01, 1.2800D-01, 1.2800D-01, & ! 6   
     &     1.0000D-03, 4.9000D-01, 4.6000D-01, 0.0000D+00, 1.0000D-03, & ! +   
     &     4.9000D-01, 4.6000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-01, & ! +   
     &     0.0000D+00, 8.5000D-02, 2.0200D-01, 1.9800D-01, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.8200D-01, 1.4500D-01, & ! +   
     &     1.2400D-01, 2.6000D-02, 8.0200D-01, 9.4700D-01, 5.1000D-02, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.2400D-01, 8.6300D-01, & ! +   
     &     8.5000D-02, 1.6000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 6.8800D-01, 0.0000D+00, & ! +   
     &     4.8200D-01, 7.0000D-02, 1.6500D-01, 2.2000D-02, 3.6000D-02, & ! 2   
     &     1.0600D-01, 0.0000D+00, 3.5900D-01, 3.5900D-01, 1.6000D-02, & ! +   
     &     2.2580D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.3000D-03, & ! 5   
     &     2.2390D-01, 2.0510D-01, 1.8930D-01, 4.7100D-02, 1.8310D-01, & ! +   
     &     2.0450D-01, 1.1160D-01, 7.3900D-02, 2.6670D-01/           !6   

      DATA ( SC( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     4.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.4200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.0500D-01, 0.0000D+00, 3.4000D-01, 0.0000D+00, 2.3300D-01, & ! 3   
     &     3.0300D-01, 7.4000D-02, 0.0000D+00, 3.4000D-01, 0.0000D+00, & ! +   
     &     1.1000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 4.4000D-01, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     4.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     4.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 8.8000D-02, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.2000D-02, 4.1000D-02, 1.4700D-01, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.3400D-01, 5.6800D-01, 3.3400D-01, & ! 8   
     &     5.6800D-01, 1.1400D-01, 5.8000D-01, 8.4000D-02, 9.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 6.7500D-01, 5.0000D-02, 0.0000D+00, & ! 9   
     &     5.5000D-02, 1.2400D-01, 5.7200D-01, 7.1000D-02, 1.7500D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 8.3000D-02, 5.0000D-01, 3.1000D-02, & ! O   
     &     0.0000D+00, 0.0000D+00, 8.4000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.7800D-01, & ! 1   
     &     1.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, 4.7100D-01, & ! +   
     &     5.0000D-01, 4.6000D-01, 1.1500D-01, 3.2000D-01, 2.7500D-01, & ! 2   
     &     1.0000D+00, 7.5000D-01, 0.0000D+00, 3.1000D-01, 0.0000D+00, & ! +   
     &     2.0000D-01, 2.0000D-01, 1.0000D-01, 1.0000D-01, 1.2000D+00, & ! 3   
     &     2.2000D-02, 3.3700D-01, 5.0000D-03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.9000D-01, 2.3800D-01, 1.0000D-01, 8.4000D-02, & ! 4   
     &     2.8600D-01, 7.4000D-02, 5.0000D-02, 0.0000D+00, 1.0000D+00, & ! +   
     &     4.4500D-01, 2.0000D-03, 2.0900D-01, 0.0000D+00, 1.0000D-02, & ! 5   
     &     3.0300D-01, 3.4000D-02, 0.0000D+00, 5.1000D-01, 2.1200D-01, & ! +   
     &     7.2000D-02, 0.0000D+00, 1.5800D-01, 8.8000D-02, 8.8000D-02, & ! 6   
     &     4.1100D-01, 1.2100D-01, 1.2000D-02, 0.0000D+00, 4.1100D-01, & ! +   
     &     1.2100D-01, 1.2000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-01, & ! +   
     &     0.0000D+00, 3.6000D-02, 9.0000D-03, 1.1600D-01, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0600D-01, 5.0200D-01, & ! +   
     &     7.4000D-02, 2.1600D-01, 5.4100D-01, 1.0000D+00, 4.2000D-02, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0600D-01, 4.5700D-01, & ! +   
     &     2.7500D-01, 2.2580D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 6.1000D-02, 0.0000D+00, & ! +   
     &     4.8800D-01, 7.8000D-02, 1.6500D-01, 8.0000D-02, 2.0600D-01, & ! 2   
     &     1.0600D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.2580D+00, & ! +   
     &     5.8200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.9440D-01, & ! 5   
     &     1.8200D-01, 1.7640D-01, 1.6680D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     4.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 7.8200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.3000D-01, 0.0000D+00, 0.0000D+00, & ! 3   
     &     1.6300D-01, 6.1000D-02, 0.0000D+00, 5.0000D-02, 0.0000D+00, & ! +   
     &     5.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 4.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0400D-01, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.2000D-02, 2.0000D-02, 1.3900D-01, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.0700D-01, 6.5200D-01, 4.0700D-01, & ! 8   
     &     6.5200D-01, 2.7400D-01, 1.9000D-01, 5.0000D-01, 3.3300D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.0000D-01, 9.5000D-01, 0.0000D+00, & ! 9   
     &     1.2900D-01, 2.1000D-01, 2.2700D-01, 2.0000D-03, 1.1000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.5000D-01, 0.0000D+00, 3.1000D-02, & ! O   
     &     0.0000D+00, 0.0000D+00, 8.4000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-01, & ! 1   
     &     0.0000D+00, 1.8500D-01, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     3.7500D-01, 1.2000D-01, 1.1500D-01, 3.5700D-01, 1.2200D-01, & ! 2   
     &     0.0000D+00, 2.5000D-01, 0.0000D+00, 2.0000D-02, 0.0000D+00, & ! +   
     &     3.7500D-01, 3.7500D-01, 9.3800D-01, 1.8800D-01, 3.7500D-01, & ! 3   
     &     7.7600D-01, 3.3700D-01, 7.0000D-03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 5.7000D-01, 1.5100D-01, 3.8000D-01, 2.3800D-01, & ! 4   
     &     1.1200D-01, 4.0500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.2200D-01, 2.9000D-02, 7.1000D-02, 0.0000D+00, 7.0000D-03, & ! 5   
     &     8.8000D-02, 7.7400D-01, 0.0000D+00, 1.4400D-01, 3.0000D-03, & ! +   
     &     5.7900D-01, 0.0000D+00, 1.0000D-01, 3.1200D-01, 3.1200D-01, & ! 6   
     &     3.8500D-01, 1.2100D-01, 2.3000D-02, 0.0000D+00, 3.8500D-01, & ! +   
     &     1.2100D-01, 2.3000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 6.5000D-02, 1.8000D-02, 5.4100D-01, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0400D-01, 2.3700D-01, & ! +   
     &     1.4700D-01, 4.8400D-01, 8.2000D-02, 0.0000D+00, 4.2000D-02, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.4000D-01, 4.7300D-01, & ! +   
     &     1.7700D-01, 5.8200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.1200D-01, 0.0000D+00, & ! +   
     &     1.0000D+00, 3.4000D-01, 3.0000D-03, 2.5800D-01, 7.2000D-02, & ! 2   
     &     1.9000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 5.8200D-01, & ! +   
     &     5.8200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0210D-01, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.6000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.3000D-01, 0.0000D+00, 0.0000D+00, & ! 3   
     &     7.8000D-01, 2.1400D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 4.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.7600D-01, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.2000D-02, 7.5000D-02, 5.6500D-01, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.2900D-01, 6.5200D-01, 1.2900D-01, & ! 8   
     &     6.5200D-01, 1.5300D-01, 3.6600D-01, 0.0000D+00, 1.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 3.5100D-01, 0.0000D+00, & ! 9   
     &     1.3000D-02, 2.3000D-02, 2.1800D-01, 2.1100D-01, 4.2900D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.3000D-02, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.2000D-02, & ! 1   
     &     0.0000D+00, 7.5000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.2500D-01, 3.5500D-01, 1.0000D-03, 1.0000D+00, 4.0000D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.8700D-01, 0.0000D+00, & ! +   
     &     7.4000D-02, 7.4000D-02, 3.7000D-02, 3.7000D-02, 7.4000D-02, & ! 3   
     &     3.4000D-02, 2.9000D-02, 6.8400D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.9000D-02, 1.8100D-01, 1.5900D-01, 1.8500D-01, & ! 4   
     &     1.5900D-01, 1.1200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.4000D-02, 4.3800D-01, 8.6000D-02, 0.0000D+00, 6.6600D-01, & ! 5   
     &     5.0000D-01, 1.6900D-01, 0.0000D+00, 8.0000D-02, 3.0000D-03, & ! +   
     &     1.6300D-01, 0.0000D+00, 1.2300D-01, 1.3400D-01, 1.3400D-01, & ! 6   
     &     3.7000D-02, 2.4900D-01, 2.0000D-03, 0.0000D+00, 3.7000D-02, & ! +   
     &     2.4900D-01, 2.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 7.0000D-02, 1.2000D-02, 7.0000D-03, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.9700D-01, 1.8600D-01, & ! +   
     &     1.3900D-01, 2.7400D-01, 1.8000D-01, 0.0000D+00, 7.1200D-01, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     6.7100D-01, 5.8200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.4300D-01, 3.4000D-02, 4.4000D-02, 2.1500D-01, & ! 2   
     &     3.8300D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 5.8200D-01, & ! +   
     &     5.4800D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.9000D-03, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 5.8000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.3000D-01, 0.0000D+00, 0.0000D+00, & ! 3   
     &     1.0000D+00, 2.3000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.0500D-01, 8.4000D-02, 2.4000D-02, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0700D-01, 6.5200D-01, 1.0700D-01, & ! 8   
     &     6.5200D-01, 1.9000D-02, 1.8400D-01, 0.0000D+00, 1.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-02, 0.0000D+00, & ! 9   
     &     1.5000D-01, 7.4200D-01, 8.0000D-03, 1.0000D-03, 1.0000D-03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.9000D-01, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D-03, 1.0000D+00, 1.9200D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 6.1000D-01, 0.0000D+00, & ! +   
     &     2.5100D-01, 2.5100D-01, 1.2600D-01, 1.2600D-01, 2.5100D-01, & ! 3   
     &     2.0000D-02, 5.1000D-02, 6.9000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.6100D-01, 6.5000D-02, 4.1000D-02, 1.6100D-01, & ! 4   
     &     8.8000D-02, 2.2000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.3200D-01, 2.3600D-01, 4.0700D-01, 0.0000D+00, 7.0000D-03, & ! 5   
     &     1.1000D-02, 8.3100D-01, 0.0000D+00, 2.0000D-03, 2.9900D-01, & ! +   
     &     1.1600D-01, 0.0000D+00, 7.2000D-02, 7.7000D-02, 7.7000D-02, & ! 6   
     &     7.0000D-03, 6.3000D-02, 4.0300D-01, 0.0000D+00, 7.0000D-03, & ! +   
     &     6.3000D-02, 4.0300D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 8.4000D-01, 5.5000D-02, 2.2000D-02, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.8500D-01, 6.7600D-01, & ! +   
     &     5.6500D-01, 2.7400D-01, 5.4100D-01, 0.0000D+00, 4.9800D-01, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     6.7000D-02, 3.5000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 7.5000D-02, 2.8700D-01, 4.1000D-02, 1.9000D-02, & ! 2   
     &     3.1700D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.5000D-02, & ! +   
     &     3.5000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.3000D-03, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 11 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 6.9800D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 7.4000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.4000D-02, 1.6000D-01, 4.4800D-01, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.6700D-01, 0.0000D+00, 2.6700D-01, & ! 8   
     &     0.0000D+00, 1.9500D-01, 3.5000D-01, 0.0000D+00, 1.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-02, 0.0000D+00, & ! 9   
     &     3.3200D-01, 1.0000D-01, 5.7200D-01, 8.3000D-02, 3.6000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 7.5000D-01, 0.0000D+00, 2.0400D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 6.1000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 5.0000D-01, 5.0000D-01, 1.0000D+00, & ! 3   
     &     2.3000D-02, 1.7000D-02, 2.0000D-03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.1600D-01, 1.9500D-01, 3.3600D-01, 4.7000D-02, & ! 4   
     &     4.5000D-02, 3.6000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     9.8300D-01, 4.2600D-01, 1.0000D+00, 0.0000D+00, 3.6000D-02, & ! 5   
     &     5.0000D-01, 0.0000D+00, 0.0000D+00, 1.2000D-02, 1.6100D-01, & ! +   
     &     2.0000D-03, 0.0000D+00, 1.8500D-01, 2.6000D-02, 2.6000D-02, & ! 6   
     &     3.0000D-03, 1.2700D-01, 2.3900D-01, 0.0000D+00, 3.0000D-03, & ! +   
     &     1.2700D-01, 2.3900D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.5900D-01, 2.3700D-01, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.8600D-01, 2.8000D-01, & ! +   
     &     2.4000D-02, 4.8400D-01, 8.3500D-01, 0.0000D+00, 1.9500D-01, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.5800D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.5300D-01, 4.1200D-01, 3.7800D-01, 3.8000D-02, & ! 2   
     &     8.6000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.5800D-01, & ! +   
     &     1.5800D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 12 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 8.5800D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 6.3000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.5600D-01, 0.0000D+00, 2.6000D-02, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 7.8300D-01, 0.0000D+00, 7.8300D-01, & ! 8   
     &     0.0000D+00, 1.9500D-01, 3.5000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-02, 0.0000D+00, & ! 9   
     &     1.5000D-01, 3.7200D-01, 8.5000D-01, 1.4300D-01, 4.0000D-03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.5000D-01, 0.0000D+00, 3.9000D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.5000D-01, 0.0000D+00, 0.0000D+00, & ! 3   
     &     1.0000D+00, 3.4400D-01, 5.6000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.1400D-01, 1.9500D-01, 1.4400D-01, 2.5300D-01, & ! 4   
     &     6.7000D-02, 8.8000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.7000D-02, 1.0600D-01, 0.0000D+00, 0.0000D+00, 1.0000D-03, & ! 5   
     &     4.4000D-02, 0.0000D+00, 0.0000D+00, 2.3000D-02, 1.3100D-01, & ! +   
     &     3.2000D-01, 0.0000D+00, 2.0200D-01, 2.2100D-01, 2.2100D-01, & ! 6   
     &     9.0000D-03, 3.3000D-02, 5.0000D-03, 0.0000D+00, 9.0000D-03, & ! +   
     &     3.3000D-02, 5.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.4700D-01, 1.0900D-01, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 8.5500D-01, & ! +   
     &     4.4800D-01, 7.8400D-01, 0.0000D+00, 0.0000D+00, 1.7000D-02, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.8500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 9.8300D-01, 2.4700D-01, 1.0000D+00, 1.9200D-01, & ! 2   
     &     4.2000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.8500D-01, & ! +   
     &     1.8500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 13 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 8.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.0000D-02, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 2.3100D-01, 1.3900D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     1.7400D-01, 4.7000D-02, 0.0000D+00, 4.0200D-01, 1.0000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.6000D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     1.0000D+00, 2.4000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 3.1200D-01, 2.3900D-01, 2.5300D-01, & ! 4   
     &     2.7800D-01, 3.5200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.4600D-01, 0.0000D+00, 0.0000D+00, 1.2000D-02, & ! 5   
     &     3.0000D-03, 0.0000D+00, 0.0000D+00, 3.1900D-01, 1.1400D-01, & ! +   
     &     3.1900D-01, 0.0000D+00, 3.0900D-01, 2.4700D-01, 2.4700D-01, & ! 6   
     &     3.0000D-03, 2.0800D-01, 1.0000D-03, 0.0000D+00, 3.0000D-03, & ! +   
     &     2.0800D-01, 1.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.5000D-02, 5.9100D-01, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.6000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 9.0000D-03, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.7400D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.7000D-02, 7.6000D-02, 0.0000D+00, 3.3700D-01, & ! 2   
     &     2.5000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.7400D-01, & ! +   
     &     2.7400D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 14 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 1.2400D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.5200D-01, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 7.9400D-01, 3.0000D-03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     7.1100D-01, 1.0000D-03, 0.0000D+00, 1.1500D-01, 1.7000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.5000D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 3.4500D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 7.3000D-02, 4.7000D-02, 1.9800D-01, & ! 4   
     &     2.8600D-01, 2.3000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 9.0000D-03, & ! 5   
     &     9.0000D-03, 0.0000D+00, 0.0000D+00, 6.8100D-01, 4.5300D-01, & ! +   
     &     6.8100D-01, 0.0000D+00, 3.6900D-01, 1.7800D-01, 1.7800D-01, & ! 6   
     &     2.0000D-03, 5.7000D-02, 4.0000D-03, 0.0000D+00, 2.0000D-03, & ! +   
     &     5.7000D-02, 4.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.0000D-01, 5.1000D-02, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 9.0000D-03, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 7.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.3000D-01, 0.0000D+00, 1.6900D-01, & ! 2   
     &     5.8000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.0000D-03, & ! +   
     &     7.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 15 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 8.3000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 7.3000D-02, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 4.0000D-03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 4.8000D-02, 0.0000D+00, 3.2900D-01, 8.0000D-03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 8.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.3500D-01, 5.5500D-01, 5.5000D-02, & ! 4   
     &     1.0200D-01, 1.5100D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.6800D-01, & ! 5   
     &     1.8500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.1000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 6.8000D-02, 6.8000D-02, & ! 6   
     &     4.0900D-01, 2.0000D-03, 2.2800D-01, 0.0000D+00, 4.0900D-01, & ! +   
     &     2.0000D-03, 2.2800D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.0000D-02, 4.0000D-02, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.5200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.1500D-01, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 8.3100D-01, & ! 2   
     &     1.6100D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0000D-03, & ! +   
     &     3.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 16 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 1.9000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 7.3000D-02, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 3.0000D-03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 7.0000D-03, 3.1000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 2.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 5.8600D-01, & ! 4   
     &     4.6100D-01, 4.3000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.6900D-01, & ! 5   
     &     1.5900D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.3300D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.7000D-02, 5.7000D-02, & ! 6   
     &     1.0000D+00, 1.7200D-01, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     1.7200D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.0000D-03, 6.8600D-01, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     7.3000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.4000D-01, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     1.3000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0000D-03, & ! +   
     &     3.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 17 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 2.6100D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 7.1300D-01, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 9.5000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.2800D-01, 1.8900D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 8.1000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 7.0500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 8.3100D-01, & ! 5   
     &     2.6800D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.9000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0100D-01, 1.0100D-01, & ! 6   
     &     1.0000D+00, 6.8000D-02, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     6.8000D-02, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.1000D-02, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     7.3000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.2000D-01, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.5800D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     1.9100D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.5800D-01, & ! +   
     &     1.5800D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 18 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 6.6000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 1.6300D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0500D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 2.5500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     1.1000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 5.1000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     0.0000D+00, 3.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.6000D-02, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     7.1300D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.6200D-01, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 6.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     3.1900D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 6.0000D-03, & ! +   
     &     6.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 19 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 5.9100D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 1.6300D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.5700D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 7.3700D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     5.2000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.3000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 3.9000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.9000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.4700D-01, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 6.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     6.8100D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 6.0000D-03, & ! +   
     &     6.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 20 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 9.5000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 6.3600D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 2.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 9.0800D-01, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-03, & ! +   
     &     1.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 21 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 2.6400D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.4000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 1.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0900D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0900D-01, & ! +   
     &     1.0900D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 22 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 6.5000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 5.0200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     5.0200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 23 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.3500D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 4.2800D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     4.2800D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 24 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.7000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 25 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.3000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( SC( IRXXN, 26 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.3600D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      INTEGER            :: NREACT( NRXNS )

      DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 9   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2/     !  6   
      INTEGER            :: NPRDCT( NRXNS )

      DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    1,    2,    1,    1,    2,    2,    4,    1,    5, & ! O   
     &      8,    6,    6,    2,    1,    3,    6,    3,    5,    3, & ! 1   
     &      3,   12,    6,    2,    2,    3,    1,    1,    0,    0, & ! 2   
     &      7,    1,   10,    4,    7,   10,   19,    3,    8,    3, & ! 3   
     &      8,    1,    2,    2,    2,    2,    2,    2,    6,    5, & ! 4   
     &      0,    0,    0,    0,    0,    1,    2,    1,    1,    1, & ! 5   
     &      1,    2,    1,    2,    2,    2,    2,    2,    3,    1, & ! 6   
     &      0,    1,    0,    1,    0,    1,    0,    1,    0,    1, & ! 7   
     &      0,    1,    0,    1,    0,    1,    0,    1,    0,    1, & ! 8   
     &      0,    1,    0,    1,    0,    1,    0,    1,    0,    1, & ! 9   
     &      0,    1,    0,    1,    0,    1,    0,    1,    0,    1, & ! O   
     &      0,    1,    0,    1,    0,    1,    0,    1,    0,    1, & ! 1   
     &      0,    1,    2,    0,    1,    1,    0,    1,    1,    0, & ! 2   
     &      1,    1,    0,    1,    1,    0,    1,    0,    1,    0, & ! 3   
     &      1,    2,    2,    1,    0,    1,    0,    1,    0,    2, & ! 4   
     &      2,    2,    2,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    0,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      2,    1,    1,    1,    2,    1,    2,    1,    2,    1, & ! 8   
     &      1,    1,    1,    1,    3,    1,    1,    0,    3,    3, & ! 9   
     &      1,    1,    3,    2,    2,    1,    0,    1,    3,    0, & ! O   
     &      0,    0,    1,    3,    0,    0,    1,    3,    6,    3, & ! 1   
     &      5,    2,    2,    2,    1,    6,    9,    6,    7,    5, & ! 2   
     &      5,    6,    5,    1,    4,    7,    4,    5,    3,    3, & ! 3   
     &      4,    6,    3,    1,    4,    7,    4,    4,    3,    3, & ! 4   
     &      4,    7,    5,    3,    1,    1,    1,    2,    3,    1, & ! 5   
     &      2,    6,    2,    4,   10,    2,    2,    6,    7,    3, & ! 6   
     &     12,   11,   17,    3,    4,    2,    3,    5,    2,    1, & ! 7   
     &      1,    2,   12,   10,   12,   10,   14,   21,    8,   11, & ! 8   
     &      6,    1,    9,   12,    2,   14,   15,   12,   17,   20, & ! 9   
     &      1,    2,    8,    7,    9,    1,    6,    8,    4,    4, & ! O   
     &      6,    5,    6,    4,   10,    7,    9,    5,    2,    8, & ! 1   
     &      9,   10,   12,   10,   16,    7,    8,    2,   11,    1, & ! 2   
     &     10,   11,   12,   11,   11,   13,   20,   14,    2,    5, & ! 3   
     &      4,   13,   16,   16,   17,   17,   18,    7,    4,    7, & ! 4   
     &     12,   14,   11,    2,   17,   19,   10,    3,   14,   26, & ! 5   
     &     14,    3,   15,   18,   18,   17,   24,   17,    3,   17, & ! 6   
     &     24,   17,    3,    1,    1,    1,    1,    2,    1,    2, & ! 7   
     &      2,    1,    2,    1,    2,    1,    3,    2,    3,    7, & ! 8   
     &      5,   11,   20,   16,    4,    3,    4,    2,   11,   12, & ! 9   
     &     18,   12,   11,    7,   18,    1,    2,    4,    9,    9, & ! O   
     &     11,   22,    2,    5,    5,    5,    5,    5,    8,    5, & ! 1   
     &      8,   13,   15,   12,   15,   19,    5,    6,    6,   22, & ! 2   
     &     22,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    2,   10,    7,    7,    7,    6,    6, & ! 5   
     &      6,    6,    6,    6/     !  6   

      INTEGER, PARAMETER :: MHETERO =   9
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    151,  152,  153,  154,  155,  171,  451,  452,  453/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2,    3,    4,    5,    8,    6,    6,    7/

      INTEGER, PARAMETER :: NPHOTAB =  38
      CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

      DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
     &   'NO2_06          ', 'NO3NO_06        ', 'NO3NO2_6        ', & 
     &   'O3O1D_06        ', 'O3O3P_06        ', 'HONO_06         ', & 
     &   'HNO3            ', 'HNO4_06         ', 'H2O2            ', & 
     &   'PAN             ', 'HCHOR_06        ', 'HCHOM_06        ', & 
     &   'CCHO_R          ', 'C2CHO           ', 'ACET_06         ', & 
     &   'MEK_06          ', 'COOH            ', 'GLY_07R         ', & 
     &   'GLY_07M         ', 'MGLY_06         ', 'BACL_07         ', & 
     &   'BALD_06         ', 'AFG1            ', 'MACR_06         ', & 
     &   'MVK_06          ', 'IC3ONO2         ', 'HOCCHO_IUPAC    ', & 
     &   'ACRO_09         ', 'PAA             ', 'CL2             ', & 
     &   'CLNO_06         ', 'CLONO           ', 'CLNO2           ', & 
     &   'CLONO2_1        ', 'CLONO2_2        ', 'HOCL_06         ', & 
     &   'CLCCHO          ', 'CLACET          '/

      INTEGER, PARAMETER :: NHETERO =   8
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_NO2      ', 'HETERO_N2O5IJ   ', 'HETERO_N2O5K    ', &
     &   'HETERO_H2NO3PAIJ', 'HETERO_H2NO3PAK ', 'HETERO_H2NO3PBIJ', &
     &   'HETERO_H2NO3PBK ', 'HETERO_IEPOX    '/

      CHARACTER( 16 )    :: RXLABEL( NRXNS )

      DATA ( RXLABEL( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &    '1               ', '16              ', '17              ', & ! 0   
     &    '18              ', '19              ', '23              ', & ! 1   
     &    '28              ', '34              ', '41              ', & ! 2   
     &    'BR20            ', 'BR30            ', 'BR41            ', & ! 3   
     &    'BR53            ', 'BP01            ', 'BP02            ', & ! 4   
     &    'BP09            ', 'BP12            ', 'BP15            ', & ! 5   
     &    'BP17            ', 'BP23            ', 'BP25            ', & ! 6   
     &    'BP27            ', 'BP29            ', 'BP30            ', & ! 7   
     &    'BP31            ', 'BP34            ', 'BP37            ', & ! 8   
     &    'BP41            ', 'BP42            ', 'BP44            ', & ! 9   
     &    'BP48            ', 'BP51            ', 'BP58            ', & ! 0   
     &    'BP63            ', 'BP67            ', 'BP69            ', & ! 1   
     &    'BP71            ', 'BP73            ', 'BP79            ', & ! 2   
     &    'BP81            ', 'IS92            ', 'CI01            ', & ! 3   
     &    'CI03            ', 'CI06            ', 'CI07            ', & ! 4   
     &    'CI14            ', 'CI15            ', 'CI19            ', & ! 5   
     &    'CP19            ', 'CP22            ', 'TR01            ', & ! 6   
     &    'TR02            ', 'TR08            ', 'TR15            ', & ! 7   
     &    'HAL_Ozone       ', '2               ', '12              ', & ! 8   
     &    '13              ', '14              ', '20              ', & ! 9   
     &    '21              ', '33              ', '45              ', & ! 0   
     &    'BR19            ', 'BR29            ', 'BR40            ', & ! 1   
     &    'BR52            ', 'BR65            ', 'BR68            ', & ! 2   
     &    'RO01            ', 'RO02            ', 'RO03            ', & ! 3   
     &    'RO04            ', 'RO05            ', 'RO06            ', & ! 4   
     &    'RO07            ', 'RO08            ', 'RO09            ', & ! 5   
     &    'RO10            ', 'RO11            ', 'RO12            ', & ! 6   
     &    'RO13            ', 'RO14            ', 'RO15            ', & ! 7   
     &    'RO16            ', 'RO17            ', 'RO18            ', & ! 8   
     &    'PO01            ', 'PO02            ', 'PO03            ', & ! 9   
     &    'PO04            ', 'PO05            ', 'PO06            ', & ! 0   
     &    'PO07            ', 'PO08            ', 'PO09            ', & ! 1   
     &    'PO10            ', 'PO11            ', 'PO12            ', & ! 2   
     &    'PO13            ', 'PO14            ', 'PO15            ', & ! 3   
     &    'PO16            ', 'PO17            ', 'PO18            ', & ! 4   
     &    'PO19            ', 'PO20            ', 'PO21            ', & ! 5   
     &    'PO22            ', 'PO23            ', 'PO24            ', & ! 6   
     &    'PO25            ', 'PO26            ', 'PO27            ', & ! 7   
     &    'PO28            ', 'PO29            ', 'PO30            ', & ! 8   
     &    'PO31            ', 'PO32            ', 'PO33            ', & ! 9   
     &    'PO34            ', 'PO35            ', 'PO36            ', & ! 0   
     &    'PO37            ', 'PO38            ', 'PO39            ', & ! 1   
     &    'PO40            ', 'PO41            ', 'PO42            ', & ! 2   
     &    'PO43            ', 'PO41a           ', 'PO42b           ', & ! 3   
     &    'PO43c           ', 'PO44            ', 'PO45            ', & ! 4   
     &    'PO46            ', 'PO47            ', 'PO48            ', & ! 5   
     &    'PO49            ', 'PO50            ', 'BE01            ', & ! 6   
     &    'CI16            ', 'CI22            ', 'CP23            ', & ! 7   
     &    'CP24            ', 'CP25            ', 'CP26            ', & ! 8   
     &    'CP27            ', 'CP28            ', 'CE01            ', & ! 9   
     &    'HET_N02         ', 'HET_N2O5IJ      ', 'HET_N2O5K       ', & ! 0   
     &    'HET_H2NO3PIJA   ', 'HET_H2NO3PKA    ', 'OLIG_XYLENE1    ', & ! 1   
     &    'OLIG_XYLENE2    ', 'OLIG_TOLUENE1   ', 'OLIG_TOLUENE2   ', & ! 2   
     &    'OLIG_BENZENE1   ', 'OLIG_BENZENE2   ', 'OLIG_TERPENE1   ', & ! 3   
     &    'OLIG_TERPENE2   ', 'OLIG_ISOPRENE1  ', 'OLIG_ISOPRENE2  ', & ! 4   
     &    'OLIG_SESQT1     ', 'OLIG_PAH1       ', 'OLIG_PAH2       ', & ! 5   
     &    'OLIG_ALK1       ', 'OLIG_ALK2       ', 'HET_IEPOX       ', & ! 6   
     &    '3               ', '4               ', '5               ', & ! 7   
     &    '6               ', '7               ', '8               ', & ! 8   
     &    '9               ', '10              ', '11              ', & ! 9   
     &    '15              ', '22              ', '24              ', & ! 0   
     &    '25              ', '26              ', '27              ', & ! 1   
     &    '29              ', '30              ', '31              ', & ! 2   
     &    '32              ', '35              ', '36              ', & ! 3   
     &    '37              ', '38              ', '39              ', & ! 4   
     &    '40              ', '42              ', '43              ', & ! 5   
     &    '44              ', 'BR01            ', 'BR02            ', & ! 6   
     &    'BR03            ', 'BR04            ', 'BR05            ', & ! 7   
     &    'BR06            ', 'BR07            ', 'BR08            ', & ! 8   
     &    'BR09            ', 'BR10            ', 'BR11            ', & ! 9   
     &    'BR12            ', 'BR13            ', 'BR14            ', & ! 0   
     &    'BR15            ', 'BR16            ', 'BR17            ', & ! 1   
     &    'BR18            ', 'BR21            ', 'BR22            ', & ! 2   
     &    'BR23            ', 'BR24            ', 'BR25            ', & ! 3   
     &    'BR26            ', 'BR27            ', 'BR28            ', & ! 4   
     &    'BR31            ', 'BR32            ', 'BR33            ', & ! 5   
     &    'BR34            ', 'BR35            ', 'BR36            ', & ! 6   
     &    'BR37            ', 'BR38            ', 'BR39            ', & ! 7   
     &    'BR42            ', 'BR43            ', 'BR44            ', & ! 8   
     &    'BR45            ', 'BR46            ', 'BR47            ', & ! 9   
     &    'BR48            ', 'BR49            ', 'BR50            ', & ! 0   
     &    'BR51            ', 'BR54            ', 'BR55            ', & ! 1   
     &    'BR56            ', 'BR57            ', 'BR58            ', & ! 2   
     &    'BR59            ', 'BR60            ', 'BR61            ', & ! 3   
     &    'BR62            ', 'BR63            ', 'BR64            ', & ! 4   
     &    'BR66            ', 'BR67            ', 'BP03            ', & ! 5   
     &    'BP07            ', 'BP08            ', 'BP10            ', & ! 6   
     &    'BP11            ', 'BP13            ', 'BP14            ', & ! 7   
     &    'BP16            ', 'BP18            ', 'BP19            ', & ! 8   
     &    'BP20            ', 'BP21            ', 'BP22            ', & ! 9   
     &    'BP24            ', 'BP26            ', 'BP28            ', & ! 0   
     &    'BP32            ', 'BP33            ', 'BP35            ', & ! 1   
     &    'BP36            ', 'BP38            ', 'BP39            ', & ! 2   
     &    'BP40            ', 'BP43            ', 'BP45            ', & ! 3   
     &    'BP46            ', 'BP47            ', 'BP49            ', & ! 4   
     &    'BP50            ', 'BP52            ', 'BP53            ', & ! 5   
     &    'BP54            ', 'BP55            ', 'BP56            ', & ! 6   
     &    'BP57            ', 'BP59            ', 'BP60            ', & ! 7   
     &    'BP62            ', 'BP64            ', 'BP65            ', & ! 8   
     &    'BP66            ', 'BP68            ', 'BP70            ', & ! 9   
     &    'BP72            ', 'BP74            ', 'BP75            ', & ! 0   
     &    'BP76            ', 'BP77            ', 'BP78            ', & ! 1   
     &    'BP80            ', 'BP82            ', 'BP84            ', & ! 2   
     &    'BP85            ', 'BP86            ', 'BE02            ', & ! 3   
     &    'BE03            ', 'BE04            ', 'BE05            ', & ! 4   
     &    'BT01            ', 'BT02            ', 'BT03            ', & ! 5   
     &    'BT04            ', 'BT05            ', 'BT06            ', & ! 6   
     &    'BT07            ', 'BT08            ', 'BE06            ', & ! 7   
     &    'BE07            ', 'BE08            ', 'BE09            ', & ! 8   
     &    'IS88            ', 'IS89            ', 'IS90            ', & ! 9   
     &    'IS91            ', 'IS96            ', 'IS112           ', & ! 0   
     &    'IS113           ', 'IS114           ', 'BT09            ', & ! 1   
     &    'BT10            ', 'BT11            ', 'BT12            ', & ! 2   
     &    'BE13            ', 'BE14            ', 'BE15            ', & ! 3   
     &    'BT13            ', 'BT14            ', 'BT15            ', & ! 4   
     &    'BT16            ', 'BT17            ', 'BT18            ', & ! 5   
     &    'BL01            ', 'BL02            ', 'BL03            ', & ! 6   
     &    'BL04            ', 'BL05            ', 'AALK            ', & ! 7   
     &    'BL06            ', 'BL07            ', 'BL08            ', & ! 8   
     &    'BL09            ', 'BL10            ', 'BL11            ', & ! 9   
     &    'BL12            ', 'BL13            ', 'BL14            ', & ! 0   
     &    'BL15            ', 'BL15b           ', 'BL16            ', & ! 1   
     &    'BL17            ', 'BL18            ', 'BL19            ', & ! 2   
     &    'BT19            ', 'BT20            ', 'BT21            ', & ! 3   
     &    'BT22            ', 'CI02            ', 'CI04            ', & ! 4   
     &    'CI05            ', 'CI08            ', 'CI09            ', & ! 5   
     &    'CI10            ', 'CI11            ', 'CI12            ', & ! 6   
     &    'CI13            ', 'CI17            ', 'CI18            ', & ! 7   
     &    'CI20            ', 'CI21            ', 'CP01            ', & ! 8   
     &    'CP02            ', 'CP03            ', 'CP04            ', & ! 9   
     &    'CP05            ', 'CP06            ', 'CP07            ', & ! 0   
     &    'CP08            ', 'CP09            ', 'CP10            ', & ! 1   
     &    'CP11            ', 'CP12            ', 'CP13            ', & ! 2   
     &    'CP14            ', 'CP15            ', 'TP01            ', & ! 3   
     &    'CP16            ', 'CP17            ', 'CP18            ', & ! 4   
     &    'CP20            ', 'CP21            ', 'CE02            ', & ! 5   
     &    'TE01            ', 'TE02            ', 'CE03            ', & ! 6   
     &    'TE03            ', 'CE04            ', 'TE04            ', & ! 7   
     &    'TE05            ', 'TE06            ', 'TE07            ', & ! 8   
     &    'TE08            ', 'TE09            ', 'BC01            ', & ! 9   
     &    'BC02            ', 'BC03            ', 'BC04            ', & ! 0   
     &    'BC05            ', 'BC06            ', 'BC07            ', & ! 1   
     &    'BC08            ', 'BC09            ', 'BC09b           ', & ! 2   
     &    'BC10            ', 'BC11            ', 'AE51            ', & ! 3   
     &    'AE52            ', 'AE53            ', 'AE54            ', & ! 4   
     &    'AE55            ', 'AE56            ', 'AE55b           ', & ! 5   
     &    'AE56b           ', 'TR03            ', 'TR05            ', & ! 6   
     &    'TR06            ', 'TR07            ', 'TR09            ', & ! 7   
     &    'TR10            ', 'TR11            ', 'TR12            ', & ! 8   
     &    'TR13            ', 'TR14            ', 'TR16            ', & ! 9   
     &    'HET_H2NO3PIB    ', 'HET_H2NO3PJB    ', 'HET_H2NO3PKB    ', & ! 0   
     &    'PCSOA           ', 'POA_AGE1        ', 'POA_AGE2        ', & ! 1   
     &    'POA_AGE3        ', 'POA_AGE4        ', 'POA_AGE5        ', & ! 2   
     &    'POA_AGE6        ', 'POA_AGE7        ', 'POA_AGE8        ', & ! 3   
     &    'POA_AGE9        ', 'POA_AGE10       '/                   ! 4  

!    NSPECIAL     = Number of special rate coefficients
!    SPECIAL      = Names of special rate coefficients
!    NSPECIAL_RXN = Number of reactions with special rates
!    ISPECIAL     = Pointers to reactions using special rates and their special rate coefficients
!    MAXSPECTERMS = Max Number of terms type used by special rate coefficients
!    KC_COEFFS    = Coefficients of standard rate coefficients  times concentration terms 
!    INDEX_KTERMS  = Pointers to standard rate coefficients in  special rate coefficients
!    INDEX_CTERMS  = Pointers to species concentrations in  special rate coefficients
!    OPERATOR_COEFFS = Coefficients of preceeding special  rate coefficients used in special coefficient 
!    OPERATORS       = Pointers to preceeding special  rate coefficients used in special coefficient 

      INTEGER, PARAMETER :: NSPECIAL_RXN =  77
      INTEGER            :: ISPECIAL( NSPECIAL_RXN,2 )

      DATA ( ISPECIAL( IRXXN,1 ), IRXXN = 1, NSPECIAL_RXN ) / & 
     &     70,   71,   72,   73,   74,   75,   76,   77,   78,   79, & ! O   
     &     80,   81,   82,   83,   84,   85,   86,   87,   88,   89, & ! 1   
     &     90,   91,   92,   93,   94,   95,   96,   97,   98,   99, & ! 2   
     &    100,  101,  102,  103,  104,  105,  106,  107,  108,  109, & ! 3   
     &    110,  111,  112,  113,  114,  115,  116,  117,  118,  119, & ! 4   
     &    120,  121,  122,  123,  124,  125,  126,  127,  128,  129, & ! 5   
     &    130,  131,  132,  133,  134,  135,  136,  137,  138,  139, & ! 6   
     &    140,  144,  145,  146,  147,  148,  149/     !  7   

      DATA ( ISPECIAL( IRXXN,2 ), IRXXN = 1, NSPECIAL_RXN ) / & 
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! O   
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! 1   
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! 2   
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! 3   
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! 4   
     &      6,    7,    1,    9,    7,    2,    8,    6,    2,    8, & ! 5   
     &      6,    2,    8,    6,    2,    8,    6,    6,    7,    6, & ! 6   
     &      7,    6,    7,    6,    7,    6,    7/     !  7   

      INTEGER, PARAMETER :: NSPECIAL =   9
      CHARACTER( 16 )    :: SPECIAL( NSPECIAL )

      DATA ( SPECIAL( IRXXN ), IRXXN = 1, NSPECIAL ) / & 
     &   'RO2NO           ', 'RO2HO2          ', 'RO2NO3          ', & 
     &   'RO2RO2          ', 'RO2RO3          ', 'RO2RO           ', & 
     &   'RO2XRO          ', 'RO2RO2M         ', 'RO22NN          '/

      INTEGER, PARAMETER :: MAXSPECTERMS =   4
      REAL( 8 )          :: KC_COEFFS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: INDEX_KTERMS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: INDEX_CTERMS( NSPECIAL + 1, MAXSPECTERMS)
      REAL( 8 )          :: OPERATOR_COEFFS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: OPERATORS( NSPECIAL + 1, MAXSPECTERMS)

      DATA ( KC_COEFFS(   1,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  1,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &    206,    0,    0,    0/

      DATA ( INDEX_CTERMS(  1,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &    170,    0,    0,    0/

      DATA ( KC_COEFFS(   2,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  2,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &    207,    0,    0,    0/

      DATA ( INDEX_CTERMS(  2,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &    193,    0,    0,    0/

      DATA ( KC_COEFFS(   3,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  3,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &    208,    0,    0,    0/

      DATA ( INDEX_CTERMS(  3,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &    188,    0,    0,    0/

      DATA ( KC_COEFFS(   4,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  4,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &    209,  210,  210,    0/

      DATA ( INDEX_CTERMS(  4,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &    176,  192,  189,    0/

      DATA ( KC_COEFFS(   5,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00/

      DATA ( INDEX_KTERMS(  5,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &    222,  222,  222,  222/

      DATA ( INDEX_CTERMS(  5,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &    177,  166,  149,  167/

      DATA ( KC_COEFFS(   6,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  6,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  6,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( KC_COEFFS(   7,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  7,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  7,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( KC_COEFFS(   8,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  8,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  8,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( KC_COEFFS(   9,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  9,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  9,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   1,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  1,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   2,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  2,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   3,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  3,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   4,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  4,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   5,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  5,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   6,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01/

      DATA ( OPERATORS(  6,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      1,    3,    5,    4/

      DATA ( OPERATOR_COEFFS(   7,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  7,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      2,    4,    0,    0/

      DATA ( OPERATOR_COEFFS(   8,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     5.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  8,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      4,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   9,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 1.0000D+00, 5.0000D-01, 0.0000D+00/

      DATA ( OPERATORS(  9,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      3,    5,    4,    0/


!    Steady-state species section
!    N_SS_SPC     = Number of species assumed to be in steady-state
!    SS_SPC_DIM   = Dimension paramete for steady-state species
!    SS_SPC       = Names of species assumed to be in steady-state
!    MAX_SS_LOSS  = Max no. of SS loss rxns for any SS species
!    MAX_SS_PROD  = Max no. of SS prod rxns for any SS species
!    N_LOSS_RXNS  = No. of SS loss rxns for each SS species
!    N_PROD_RXNS  = No. of SS prod rxns for each SS species
!    SS_LOSS_RXNS = List of SS loss rxns for each SS species
!    SS_PROD_RXNS = List of SS prod rxns for each SS species
!    SS_PROD_COEF = List of SS prod yields for each SS species
!    SS_RCT_IND   = SS species index if it is a rxn reactant

      INTEGER, PARAMETER :: N_SS_SPC =   0

      INTEGER, PARAMETER :: SS_SPC_DIM =   1

      INTEGER, PARAMETER :: MAX_SS_LOSS =   0

      INTEGER, PARAMETER :: MAX_SS_PROD =   0

      CHARACTER( 16 )    :: SS_SPC( 1 )

      INTEGER            :: N_LOSS_RXNS( 1 )
      INTEGER            :: N_PROD_RXNS( 1 )
      INTEGER            :: SS_LOSS_RXNS( 1, 1 )
      INTEGER            :: SS_PROD_RXNS( 1, 1 )
      INTEGER            :: SS_RCT_IND( 1 )

      REAL               :: SS_PROD_COEF( 1,1 ) 
       LOGICAL,  PARAMETER :: USE_SPECIAL_RATES = .TRUE. 
! pointers and names to specific photolysis rates
       INTEGER, PARAMETER  :: IJ_NO2_06           =   1
       INTEGER, PARAMETER  :: IJ_NO3NO_06         =   2
       INTEGER, PARAMETER  :: IJ_NO3NO2_6         =   3
       INTEGER, PARAMETER  :: IJ_O3O1D_06         =   4
       INTEGER, PARAMETER  :: IJ_O3O3P_06         =   5
       INTEGER, PARAMETER  :: IJ_HONO_06          =   6
       INTEGER, PARAMETER  :: IJ_HNO3             =   7
       INTEGER, PARAMETER  :: IJ_HNO4_06          =   8
       INTEGER, PARAMETER  :: IJ_H2O2             =   9
       INTEGER, PARAMETER  :: IJ_PAN              =  10
       INTEGER, PARAMETER  :: IJ_HCHOR_06         =  11
       INTEGER, PARAMETER  :: IJ_HCHOM_06         =  12
       INTEGER, PARAMETER  :: IJ_CCHO_R           =  13
       INTEGER, PARAMETER  :: IJ_C2CHO            =  14
       INTEGER, PARAMETER  :: IJ_ACET_06          =  15
       INTEGER, PARAMETER  :: IJ_MEK_06           =  16
       INTEGER, PARAMETER  :: IJ_COOH             =  17
       INTEGER, PARAMETER  :: IJ_GLY_07R          =  18
       INTEGER, PARAMETER  :: IJ_GLY_07M          =  19
       INTEGER, PARAMETER  :: IJ_MGLY_06          =  20
       INTEGER, PARAMETER  :: IJ_BACL_07          =  21
       INTEGER, PARAMETER  :: IJ_BALD_06          =  22
       INTEGER, PARAMETER  :: IJ_AFG1             =  23
       INTEGER, PARAMETER  :: IJ_MACR_06          =  24
       INTEGER, PARAMETER  :: IJ_MVK_06           =  25
       INTEGER, PARAMETER  :: IJ_IC3ONO2          =  26
       INTEGER, PARAMETER  :: IJ_HOCCHO_IUPAC     =  27
       INTEGER, PARAMETER  :: IJ_ACRO_09          =  28
       INTEGER, PARAMETER  :: IJ_PAA              =  29
       INTEGER, PARAMETER  :: IJ_CL2              =  30
       INTEGER, PARAMETER  :: IJ_CLNO_06          =  31
       INTEGER, PARAMETER  :: IJ_CLONO            =  32
       INTEGER, PARAMETER  :: IJ_CLNO2            =  33
       INTEGER, PARAMETER  :: IJ_CLONO2_1         =  34
       INTEGER, PARAMETER  :: IJ_CLONO2_2         =  35
       INTEGER, PARAMETER  :: IJ_HOCL_06          =  36
       INTEGER, PARAMETER  :: IJ_CLCCHO           =  37
       INTEGER, PARAMETER  :: IJ_CLACET           =  38
       INTEGER, PARAMETER  :: IK_HETERO_NO2       =   1
       INTEGER, PARAMETER  :: IK_HETERO_N2O5IJ    =   2
       INTEGER, PARAMETER  :: IK_HETERO_N2O5K     =   3
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAIJ =   4
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAK  =   5
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBIJ =   6
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBK  =   7
       INTEGER, PARAMETER  :: IK_HETERO_IEPOX     =   8
       END MODULE RXNS_DATA
