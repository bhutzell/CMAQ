       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: /home/hwo/CCTM_git_repository/CCTM/src/MECHS/cb6r3_ae6_aq/mech_cb6r3_ae6_aq.def
! for Mechanism Name: CB6R3_AE6_AQ                    

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

      CHARACTER( 32 ), PARAMETER :: MECHNAME = 'CB6R3_AE6_AQ'

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 127
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 149

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
      DATA GAS_CHEM_SPC(   5 ) / 'KET             ' /
      DATA GAS_CHEM_SPC(   6 ) / 'PRPA            ' /
      DATA GAS_CHEM_SPC(   7 ) / 'ETHY            ' /
      DATA GAS_CHEM_SPC(   8 ) / 'BENZENE         ' /
      DATA GAS_CHEM_SPC(   9 ) / 'ECH4            ' /
      DATA GAS_CHEM_SPC(  10 ) / 'CL2             ' /
      DATA GAS_CHEM_SPC(  11 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC(  12 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC(  13 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC(  14 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC(  15 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC(  16 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC(  17 ) / 'PAHNRXN         ' /
      DATA GAS_CHEM_SPC(  18 ) / 'PAHHRXN         ' /
      DATA GAS_CHEM_SPC(  19 ) / 'SOAALK          ' /
      DATA GAS_CHEM_SPC(  20 ) / 'ALKRXN          ' /
      DATA GAS_CHEM_SPC(  21 ) / 'PCVOC           ' /
      DATA GAS_CHEM_SPC(  22 ) / 'PCSOARXN        ' /
      DATA GAS_CHEM_SPC(  23 ) / 'N2O5            ' /
      DATA GAS_CHEM_SPC(  24 ) / 'PAN             ' /
      DATA GAS_CHEM_SPC(  25 ) / 'ETHA            ' /
      DATA GAS_CHEM_SPC(  26 ) / 'ETOH            ' /
      DATA GAS_CHEM_SPC(  27 ) / 'XPRP            ' /
      DATA GAS_CHEM_SPC(  28 ) / 'XPAR            ' /
      DATA GAS_CHEM_SPC(  29 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC(  30 ) / 'HPLD            ' /
      DATA GAS_CHEM_SPC(  31 ) / 'EPOX            ' /
      DATA GAS_CHEM_SPC(  32 ) / 'TOL             ' /
      DATA GAS_CHEM_SPC(  33 ) / 'XYLMN           ' /
      DATA GAS_CHEM_SPC(  34 ) / 'NAPH            ' /
      DATA GAS_CHEM_SPC(  35 ) / 'HOCL            ' /
      DATA GAS_CHEM_SPC(  36 ) / 'H2NO3PK         ' /
      DATA GAS_CHEM_SPC(  37 ) / 'VIVPO1          ' /
      DATA GAS_CHEM_SPC(  38 ) / 'PNA             ' /
      DATA GAS_CHEM_SPC(  39 ) / 'PANX            ' /
      DATA GAS_CHEM_SPC(  40 ) / 'HCO3            ' /
      DATA GAS_CHEM_SPC(  41 ) / 'INTR            ' /
      DATA GAS_CHEM_SPC(  42 ) / 'ISPX            ' /
      DATA GAS_CHEM_SPC(  43 ) / 'CAT1            ' /
      DATA GAS_CHEM_SPC(  44 ) / 'OPAN            ' /
      DATA GAS_CHEM_SPC(  45 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC(  46 ) / 'SESQRXN         ' /
      DATA GAS_CHEM_SPC(  47 ) / 'H2NO3PIJ        ' /
      DATA GAS_CHEM_SPC(  48 ) / 'HONO            ' /
      DATA GAS_CHEM_SPC(  49 ) / 'MEPX            ' /
      DATA GAS_CHEM_SPC(  50 ) / 'ROR             ' /
      DATA GAS_CHEM_SPC(  51 ) / 'ETHY            ' /
      DATA GAS_CHEM_SPC(  52 ) / 'ETH             ' /
      DATA GAS_CHEM_SPC(  53 ) / 'OLE             ' /
      DATA GAS_CHEM_SPC(  54 ) / 'IOLE            ' /
      DATA GAS_CHEM_SPC(  55 ) / 'ISOP            ' /
      DATA GAS_CHEM_SPC(  56 ) / 'ISO2            ' /
      DATA GAS_CHEM_SPC(  57 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC(  58 ) / 'ISPD            ' /
      DATA GAS_CHEM_SPC(  59 ) / 'INTR            ' /
      DATA GAS_CHEM_SPC(  60 ) / 'ISPX            ' /
      DATA GAS_CHEM_SPC(  61 ) / 'HPLD            ' /
      DATA GAS_CHEM_SPC(  62 ) / 'OPO3            ' /
      DATA GAS_CHEM_SPC(  63 ) / 'EPOX            ' /
      DATA GAS_CHEM_SPC(  64 ) / 'EPX2            ' /
      DATA GAS_CHEM_SPC(  65 ) / 'TERP            ' /
      DATA GAS_CHEM_SPC(  66 ) / 'TRPRXN          ' /
      DATA GAS_CHEM_SPC(  67 ) / 'BENZENE         ' /
      DATA GAS_CHEM_SPC(  68 ) / 'CRES            ' /
      DATA GAS_CHEM_SPC(  69 ) / 'BZO2            ' /
      DATA GAS_CHEM_SPC(  70 ) / 'OPEN            ' /
      DATA GAS_CHEM_SPC(  71 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC(  72 ) / 'TOL             ' /
      DATA GAS_CHEM_SPC(  73 ) / 'TO2             ' /
      DATA GAS_CHEM_SPC(  74 ) / 'TOLRO2          ' /
      DATA GAS_CHEM_SPC(  75 ) / 'XOPN            ' /
      DATA GAS_CHEM_SPC(  76 ) / 'XYLMN           ' /
      DATA GAS_CHEM_SPC(  77 ) / 'XLO2            ' /
      DATA GAS_CHEM_SPC(  78 ) / 'XYLRO2          ' /
      DATA GAS_CHEM_SPC(  79 ) / 'NAPH            ' /
      DATA GAS_CHEM_SPC(  80 ) / 'PAHRO2          ' /
      DATA GAS_CHEM_SPC(  81 ) / 'CRO             ' /
      DATA GAS_CHEM_SPC(  82 ) / 'CAT1            ' /
      DATA GAS_CHEM_SPC(  83 ) / 'CRON            ' /
      DATA GAS_CHEM_SPC(  84 ) / 'OPAN            ' /
      DATA GAS_CHEM_SPC(  85 ) / 'ECH4            ' /
      DATA GAS_CHEM_SPC(  86 ) / 'CL2             ' /
      DATA GAS_CHEM_SPC(  87 ) / 'CL              ' /
      DATA GAS_CHEM_SPC(  88 ) / 'HOCL            ' /
      DATA GAS_CHEM_SPC(  89 ) / 'CLO             ' /
      DATA GAS_CHEM_SPC(  90 ) / 'FMCL            ' /
      DATA GAS_CHEM_SPC(  91 ) / 'HCL             ' /
      DATA GAS_CHEM_SPC(  92 ) / 'CLNO2           ' /
      DATA GAS_CHEM_SPC(  93 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC(  94 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC(  95 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC(  96 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC(  97 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC(  98 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC(  99 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC( 100 ) / 'SESQRXN         ' /
      DATA GAS_CHEM_SPC( 101 ) / 'PAHNRXN         ' /
      DATA GAS_CHEM_SPC( 102 ) / 'PAHHRXN         ' /
      DATA GAS_CHEM_SPC( 103 ) / 'SOAALK          ' /
      DATA GAS_CHEM_SPC( 104 ) / 'ALKRXN          ' /
      DATA GAS_CHEM_SPC( 105 ) / 'H2NO3PIJ        ' /
      DATA GAS_CHEM_SPC( 106 ) / 'H2NO3PK         ' /
      DATA GAS_CHEM_SPC( 107 ) / 'PCVOC           ' /
      DATA GAS_CHEM_SPC( 108 ) / 'PCSOARXN        ' /
      DATA GAS_CHEM_SPC( 109 ) / 'VLVPO1          ' /
      DATA GAS_CHEM_SPC( 110 ) / 'VSVPO1          ' /
      DATA GAS_CHEM_SPC( 111 ) / 'VSVPO2          ' /
      DATA GAS_CHEM_SPC( 112 ) / 'VSVPO3          ' /
      DATA GAS_CHEM_SPC( 113 ) / 'VIVPO1          ' /
      DATA GAS_CHEM_SPC( 114 ) / 'VLVOO1          ' /
      DATA GAS_CHEM_SPC( 115 ) / 'VLVOO2          ' /
      DATA GAS_CHEM_SPC( 116 ) / 'VSVOO2          ' /
      DATA GAS_CHEM_SPC( 117 ) / 'VSVOO3          ' /
      DATA GAS_CHEM_SPC( 118 ) / 'VSVOO1          ' /
      DATA GAS_CHEM_SPC( 119 ) / 'FORM_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 120 ) / 'ALD2_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 121 ) / 'BUTADIENE13     ' /
      DATA GAS_CHEM_SPC( 122 ) / 'ACROLEIN        ' /
      DATA GAS_CHEM_SPC( 123 ) / 'ACRO_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 124 ) / 'TOLU            ' /
      DATA GAS_CHEM_SPC( 125 ) / 'HG              ' /
      DATA GAS_CHEM_SPC( 126 ) / 'HGIIAER         ' /
      DATA GAS_CHEM_SPC( 127 ) / 'HGIIGAS         ' /




      LOGICAL   :: HALOGEN_PARAMETER = .TRUE. 

      DATA CHEMISTRY_SPC(   1 ), SPECIES_MOLWT(   1 ) / 'NO2             ',   46.00 /
      DATA CHEMISTRY_SPC(   2 ), SPECIES_MOLWT(   2 ) / 'NO              ',   30.00 /
      DATA CHEMISTRY_SPC(   3 ), SPECIES_MOLWT(   3 ) / 'O               ',   16.00 /
      DATA CHEMISTRY_SPC(   4 ), SPECIES_MOLWT(   4 ) / 'O3              ',   48.00 /
      DATA CHEMISTRY_SPC(   5 ), SPECIES_MOLWT(   5 ) / 'NO3             ',   62.00 /
      DATA CHEMISTRY_SPC(   6 ), SPECIES_MOLWT(   6 ) / 'O1D             ',   16.00 /
      DATA CHEMISTRY_SPC(   7 ), SPECIES_MOLWT(   7 ) / 'OH              ',   17.00 /
      DATA CHEMISTRY_SPC(   8 ), SPECIES_MOLWT(   8 ) / 'HO2             ',   33.00 /
      DATA CHEMISTRY_SPC(   9 ), SPECIES_MOLWT(   9 ) / 'H2O2            ',   34.00 /
      DATA CHEMISTRY_SPC(  10 ), SPECIES_MOLWT(  10 ) / 'N2O5            ',  108.00 /
      DATA CHEMISTRY_SPC(  11 ), SPECIES_MOLWT(  11 ) / 'HNO3            ',   63.00 /
      DATA CHEMISTRY_SPC(  12 ), SPECIES_MOLWT(  12 ) / 'HONO            ',   47.00 /
      DATA CHEMISTRY_SPC(  13 ), SPECIES_MOLWT(  13 ) / 'PNA             ',   79.00 /
      DATA CHEMISTRY_SPC(  14 ), SPECIES_MOLWT(  14 ) / 'SO2             ',   64.00 /
      DATA CHEMISTRY_SPC(  15 ), SPECIES_MOLWT(  15 ) / 'SULF            ',   98.00 /
      DATA CHEMISTRY_SPC(  16 ), SPECIES_MOLWT(  16 ) / 'SULRXN          ',   98.00 /
      DATA CHEMISTRY_SPC(  17 ), SPECIES_MOLWT(  17 ) / 'C2O3            ',   75.00 /
      DATA CHEMISTRY_SPC(  18 ), SPECIES_MOLWT(  18 ) / 'MEO2            ',   47.00 /
      DATA CHEMISTRY_SPC(  19 ), SPECIES_MOLWT(  19 ) / 'RO2             ',   87.10 /
      DATA CHEMISTRY_SPC(  20 ), SPECIES_MOLWT(  20 ) / 'PAN             ',  121.00 /
      DATA CHEMISTRY_SPC(  21 ), SPECIES_MOLWT(  21 ) / 'PACD            ',   76.00 /
      DATA CHEMISTRY_SPC(  22 ), SPECIES_MOLWT(  22 ) / 'AACD            ',   60.00 /
      DATA CHEMISTRY_SPC(  23 ), SPECIES_MOLWT(  23 ) / 'CXO3            ',   89.00 /
      DATA CHEMISTRY_SPC(  24 ), SPECIES_MOLWT(  24 ) / 'ALD2            ',   44.00 /
      DATA CHEMISTRY_SPC(  25 ), SPECIES_MOLWT(  25 ) / 'XO2H            ',   87.10 /
      DATA CHEMISTRY_SPC(  26 ), SPECIES_MOLWT(  26 ) / 'PANX            ',  135.00 /
      DATA CHEMISTRY_SPC(  27 ), SPECIES_MOLWT(  27 ) / 'FORM            ',   30.00 /
      DATA CHEMISTRY_SPC(  28 ), SPECIES_MOLWT(  28 ) / 'MEPX            ',   48.00 /
      DATA CHEMISTRY_SPC(  29 ), SPECIES_MOLWT(  29 ) / 'MEOH            ',   32.00 /
      DATA CHEMISTRY_SPC(  30 ), SPECIES_MOLWT(  30 ) / 'ROOH            ',   90.10 /
      DATA CHEMISTRY_SPC(  31 ), SPECIES_MOLWT(  31 ) / 'XO2             ',   87.10 /
      DATA CHEMISTRY_SPC(  32 ), SPECIES_MOLWT(  32 ) / 'XO2N            ',   87.10 /
      DATA CHEMISTRY_SPC(  33 ), SPECIES_MOLWT(  33 ) / 'NTR1            ',  119.10 /
      DATA CHEMISTRY_SPC(  34 ), SPECIES_MOLWT(  34 ) / 'NTR2            ',  135.10 /
      DATA CHEMISTRY_SPC(  35 ), SPECIES_MOLWT(  35 ) / 'FACD            ',   46.00 /
      DATA CHEMISTRY_SPC(  36 ), SPECIES_MOLWT(  36 ) / 'CO              ',   28.00 /
      DATA CHEMISTRY_SPC(  37 ), SPECIES_MOLWT(  37 ) / 'HCO3            ',   63.00 /
      DATA CHEMISTRY_SPC(  38 ), SPECIES_MOLWT(  38 ) / 'ALDX            ',   58.10 /
      DATA CHEMISTRY_SPC(  39 ), SPECIES_MOLWT(  39 ) / 'GLYD            ',   60.00 /
      DATA CHEMISTRY_SPC(  40 ), SPECIES_MOLWT(  40 ) / 'GLY             ',   58.00 /
      DATA CHEMISTRY_SPC(  41 ), SPECIES_MOLWT(  41 ) / 'MGLY            ',   72.00 /
      DATA CHEMISTRY_SPC(  42 ), SPECIES_MOLWT(  42 ) / 'ETHA            ',   30.10 /
      DATA CHEMISTRY_SPC(  43 ), SPECIES_MOLWT(  43 ) / 'ETOH            ',   46.10 /
      DATA CHEMISTRY_SPC(  44 ), SPECIES_MOLWT(  44 ) / 'KET             ',   72.10 /
      DATA CHEMISTRY_SPC(  45 ), SPECIES_MOLWT(  45 ) / 'PAR             ',   72.10 /
      DATA CHEMISTRY_SPC(  46 ), SPECIES_MOLWT(  46 ) / 'ACET            ',   58.10 /
      DATA CHEMISTRY_SPC(  47 ), SPECIES_MOLWT(  47 ) / 'PRPA            ',   44.10 /
      DATA CHEMISTRY_SPC(  48 ), SPECIES_MOLWT(  48 ) / 'XPRP            ',   89.10 /
      DATA CHEMISTRY_SPC(  49 ), SPECIES_MOLWT(  49 ) / 'XPAR            ',  117.10 /
      DATA CHEMISTRY_SPC(  50 ), SPECIES_MOLWT(  50 ) / 'ROR             ',   71.10 /
      DATA CHEMISTRY_SPC(  51 ), SPECIES_MOLWT(  51 ) / 'ETHY            ',   26.00 /
      DATA CHEMISTRY_SPC(  52 ), SPECIES_MOLWT(  52 ) / 'ETH             ',   28.00 /
      DATA CHEMISTRY_SPC(  53 ), SPECIES_MOLWT(  53 ) / 'OLE             ',   42.10 /
      DATA CHEMISTRY_SPC(  54 ), SPECIES_MOLWT(  54 ) / 'IOLE            ',   56.10 /
      DATA CHEMISTRY_SPC(  55 ), SPECIES_MOLWT(  55 ) / 'ISOP            ',   68.10 /
      DATA CHEMISTRY_SPC(  56 ), SPECIES_MOLWT(  56 ) / 'ISO2            ',  117.10 /
      DATA CHEMISTRY_SPC(  57 ), SPECIES_MOLWT(  57 ) / 'ISOPRXN         ',   68.10 /
      DATA CHEMISTRY_SPC(  58 ), SPECIES_MOLWT(  58 ) / 'ISPD            ',   70.10 /
      DATA CHEMISTRY_SPC(  59 ), SPECIES_MOLWT(  59 ) / 'INTR            ',  147.10 /
      DATA CHEMISTRY_SPC(  60 ), SPECIES_MOLWT(  60 ) / 'ISPX            ',  118.10 /
      DATA CHEMISTRY_SPC(  61 ), SPECIES_MOLWT(  61 ) / 'HPLD            ',  116.10 /
      DATA CHEMISTRY_SPC(  62 ), SPECIES_MOLWT(  62 ) / 'OPO3            ',  115.00 /
      DATA CHEMISTRY_SPC(  63 ), SPECIES_MOLWT(  63 ) / 'EPOX            ',  118.10 /
      DATA CHEMISTRY_SPC(  64 ), SPECIES_MOLWT(  64 ) / 'EPX2            ',  149.10 /
      DATA CHEMISTRY_SPC(  65 ), SPECIES_MOLWT(  65 ) / 'TERP            ',  136.20 /
      DATA CHEMISTRY_SPC(  66 ), SPECIES_MOLWT(  66 ) / 'TRPRXN          ',  136.20 /
      DATA CHEMISTRY_SPC(  67 ), SPECIES_MOLWT(  67 ) / 'BENZENE         ',   78.10 /
      DATA CHEMISTRY_SPC(  68 ), SPECIES_MOLWT(  68 ) / 'CRES            ',  108.10 /
      DATA CHEMISTRY_SPC(  69 ), SPECIES_MOLWT(  69 ) / 'BZO2            ',  159.10 /
      DATA CHEMISTRY_SPC(  70 ), SPECIES_MOLWT(  70 ) / 'OPEN            ',   84.00 /
      DATA CHEMISTRY_SPC(  71 ), SPECIES_MOLWT(  71 ) / 'BENZRO2         ',  127.00 /
      DATA CHEMISTRY_SPC(  72 ), SPECIES_MOLWT(  72 ) / 'TOL             ',   92.10 /
      DATA CHEMISTRY_SPC(  73 ), SPECIES_MOLWT(  73 ) / 'TO2             ',  173.10 /
      DATA CHEMISTRY_SPC(  74 ), SPECIES_MOLWT(  74 ) / 'TOLRO2          ',  141.00 /
      DATA CHEMISTRY_SPC(  75 ), SPECIES_MOLWT(  75 ) / 'XOPN            ',   98.10 /
      DATA CHEMISTRY_SPC(  76 ), SPECIES_MOLWT(  76 ) / 'XYLMN           ',  106.20 /
      DATA CHEMISTRY_SPC(  77 ), SPECIES_MOLWT(  77 ) / 'XLO2            ',  187.10 /
      DATA CHEMISTRY_SPC(  78 ), SPECIES_MOLWT(  78 ) / 'XYLRO2          ',  155.00 /
      DATA CHEMISTRY_SPC(  79 ), SPECIES_MOLWT(  79 ) / 'NAPH            ',  128.20 /
      DATA CHEMISTRY_SPC(  80 ), SPECIES_MOLWT(  80 ) / 'PAHRO2          ',  187.20 /
      DATA CHEMISTRY_SPC(  81 ), SPECIES_MOLWT(  81 ) / 'CRO             ',  107.10 /
      DATA CHEMISTRY_SPC(  82 ), SPECIES_MOLWT(  82 ) / 'CAT1            ',  124.10 /
      DATA CHEMISTRY_SPC(  83 ), SPECIES_MOLWT(  83 ) / 'CRON            ',  153.10 /
      DATA CHEMISTRY_SPC(  84 ), SPECIES_MOLWT(  84 ) / 'OPAN            ',  161.00 /
      DATA CHEMISTRY_SPC(  85 ), SPECIES_MOLWT(  85 ) / 'ECH4            ',   16.00 /
      DATA CHEMISTRY_SPC(  86 ), SPECIES_MOLWT(  86 ) / 'CL2             ',   71.00 /
      DATA CHEMISTRY_SPC(  87 ), SPECIES_MOLWT(  87 ) / 'CL              ',   35.50 /
      DATA CHEMISTRY_SPC(  88 ), SPECIES_MOLWT(  88 ) / 'HOCL            ',   52.50 /
      DATA CHEMISTRY_SPC(  89 ), SPECIES_MOLWT(  89 ) / 'CLO             ',   51.50 /
      DATA CHEMISTRY_SPC(  90 ), SPECIES_MOLWT(  90 ) / 'FMCL            ',   64.50 /
      DATA CHEMISTRY_SPC(  91 ), SPECIES_MOLWT(  91 ) / 'HCL             ',   36.50 /
      DATA CHEMISTRY_SPC(  92 ), SPECIES_MOLWT(  92 ) / 'CLNO2           ',   81.50 /
      DATA CHEMISTRY_SPC(  93 ), SPECIES_MOLWT(  93 ) / 'TOLNRXN         ',  141.00 /
      DATA CHEMISTRY_SPC(  94 ), SPECIES_MOLWT(  94 ) / 'TOLHRXN         ',  141.00 /
      DATA CHEMISTRY_SPC(  95 ), SPECIES_MOLWT(  95 ) / 'XYLNRXN         ',  155.00 /
      DATA CHEMISTRY_SPC(  96 ), SPECIES_MOLWT(  96 ) / 'XYLHRXN         ',  155.00 /
      DATA CHEMISTRY_SPC(  97 ), SPECIES_MOLWT(  97 ) / 'BNZNRXN         ',  127.00 /
      DATA CHEMISTRY_SPC(  98 ), SPECIES_MOLWT(  98 ) / 'BNZHRXN         ',  127.00 /
      DATA CHEMISTRY_SPC(  99 ), SPECIES_MOLWT(  99 ) / 'SESQ            ',  204.00 /
      DATA CHEMISTRY_SPC( 100 ), SPECIES_MOLWT( 100 ) / 'SESQRXN         ',  204.00 /
      DATA CHEMISTRY_SPC( 101 ), SPECIES_MOLWT( 101 ) / 'PAHNRXN         ',  187.20 /
      DATA CHEMISTRY_SPC( 102 ), SPECIES_MOLWT( 102 ) / 'PAHHRXN         ',  187.20 /
      DATA CHEMISTRY_SPC( 103 ), SPECIES_MOLWT( 103 ) / 'SOAALK          ',  112.00 /
      DATA CHEMISTRY_SPC( 104 ), SPECIES_MOLWT( 104 ) / 'ALKRXN          ',  112.00 /
      DATA CHEMISTRY_SPC( 105 ), SPECIES_MOLWT( 105 ) / 'H2NO3PIJ        ',   64.00 /
      DATA CHEMISTRY_SPC( 106 ), SPECIES_MOLWT( 106 ) / 'H2NO3PK         ',   64.00 /
      DATA CHEMISTRY_SPC( 107 ), SPECIES_MOLWT( 107 ) / 'ACLI            ',   35.50 /
      DATA CHEMISTRY_SPC( 108 ), SPECIES_MOLWT( 108 ) / 'ACLJ            ',   35.50 /
      DATA CHEMISTRY_SPC( 109 ), SPECIES_MOLWT( 109 ) / 'ACLK            ',   35.50 /
      DATA CHEMISTRY_SPC( 110 ), SPECIES_MOLWT( 110 ) / 'AISO3J          ',  168.20 /
      DATA CHEMISTRY_SPC( 111 ), SPECIES_MOLWT( 111 ) / 'AGLYJ           ',   66.40 /
      DATA CHEMISTRY_SPC( 112 ), SPECIES_MOLWT( 112 ) / 'AXYL1J          ',  174.00 /
      DATA CHEMISTRY_SPC( 113 ), SPECIES_MOLWT( 113 ) / 'AOLGAJ          ',  206.00 /
      DATA CHEMISTRY_SPC( 114 ), SPECIES_MOLWT( 114 ) / 'AXYL2J          ',  185.00 /
      DATA CHEMISTRY_SPC( 115 ), SPECIES_MOLWT( 115 ) / 'ATOL1J          ',  163.00 /
      DATA CHEMISTRY_SPC( 116 ), SPECIES_MOLWT( 116 ) / 'ATOL2J          ',  175.00 /
      DATA CHEMISTRY_SPC( 117 ), SPECIES_MOLWT( 117 ) / 'ABNZ1J          ',  161.00 /
      DATA CHEMISTRY_SPC( 118 ), SPECIES_MOLWT( 118 ) / 'ABNZ2J          ',  134.00 /
      DATA CHEMISTRY_SPC( 119 ), SPECIES_MOLWT( 119 ) / 'ATRP1J          ',  177.00 /
      DATA CHEMISTRY_SPC( 120 ), SPECIES_MOLWT( 120 ) / 'AOLGBJ          ',  248.00 /
      DATA CHEMISTRY_SPC( 121 ), SPECIES_MOLWT( 121 ) / 'ATRP2J          ',  198.00 /
      DATA CHEMISTRY_SPC( 122 ), SPECIES_MOLWT( 122 ) / 'AISO1J          ',  132.00 /
      DATA CHEMISTRY_SPC( 123 ), SPECIES_MOLWT( 123 ) / 'AISO2J          ',  133.00 /
      DATA CHEMISTRY_SPC( 124 ), SPECIES_MOLWT( 124 ) / 'ASQTJ           ',  273.00 /
      DATA CHEMISTRY_SPC( 125 ), SPECIES_MOLWT( 125 ) / 'APAH1J          ',  195.60 /
      DATA CHEMISTRY_SPC( 126 ), SPECIES_MOLWT( 126 ) / 'APAH2J          ',  178.70 /
      DATA CHEMISTRY_SPC( 127 ), SPECIES_MOLWT( 127 ) / 'AALK1J          ',  225.00 /
      DATA CHEMISTRY_SPC( 128 ), SPECIES_MOLWT( 128 ) / 'AALK2J          ',  205.10 /
      DATA CHEMISTRY_SPC( 129 ), SPECIES_MOLWT( 129 ) / 'PCVOC           ',  170.00 /
      DATA CHEMISTRY_SPC( 130 ), SPECIES_MOLWT( 130 ) / 'PCSOARXN        ',  170.00 /
      DATA CHEMISTRY_SPC( 131 ), SPECIES_MOLWT( 131 ) / 'VLVPO1          ',  218.00 /
      DATA CHEMISTRY_SPC( 132 ), SPECIES_MOLWT( 132 ) / 'VSVPO1          ',  230.00 /
      DATA CHEMISTRY_SPC( 133 ), SPECIES_MOLWT( 133 ) / 'VSVPO2          ',  241.00 /
      DATA CHEMISTRY_SPC( 134 ), SPECIES_MOLWT( 134 ) / 'VSVPO3          ',  253.00 /
      DATA CHEMISTRY_SPC( 135 ), SPECIES_MOLWT( 135 ) / 'VIVPO1          ',  266.00 /
      DATA CHEMISTRY_SPC( 136 ), SPECIES_MOLWT( 136 ) / 'VLVOO1          ',  136.00 /
      DATA CHEMISTRY_SPC( 137 ), SPECIES_MOLWT( 137 ) / 'VLVOO2          ',  136.00 /
      DATA CHEMISTRY_SPC( 138 ), SPECIES_MOLWT( 138 ) / 'VSVOO2          ',  135.00 /
      DATA CHEMISTRY_SPC( 139 ), SPECIES_MOLWT( 139 ) / 'VSVOO3          ',  134.00 /
      DATA CHEMISTRY_SPC( 140 ), SPECIES_MOLWT( 140 ) / 'VSVOO1          ',  135.00 /
      DATA CHEMISTRY_SPC( 141 ), SPECIES_MOLWT( 141 ) / 'FORM_PRIMARY    ',   30.00 /
      DATA CHEMISTRY_SPC( 142 ), SPECIES_MOLWT( 142 ) / 'ALD2_PRIMARY    ',   44.00 /
      DATA CHEMISTRY_SPC( 143 ), SPECIES_MOLWT( 143 ) / 'BUTADIENE13     ',   54.00 /
      DATA CHEMISTRY_SPC( 144 ), SPECIES_MOLWT( 144 ) / 'ACROLEIN        ',   56.10 /
      DATA CHEMISTRY_SPC( 145 ), SPECIES_MOLWT( 145 ) / 'ACRO_PRIMARY    ',   56.10 /
      DATA CHEMISTRY_SPC( 146 ), SPECIES_MOLWT( 146 ) / 'TOLU            ',   92.00 /
      DATA CHEMISTRY_SPC( 147 ), SPECIES_MOLWT( 147 ) / 'HG              ',  200.60 /
      DATA CHEMISTRY_SPC( 148 ), SPECIES_MOLWT( 148 ) / 'HGIIAER         ',  200.60 /
      DATA CHEMISTRY_SPC( 149 ), SPECIES_MOLWT( 149 ) / 'HGIIGAS         ',  200.60 /


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
      TYPE( MEMBER ) ::  SPECIES_LIST( NUMB_MECH_SPC ) = (/ &
      & MEMBER("AISO3J          ",  181, "AE",  168.20, T), &
      & MEMBER("AXYL1J          ",  128, "AE",  174.00, T), &
      & MEMBER("AXYL2J          ",  129, "AE",  185.00, T), &
      & MEMBER("ATOL1J          ",  131, "AE",  163.00, T), &
      & MEMBER("ATOL2J          ",  132, "AE",  175.00, T), &
      & MEMBER("ABNZ1J          ",  134, "AE",  161.00, T), &
      & MEMBER("ABNZ2J          ",  135, "AE",  134.00, T), &
      & MEMBER("ATRP1J          ",  140, "AE",  177.00, T), &
      & MEMBER("ATRP2J          ",  141, "AE",  198.00, T), &
      & MEMBER("AISO1J          ",  142, "AE",  132.00, T), &
      & MEMBER("AISO2J          ",  143, "AE",  133.00, T), &
      & MEMBER("ASQTJ           ",  144, "AE",  273.00, T), &
      & MEMBER("APAH1J          ",  137, "AE",  195.60, T), &
      & MEMBER("APAH2J          ",  138, "AE",  178.70, T), &
      & MEMBER("AALK1J          ",  126, "AE",  225.00, T), &
      & MEMBER("AALK2J          ",  127, "AE",  205.10, T), &
      & MEMBER("O1D             ",    6, "GC",   16.00, F), &
      & MEMBER("SO2             ",   14, "GC",   64.00, F), &
      & MEMBER("SULF            ",   15, "GC",   98.00, F), &
      & MEMBER("SULRXN          ",   16, "GC",   98.00, F), &
      & MEMBER("KET             ",   46, "GC",   72.10, F), &
      & MEMBER("PRPA            ",   49, "GC",   44.10, F), &
      & MEMBER("ETHY            ",   51, "GC",   26.00, F), &
      & MEMBER("BENZENE         ",   67, "GC",   78.10, F), &
      & MEMBER("ECH4            ",   85, "GC",   16.00, F), &
      & MEMBER("CL2             ",   86, "GC",   71.00, F), &
      & MEMBER("TOLNRXN         ",   93, "GC",  141.00, F), &
      & MEMBER("TOLHRXN         ",   94, "GC",  141.00, F), &
      & MEMBER("XYLNRXN         ",   95, "GC",  155.00, F), &
      & MEMBER("XYLHRXN         ",   96, "GC",  155.00, F), &
      & MEMBER("BNZNRXN         ",   97, "GC",  127.00, F), &
      & MEMBER("BNZHRXN         ",   98, "GC",  127.00, F), &
      & MEMBER("PAHNRXN         ",  101, "GC",  187.20, F), &
      & MEMBER("PAHHRXN         ",  102, "GC",  187.20, F), &
      & MEMBER("SOAALK          ",  103, "GC",  112.00, F), &
      & MEMBER("ALKRXN          ",  104, "GC",  112.00, F), &
      & MEMBER("ACLI            ",  173, "AE",   35.50, T), &
      & MEMBER("ACLJ            ",  172, "AE",   35.50, T), &
      & MEMBER("ACLK            ",  175, "AE",   35.50, T), &
      & MEMBER("AGLYJ           ",  184, "AE",   66.40, T), &
      & MEMBER("PCVOC           ",  117, "GC",  170.00, F), &
      & MEMBER("PCSOARXN        ",  118, "GC",  170.00, F), &
      & MEMBER("N2O5            ",   10, "GC",  108.00, F), &
      & MEMBER("PAN             ",   20, "GC",  121.00, F), &
      & MEMBER("ETHA            ",   44, "GC",   30.10, F), &
      & MEMBER("ETOH            ",   45, "GC",   46.10, F), &
      & MEMBER("XPRP            ",   34, "GC",   89.10, F), &
      & MEMBER("XPAR            ",   33, "GC",  117.10, F), &
      & MEMBER("ISOPRXN         ",   57, "GC",   68.10, F), &
      & MEMBER("HPLD            ",   61, "GC",  116.10, F), &
      & MEMBER("EPOX            ",   63, "GC",  118.10, F), &
      & MEMBER("TOL             ",   72, "GC",   92.10, F), &
      & MEMBER("XYLMN           ",   76, "GC",  106.20, F), &
      & MEMBER("NAPH            ",   79, "GC",  128.20, F), &
      & MEMBER("HOCL            ",   88, "GC",   52.50, F), &
      & MEMBER("H2NO3PK         ",  106, "GC",   64.00, F), &
      & MEMBER("VIVPO1          ",  111, "GC",  266.00, F), &
      & MEMBER("PNA             ",   13, "GC",   79.00, F), &
      & MEMBER("PANX            ",   26, "GC",  135.00, F), &
      & MEMBER("HCO3            ",   39, "GC",   63.00, F), &
      & MEMBER("INTR            ",   59, "GC",  147.10, F), &
      & MEMBER("ISPX            ",   60, "GC",  118.10, F), &
      & MEMBER("CAT1            ",   82, "GC",  124.10, F), &
      & MEMBER("OPAN            ",   84, "GC",  161.00, F), &
      & MEMBER("SESQ            ",   99, "GC",  204.00, F), &
      & MEMBER("SESQRXN         ",  100, "GC",  204.00, F), &
      & MEMBER("H2NO3PIJ        ",  105, "GC",   64.00, F), &
      & MEMBER("HONO            ",   12, "GC",   47.00, F), &
      & MEMBER("MEPX            ",   28, "GC",   48.00, F), &
      & MEMBER("ROR             ",   50, "GC",   71.10, F), &
      & MEMBER("TERP            ",   65, "GC",  136.20, F), &
      & MEMBER("TRPRXN          ",   66, "GC",  136.20, F), &
      & MEMBER("BENZRO2         ",   71, "GC",  127.00, F), &
      & MEMBER("TOLRO2          ",   74, "GC",  141.00, F), &
      & MEMBER("XYLRO2          ",   78, "GC",  155.00, F), &
      & MEMBER("PAHRO2          ",   80, "GC",  187.20, F), &
      & MEMBER("CRON            ",   83, "GC",  153.10, F), &
      & MEMBER("AOLGBJ          ",  183, "AE",  248.00, T), &
      & MEMBER("VSVPO3          ",  110, "GC",  253.00, F), &
      & MEMBER("PACD            ",   21, "GC",   76.00, F), &
      & MEMBER("MEOH            ",   29, "GC",   32.00, F), &
      & MEMBER("ROOH            ",   30, "GC",   90.10, F), &
      & MEMBER("ACET            ",   48, "GC",   58.10, F), &
      & MEMBER("ETH             ",   52, "GC",   28.00, F), &
      & MEMBER("ISOP            ",   55, "GC",   68.10, F), &
      & MEMBER("CLO             ",   89, "GC",   51.50, F), &
      & MEMBER("VLVPO1          ",  107, "GC",  218.00, F), &
      & MEMBER("VSVPO1          ",  108, "GC",  230.00, F), &
      & MEMBER("VSVPO2          ",  109, "GC",  241.00, F), &
      & MEMBER("VSVOO1          ",  114, "GC",  135.00, F), &
      & MEMBER("H2O2            ",    9, "GC",   34.00, F), &
      & MEMBER("IOLE            ",   54, "GC",   56.10, F), &
      & MEMBER("EPX2            ",   64, "GC",  149.10, F), &
      & MEMBER("BZO2            ",   69, "GC",  159.10, F), &
      & MEMBER("TO2             ",   73, "GC",  173.10, F), &
      & MEMBER("FMCL            ",   90, "GC",   64.50, F), &
      & MEMBER("VSVOO2          ",  115, "GC",  135.00, F), &
      & MEMBER("VSVOO3          ",  116, "GC",  134.00, F), &
      & MEMBER("ISO2            ",   56, "GC",  117.10, F), &
      & MEMBER("XLO2            ",   77, "GC",  187.10, F), &
      & MEMBER("CRO             ",   81, "GC",  107.10, F), &
      & MEMBER("CLNO2           ",   92, "GC",   81.50, F), &
      & MEMBER("OLE             ",   53, "GC",   42.10, F), &
      & MEMBER("CRES            ",   68, "GC",  108.10, F), &
      & MEMBER("NTR1            ",   35, "GC",  119.10, F), &
      & MEMBER("AOLGAJ          ",  182, "AE",  206.00, T), &
      & MEMBER("XOPN            ",   75, "GC",   98.10, F), &
      & MEMBER("VLVOO1          ",  112, "GC",  136.00, F), &
      & MEMBER("VLVOO2          ",  113, "GC",  136.00, F), &
      & MEMBER("FACD            ",   37, "GC",   46.00, F), &
      & MEMBER("GLYD            ",   41, "GC",   60.00, F), &
      & MEMBER("OPO3            ",   62, "GC",  115.00, F), &
      & MEMBER("ISPD            ",   58, "GC",   70.10, F), &
      & MEMBER("OPEN            ",   70, "GC",   84.00, F), &
      & MEMBER("HCL             ",   91, "GC",   36.50, F), &
      & MEMBER("AACD            ",   22, "GC",   60.00, F), &
      & MEMBER("NTR2            ",   36, "GC",  135.10, F), &
      & MEMBER("O               ",    3, "GC",   16.00, F), &
      & MEMBER("MGLY            ",   43, "GC",   72.00, F), &
      & MEMBER("CXO3            ",   23, "GC",   89.00, F), &
      & MEMBER("O3              ",    4, "GC",   48.00, F), &
      & MEMBER("HNO3            ",   11, "GC",   63.00, F), &
      & MEMBER("ALDX            ",   40, "GC",   58.10, F), &
      & MEMBER("PAR             ",   47, "GC",   72.10, F), &
      & MEMBER("GLY             ",   42, "GC",   58.00, F), &
      & MEMBER("ALD2            ",   24, "GC",   44.00, F), &
      & MEMBER("CL              ",   87, "GC",   35.50, F), &
      & MEMBER("MEO2            ",   18, "GC",   47.00, F), &
      & MEMBER("XO2N            ",   32, "GC",   87.10, F), &
      & MEMBER("NO              ",    2, "GC",   30.00, F), &
      & MEMBER("FORM            ",   27, "GC",   30.00, F), &
      & MEMBER("XO2             ",   31, "GC",   87.10, F), &
      & MEMBER("CO              ",   38, "GC",   28.00, F), &
      & MEMBER("C2O3            ",   17, "GC",   75.00, F), &
      & MEMBER("NO3             ",    5, "GC",   62.00, F), &
      & MEMBER("XO2H            ",   25, "GC",   87.10, F), &
      & MEMBER("NO2             ",    1, "GC",   46.00, F), &
      & MEMBER("RO2             ",   19, "GC",   87.10, F), &
      & MEMBER("HO2             ",    8, "GC",   33.00, F), &
      & MEMBER("OH              ",    7, "GC",   17.00, F) /)

      DATA CHEMISTRY_SPC(   1 ), SPECIES_MOLWT(   1 ) / 'AISO3J          ',  168.20 /
      DATA CHEMISTRY_SPC(   2 ), SPECIES_MOLWT(   2 ) / 'AXYL1J          ',  174.00 /
      DATA CHEMISTRY_SPC(   3 ), SPECIES_MOLWT(   3 ) / 'AXYL2J          ',  185.00 /
      DATA CHEMISTRY_SPC(   4 ), SPECIES_MOLWT(   4 ) / 'ATOL1J          ',  163.00 /
      DATA CHEMISTRY_SPC(   5 ), SPECIES_MOLWT(   5 ) / 'ATOL2J          ',  175.00 /
      DATA CHEMISTRY_SPC(   6 ), SPECIES_MOLWT(   6 ) / 'ABNZ1J          ',  161.00 /
      DATA CHEMISTRY_SPC(   7 ), SPECIES_MOLWT(   7 ) / 'ABNZ2J          ',  134.00 /
      DATA CHEMISTRY_SPC(   8 ), SPECIES_MOLWT(   8 ) / 'ATRP1J          ',  177.00 /
      DATA CHEMISTRY_SPC(   9 ), SPECIES_MOLWT(   9 ) / 'ATRP2J          ',  198.00 /
      DATA CHEMISTRY_SPC(  10 ), SPECIES_MOLWT(  10 ) / 'AISO1J          ',  132.00 /
      DATA CHEMISTRY_SPC(  11 ), SPECIES_MOLWT(  11 ) / 'AISO2J          ',  133.00 /
      DATA CHEMISTRY_SPC(  12 ), SPECIES_MOLWT(  12 ) / 'ASQTJ           ',  273.00 /
      DATA CHEMISTRY_SPC(  13 ), SPECIES_MOLWT(  13 ) / 'APAH1J          ',  195.60 /
      DATA CHEMISTRY_SPC(  14 ), SPECIES_MOLWT(  14 ) / 'APAH2J          ',  178.70 /
      DATA CHEMISTRY_SPC(  15 ), SPECIES_MOLWT(  15 ) / 'AALK1J          ',  225.00 /
      DATA CHEMISTRY_SPC(  16 ), SPECIES_MOLWT(  16 ) / 'AALK2J          ',  205.10 /
      DATA CHEMISTRY_SPC(  17 ), SPECIES_MOLWT(  17 ) / 'O1D             ',   16.00 /
      DATA CHEMISTRY_SPC(  18 ), SPECIES_MOLWT(  18 ) / 'SO2             ',   64.00 /
      DATA CHEMISTRY_SPC(  19 ), SPECIES_MOLWT(  19 ) / 'SULF            ',   98.00 /
      DATA CHEMISTRY_SPC(  20 ), SPECIES_MOLWT(  20 ) / 'SULRXN          ',   98.00 /
      DATA CHEMISTRY_SPC(  21 ), SPECIES_MOLWT(  21 ) / 'KET             ',   72.10 /
      DATA CHEMISTRY_SPC(  22 ), SPECIES_MOLWT(  22 ) / 'PRPA            ',   44.10 /
      DATA CHEMISTRY_SPC(  23 ), SPECIES_MOLWT(  23 ) / 'ETHY            ',   26.00 /
      DATA CHEMISTRY_SPC(  24 ), SPECIES_MOLWT(  24 ) / 'BENZENE         ',   78.10 /
      DATA CHEMISTRY_SPC(  25 ), SPECIES_MOLWT(  25 ) / 'ECH4            ',   16.00 /
      DATA CHEMISTRY_SPC(  26 ), SPECIES_MOLWT(  26 ) / 'CL2             ',   71.00 /
      DATA CHEMISTRY_SPC(  27 ), SPECIES_MOLWT(  27 ) / 'TOLNRXN         ',  141.00 /
      DATA CHEMISTRY_SPC(  28 ), SPECIES_MOLWT(  28 ) / 'TOLHRXN         ',  141.00 /
      DATA CHEMISTRY_SPC(  29 ), SPECIES_MOLWT(  29 ) / 'XYLNRXN         ',  155.00 /
      DATA CHEMISTRY_SPC(  30 ), SPECIES_MOLWT(  30 ) / 'XYLHRXN         ',  155.00 /
      DATA CHEMISTRY_SPC(  31 ), SPECIES_MOLWT(  31 ) / 'BNZNRXN         ',  127.00 /
      DATA CHEMISTRY_SPC(  32 ), SPECIES_MOLWT(  32 ) / 'BNZHRXN         ',  127.00 /
      DATA CHEMISTRY_SPC(  33 ), SPECIES_MOLWT(  33 ) / 'PAHNRXN         ',  187.20 /
      DATA CHEMISTRY_SPC(  34 ), SPECIES_MOLWT(  34 ) / 'PAHHRXN         ',  187.20 /
      DATA CHEMISTRY_SPC(  35 ), SPECIES_MOLWT(  35 ) / 'SOAALK          ',  112.00 /
      DATA CHEMISTRY_SPC(  36 ), SPECIES_MOLWT(  36 ) / 'ALKRXN          ',  112.00 /
      DATA CHEMISTRY_SPC(  37 ), SPECIES_MOLWT(  37 ) / 'ACLI            ',   35.50 /
      DATA CHEMISTRY_SPC(  38 ), SPECIES_MOLWT(  38 ) / 'ACLJ            ',   35.50 /
      DATA CHEMISTRY_SPC(  39 ), SPECIES_MOLWT(  39 ) / 'ACLK            ',   35.50 /
      DATA CHEMISTRY_SPC(  40 ), SPECIES_MOLWT(  40 ) / 'AGLYJ           ',   66.40 /
      DATA CHEMISTRY_SPC(  41 ), SPECIES_MOLWT(  41 ) / 'PCVOC           ',  170.00 /
      DATA CHEMISTRY_SPC(  42 ), SPECIES_MOLWT(  42 ) / 'PCSOARXN        ',  170.00 /
      DATA CHEMISTRY_SPC(  43 ), SPECIES_MOLWT(  43 ) / 'N2O5            ',  108.00 /
      DATA CHEMISTRY_SPC(  44 ), SPECIES_MOLWT(  44 ) / 'PAN             ',  121.00 /
      DATA CHEMISTRY_SPC(  45 ), SPECIES_MOLWT(  45 ) / 'ETHA            ',   30.10 /
      DATA CHEMISTRY_SPC(  46 ), SPECIES_MOLWT(  46 ) / 'ETOH            ',   46.10 /
      DATA CHEMISTRY_SPC(  47 ), SPECIES_MOLWT(  47 ) / 'XPRP            ',   89.10 /
      DATA CHEMISTRY_SPC(  48 ), SPECIES_MOLWT(  48 ) / 'XPAR            ',  117.10 /
      DATA CHEMISTRY_SPC(  49 ), SPECIES_MOLWT(  49 ) / 'ISOPRXN         ',   68.10 /
      DATA CHEMISTRY_SPC(  50 ), SPECIES_MOLWT(  50 ) / 'HPLD            ',  116.10 /
      DATA CHEMISTRY_SPC(  51 ), SPECIES_MOLWT(  51 ) / 'EPOX            ',  118.10 /
      DATA CHEMISTRY_SPC(  52 ), SPECIES_MOLWT(  52 ) / 'TOL             ',   92.10 /
      DATA CHEMISTRY_SPC(  53 ), SPECIES_MOLWT(  53 ) / 'XYLMN           ',  106.20 /
      DATA CHEMISTRY_SPC(  54 ), SPECIES_MOLWT(  54 ) / 'NAPH            ',  128.20 /
      DATA CHEMISTRY_SPC(  55 ), SPECIES_MOLWT(  55 ) / 'HOCL            ',   52.50 /
      DATA CHEMISTRY_SPC(  56 ), SPECIES_MOLWT(  56 ) / 'H2NO3PK         ',   64.00 /
      DATA CHEMISTRY_SPC(  57 ), SPECIES_MOLWT(  57 ) / 'VIVPO1          ',  266.00 /
      DATA CHEMISTRY_SPC(  58 ), SPECIES_MOLWT(  58 ) / 'PNA             ',   79.00 /
      DATA CHEMISTRY_SPC(  59 ), SPECIES_MOLWT(  59 ) / 'PANX            ',  135.00 /
      DATA CHEMISTRY_SPC(  60 ), SPECIES_MOLWT(  60 ) / 'HCO3            ',   63.00 /
      DATA CHEMISTRY_SPC(  61 ), SPECIES_MOLWT(  61 ) / 'INTR            ',  147.10 /
      DATA CHEMISTRY_SPC(  62 ), SPECIES_MOLWT(  62 ) / 'ISPX            ',  118.10 /
      DATA CHEMISTRY_SPC(  63 ), SPECIES_MOLWT(  63 ) / 'CAT1            ',  124.10 /
      DATA CHEMISTRY_SPC(  64 ), SPECIES_MOLWT(  64 ) / 'OPAN            ',  161.00 /
      DATA CHEMISTRY_SPC(  65 ), SPECIES_MOLWT(  65 ) / 'SESQ            ',  204.00 /
      DATA CHEMISTRY_SPC(  66 ), SPECIES_MOLWT(  66 ) / 'SESQRXN         ',  204.00 /
      DATA CHEMISTRY_SPC(  67 ), SPECIES_MOLWT(  67 ) / 'H2NO3PIJ        ',   64.00 /
      DATA CHEMISTRY_SPC(  68 ), SPECIES_MOLWT(  68 ) / 'HONO            ',   47.00 /
      DATA CHEMISTRY_SPC(  69 ), SPECIES_MOLWT(  69 ) / 'MEPX            ',   48.00 /
      DATA CHEMISTRY_SPC(  70 ), SPECIES_MOLWT(  70 ) / 'ROR             ',   71.10 /
      DATA CHEMISTRY_SPC(  71 ), SPECIES_MOLWT(  71 ) / 'TERP            ',  136.20 /
      DATA CHEMISTRY_SPC(  72 ), SPECIES_MOLWT(  72 ) / 'TRPRXN          ',  136.20 /
      DATA CHEMISTRY_SPC(  73 ), SPECIES_MOLWT(  73 ) / 'BENZRO2         ',  127.00 /
      DATA CHEMISTRY_SPC(  74 ), SPECIES_MOLWT(  74 ) / 'TOLRO2          ',  141.00 /
      DATA CHEMISTRY_SPC(  75 ), SPECIES_MOLWT(  75 ) / 'XYLRO2          ',  155.00 /
      DATA CHEMISTRY_SPC(  76 ), SPECIES_MOLWT(  76 ) / 'PAHRO2          ',  187.20 /
      DATA CHEMISTRY_SPC(  77 ), SPECIES_MOLWT(  77 ) / 'CRON            ',  153.10 /
      DATA CHEMISTRY_SPC(  78 ), SPECIES_MOLWT(  78 ) / 'AOLGBJ          ',  248.00 /
      DATA CHEMISTRY_SPC(  79 ), SPECIES_MOLWT(  79 ) / 'VSVPO3          ',  253.00 /
      DATA CHEMISTRY_SPC(  80 ), SPECIES_MOLWT(  80 ) / 'PACD            ',   76.00 /
      DATA CHEMISTRY_SPC(  81 ), SPECIES_MOLWT(  81 ) / 'MEOH            ',   32.00 /
      DATA CHEMISTRY_SPC(  82 ), SPECIES_MOLWT(  82 ) / 'ROOH            ',   90.10 /
      DATA CHEMISTRY_SPC(  83 ), SPECIES_MOLWT(  83 ) / 'ACET            ',   58.10 /
      DATA CHEMISTRY_SPC(  84 ), SPECIES_MOLWT(  84 ) / 'ETH             ',   28.00 /
      DATA CHEMISTRY_SPC(  85 ), SPECIES_MOLWT(  85 ) / 'ISOP            ',   68.10 /
      DATA CHEMISTRY_SPC(  86 ), SPECIES_MOLWT(  86 ) / 'CLO             ',   51.50 /
      DATA CHEMISTRY_SPC(  87 ), SPECIES_MOLWT(  87 ) / 'VLVPO1          ',  218.00 /
      DATA CHEMISTRY_SPC(  88 ), SPECIES_MOLWT(  88 ) / 'VSVPO1          ',  230.00 /
      DATA CHEMISTRY_SPC(  89 ), SPECIES_MOLWT(  89 ) / 'VSVPO2          ',  241.00 /
      DATA CHEMISTRY_SPC(  90 ), SPECIES_MOLWT(  90 ) / 'VSVOO1          ',  135.00 /
      DATA CHEMISTRY_SPC(  91 ), SPECIES_MOLWT(  91 ) / 'H2O2            ',   34.00 /
      DATA CHEMISTRY_SPC(  92 ), SPECIES_MOLWT(  92 ) / 'IOLE            ',   56.10 /
      DATA CHEMISTRY_SPC(  93 ), SPECIES_MOLWT(  93 ) / 'EPX2            ',  149.10 /
      DATA CHEMISTRY_SPC(  94 ), SPECIES_MOLWT(  94 ) / 'BZO2            ',  159.10 /
      DATA CHEMISTRY_SPC(  95 ), SPECIES_MOLWT(  95 ) / 'TO2             ',  173.10 /
      DATA CHEMISTRY_SPC(  96 ), SPECIES_MOLWT(  96 ) / 'FMCL            ',   64.50 /
      DATA CHEMISTRY_SPC(  97 ), SPECIES_MOLWT(  97 ) / 'VSVOO2          ',  135.00 /
      DATA CHEMISTRY_SPC(  98 ), SPECIES_MOLWT(  98 ) / 'VSVOO3          ',  134.00 /
      DATA CHEMISTRY_SPC(  99 ), SPECIES_MOLWT(  99 ) / 'ISO2            ',  117.10 /
      DATA CHEMISTRY_SPC( 100 ), SPECIES_MOLWT( 100 ) / 'XLO2            ',  187.10 /
      DATA CHEMISTRY_SPC( 101 ), SPECIES_MOLWT( 101 ) / 'CRO             ',  107.10 /
      DATA CHEMISTRY_SPC( 102 ), SPECIES_MOLWT( 102 ) / 'CLNO2           ',   81.50 /
      DATA CHEMISTRY_SPC( 103 ), SPECIES_MOLWT( 103 ) / 'OLE             ',   42.10 /
      DATA CHEMISTRY_SPC( 104 ), SPECIES_MOLWT( 104 ) / 'CRES            ',  108.10 /
      DATA CHEMISTRY_SPC( 105 ), SPECIES_MOLWT( 105 ) / 'NTR1            ',  119.10 /
      DATA CHEMISTRY_SPC( 106 ), SPECIES_MOLWT( 106 ) / 'AOLGAJ          ',  206.00 /
      DATA CHEMISTRY_SPC( 107 ), SPECIES_MOLWT( 107 ) / 'XOPN            ',   98.10 /
      DATA CHEMISTRY_SPC( 108 ), SPECIES_MOLWT( 108 ) / 'VLVOO1          ',  136.00 /
      DATA CHEMISTRY_SPC( 109 ), SPECIES_MOLWT( 109 ) / 'VLVOO2          ',  136.00 /
      DATA CHEMISTRY_SPC( 110 ), SPECIES_MOLWT( 110 ) / 'FACD            ',   46.00 /
      DATA CHEMISTRY_SPC( 111 ), SPECIES_MOLWT( 111 ) / 'GLYD            ',   60.00 /
      DATA CHEMISTRY_SPC( 112 ), SPECIES_MOLWT( 112 ) / 'OPO3            ',  115.00 /
      DATA CHEMISTRY_SPC( 113 ), SPECIES_MOLWT( 113 ) / 'ISPD            ',   70.10 /
      DATA CHEMISTRY_SPC( 114 ), SPECIES_MOLWT( 114 ) / 'OPEN            ',   84.00 /
      DATA CHEMISTRY_SPC( 115 ), SPECIES_MOLWT( 115 ) / 'HCL             ',   36.50 /
      DATA CHEMISTRY_SPC( 116 ), SPECIES_MOLWT( 116 ) / 'AACD            ',   60.00 /
      DATA CHEMISTRY_SPC( 117 ), SPECIES_MOLWT( 117 ) / 'NTR2            ',  135.10 /
      DATA CHEMISTRY_SPC( 118 ), SPECIES_MOLWT( 118 ) / 'O               ',   16.00 /
      DATA CHEMISTRY_SPC( 119 ), SPECIES_MOLWT( 119 ) / 'MGLY            ',   72.00 /
      DATA CHEMISTRY_SPC( 120 ), SPECIES_MOLWT( 120 ) / 'CXO3            ',   89.00 /
      DATA CHEMISTRY_SPC( 121 ), SPECIES_MOLWT( 121 ) / 'O3              ',   48.00 /
      DATA CHEMISTRY_SPC( 122 ), SPECIES_MOLWT( 122 ) / 'HNO3            ',   63.00 /
      DATA CHEMISTRY_SPC( 123 ), SPECIES_MOLWT( 123 ) / 'ALDX            ',   58.10 /
      DATA CHEMISTRY_SPC( 124 ), SPECIES_MOLWT( 124 ) / 'PAR             ',   72.10 /
      DATA CHEMISTRY_SPC( 125 ), SPECIES_MOLWT( 125 ) / 'GLY             ',   58.00 /
      DATA CHEMISTRY_SPC( 126 ), SPECIES_MOLWT( 126 ) / 'ALD2            ',   44.00 /
      DATA CHEMISTRY_SPC( 127 ), SPECIES_MOLWT( 127 ) / 'CL              ',   35.50 /
      DATA CHEMISTRY_SPC( 128 ), SPECIES_MOLWT( 128 ) / 'MEO2            ',   47.00 /
      DATA CHEMISTRY_SPC( 129 ), SPECIES_MOLWT( 129 ) / 'XO2N            ',   87.10 /
      DATA CHEMISTRY_SPC( 130 ), SPECIES_MOLWT( 130 ) / 'NO              ',   30.00 /
      DATA CHEMISTRY_SPC( 131 ), SPECIES_MOLWT( 131 ) / 'FORM            ',   30.00 /
      DATA CHEMISTRY_SPC( 132 ), SPECIES_MOLWT( 132 ) / 'XO2             ',   87.10 /
      DATA CHEMISTRY_SPC( 133 ), SPECIES_MOLWT( 133 ) / 'CO              ',   28.00 /
      DATA CHEMISTRY_SPC( 134 ), SPECIES_MOLWT( 134 ) / 'C2O3            ',   75.00 /
      DATA CHEMISTRY_SPC( 135 ), SPECIES_MOLWT( 135 ) / 'NO3             ',   62.00 /
      DATA CHEMISTRY_SPC( 136 ), SPECIES_MOLWT( 136 ) / 'XO2H            ',   87.10 /
      DATA CHEMISTRY_SPC( 137 ), SPECIES_MOLWT( 137 ) / 'NO2             ',   46.00 /
      DATA CHEMISTRY_SPC( 138 ), SPECIES_MOLWT( 138 ) / 'RO2             ',   87.10 /
      DATA CHEMISTRY_SPC( 139 ), SPECIES_MOLWT( 139 ) / 'HO2             ',   33.00 /
      DATA CHEMISTRY_SPC( 140 ), SPECIES_MOLWT( 140 ) / 'OH              ',   17.00 /


      DATA CGRID_INDEX(   1 ), SPECIES_TYPE(   1 ), CONVERT_CONC(   1 ) /  181, 'AE', T /  ! AISO3J
      DATA CGRID_INDEX(   2 ), SPECIES_TYPE(   2 ), CONVERT_CONC(   2 ) /  128, 'AE', T /  ! AXYL1J
      DATA CGRID_INDEX(   3 ), SPECIES_TYPE(   3 ), CONVERT_CONC(   3 ) /  129, 'AE', T /  ! AXYL2J
      DATA CGRID_INDEX(   4 ), SPECIES_TYPE(   4 ), CONVERT_CONC(   4 ) /  131, 'AE', T /  ! ATOL1J
      DATA CGRID_INDEX(   5 ), SPECIES_TYPE(   5 ), CONVERT_CONC(   5 ) /  132, 'AE', T /  ! ATOL2J
      DATA CGRID_INDEX(   6 ), SPECIES_TYPE(   6 ), CONVERT_CONC(   6 ) /  134, 'AE', T /  ! ABNZ1J
      DATA CGRID_INDEX(   7 ), SPECIES_TYPE(   7 ), CONVERT_CONC(   7 ) /  135, 'AE', T /  ! ABNZ2J
      DATA CGRID_INDEX(   8 ), SPECIES_TYPE(   8 ), CONVERT_CONC(   8 ) /  140, 'AE', T /  ! ATRP1J
      DATA CGRID_INDEX(   9 ), SPECIES_TYPE(   9 ), CONVERT_CONC(   9 ) /  141, 'AE', T /  ! ATRP2J
      DATA CGRID_INDEX(  10 ), SPECIES_TYPE(  10 ), CONVERT_CONC(  10 ) /  142, 'AE', T /  ! AISO1J
      DATA CGRID_INDEX(  11 ), SPECIES_TYPE(  11 ), CONVERT_CONC(  11 ) /  143, 'AE', T /  ! AISO2J
      DATA CGRID_INDEX(  12 ), SPECIES_TYPE(  12 ), CONVERT_CONC(  12 ) /  144, 'AE', T /  ! ASQTJ
      DATA CGRID_INDEX(  13 ), SPECIES_TYPE(  13 ), CONVERT_CONC(  13 ) /  137, 'AE', T /  ! APAH1J
      DATA CGRID_INDEX(  14 ), SPECIES_TYPE(  14 ), CONVERT_CONC(  14 ) /  138, 'AE', T /  ! APAH2J
      DATA CGRID_INDEX(  15 ), SPECIES_TYPE(  15 ), CONVERT_CONC(  15 ) /  126, 'AE', T /  ! AALK1J
      DATA CGRID_INDEX(  16 ), SPECIES_TYPE(  16 ), CONVERT_CONC(  16 ) /  127, 'AE', T /  ! AALK2J
      DATA CGRID_INDEX(  17 ), SPECIES_TYPE(  17 ), CONVERT_CONC(  17 ) /    6, 'GC', F /  ! O1D
      DATA CGRID_INDEX(  18 ), SPECIES_TYPE(  18 ), CONVERT_CONC(  18 ) /   14, 'GC', F /  ! SO2
      DATA CGRID_INDEX(  19 ), SPECIES_TYPE(  19 ), CONVERT_CONC(  19 ) /   15, 'GC', F /  ! SULF
      DATA CGRID_INDEX(  20 ), SPECIES_TYPE(  20 ), CONVERT_CONC(  20 ) /   16, 'GC', F /  ! SULRXN
      DATA CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), CONVERT_CONC(  21 ) /   46, 'GC', F /  ! KET
      DATA CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), CONVERT_CONC(  22 ) /   49, 'GC', F /  ! PRPA
      DATA CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), CONVERT_CONC(  23 ) /   51, 'GC', F /  ! ETHY
      DATA CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), CONVERT_CONC(  24 ) /   67, 'GC', F /  ! BENZENE
      DATA CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), CONVERT_CONC(  25 ) /   85, 'GC', F /  ! ECH4
      DATA CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), CONVERT_CONC(  26 ) /   86, 'GC', F /  ! CL2
      DATA CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), CONVERT_CONC(  27 ) /   93, 'GC', F /  ! TOLNRXN
      DATA CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), CONVERT_CONC(  28 ) /   94, 'GC', F /  ! TOLHRXN
      DATA CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), CONVERT_CONC(  29 ) /   95, 'GC', F /  ! XYLNRXN
      DATA CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), CONVERT_CONC(  30 ) /   96, 'GC', F /  ! XYLHRXN
      DATA CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), CONVERT_CONC(  31 ) /   97, 'GC', F /  ! BNZNRXN
      DATA CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), CONVERT_CONC(  32 ) /   98, 'GC', F /  ! BNZHRXN
      DATA CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), CONVERT_CONC(  33 ) /  101, 'GC', F /  ! PAHNRXN
      DATA CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), CONVERT_CONC(  34 ) /  102, 'GC', F /  ! PAHHRXN
      DATA CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), CONVERT_CONC(  35 ) /  103, 'GC', F /  ! SOAALK
      DATA CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), CONVERT_CONC(  36 ) /  104, 'GC', F /  ! ALKRXN
      DATA CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), CONVERT_CONC(  37 ) /  173, 'AE', T /  ! ACLI
      DATA CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), CONVERT_CONC(  38 ) /  172, 'AE', T /  ! ACLJ
      DATA CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), CONVERT_CONC(  39 ) /  175, 'AE', T /  ! ACLK
      DATA CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), CONVERT_CONC(  40 ) /  184, 'AE', T /  ! AGLYJ
      DATA CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), CONVERT_CONC(  41 ) /  117, 'GC', F /  ! PCVOC
      DATA CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), CONVERT_CONC(  42 ) /  118, 'GC', F /  ! PCSOARXN
      DATA CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), CONVERT_CONC(  43 ) /   10, 'GC', F /  ! N2O5
      DATA CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), CONVERT_CONC(  44 ) /   20, 'GC', F /  ! PAN
      DATA CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), CONVERT_CONC(  45 ) /   44, 'GC', F /  ! ETHA
      DATA CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), CONVERT_CONC(  46 ) /   45, 'GC', F /  ! ETOH
      DATA CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), CONVERT_CONC(  47 ) /   34, 'GC', F /  ! XPRP
      DATA CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), CONVERT_CONC(  48 ) /   33, 'GC', F /  ! XPAR
      DATA CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), CONVERT_CONC(  49 ) /   57, 'GC', F /  ! ISOPRXN
      DATA CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), CONVERT_CONC(  50 ) /   61, 'GC', F /  ! HPLD
      DATA CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), CONVERT_CONC(  51 ) /   63, 'GC', F /  ! EPOX
      DATA CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), CONVERT_CONC(  52 ) /   72, 'GC', F /  ! TOL
      DATA CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), CONVERT_CONC(  53 ) /   76, 'GC', F /  ! XYLMN
      DATA CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), CONVERT_CONC(  54 ) /   79, 'GC', F /  ! NAPH
      DATA CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), CONVERT_CONC(  55 ) /   88, 'GC', F /  ! HOCL
      DATA CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), CONVERT_CONC(  56 ) /  106, 'GC', F /  ! H2NO3PK
      DATA CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), CONVERT_CONC(  57 ) /  111, 'GC', F /  ! VIVPO1
      DATA CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), CONVERT_CONC(  58 ) /   13, 'GC', F /  ! PNA
      DATA CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), CONVERT_CONC(  59 ) /   26, 'GC', F /  ! PANX
      DATA CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), CONVERT_CONC(  60 ) /   39, 'GC', F /  ! HCO3
      DATA CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), CONVERT_CONC(  61 ) /   59, 'GC', F /  ! INTR
      DATA CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), CONVERT_CONC(  62 ) /   60, 'GC', F /  ! ISPX
      DATA CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), CONVERT_CONC(  63 ) /   82, 'GC', F /  ! CAT1
      DATA CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), CONVERT_CONC(  64 ) /   84, 'GC', F /  ! OPAN
      DATA CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), CONVERT_CONC(  65 ) /   99, 'GC', F /  ! SESQ
      DATA CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), CONVERT_CONC(  66 ) /  100, 'GC', F /  ! SESQRXN
      DATA CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), CONVERT_CONC(  67 ) /  105, 'GC', F /  ! H2NO3PIJ
      DATA CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), CONVERT_CONC(  68 ) /   12, 'GC', F /  ! HONO
      DATA CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), CONVERT_CONC(  69 ) /   28, 'GC', F /  ! MEPX
      DATA CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), CONVERT_CONC(  70 ) /   50, 'GC', F /  ! ROR
      DATA CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), CONVERT_CONC(  71 ) /   65, 'GC', F /  ! TERP
      DATA CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), CONVERT_CONC(  72 ) /   66, 'GC', F /  ! TRPRXN
      DATA CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), CONVERT_CONC(  73 ) /   71, 'GC', F /  ! BENZRO2
      DATA CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), CONVERT_CONC(  74 ) /   74, 'GC', F /  ! TOLRO2
      DATA CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), CONVERT_CONC(  75 ) /   75, 'GC', F /  ! XOPN
      DATA CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), CONVERT_CONC(  76 ) /   76, 'GC', F /  ! XYLMN
      DATA CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), CONVERT_CONC(  77 ) /   77, 'GC', F /  ! XLO2
      DATA CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), CONVERT_CONC(  78 ) /   78, 'GC', F /  ! XYLRO2
      DATA CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), CONVERT_CONC(  79 ) /   79, 'GC', F /  ! NAPH
      DATA CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), CONVERT_CONC(  80 ) /   80, 'GC', F /  ! PAHRO2
      DATA CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), CONVERT_CONC(  81 ) /   81, 'GC', F /  ! CRO
      DATA CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), CONVERT_CONC(  82 ) /   82, 'GC', F /  ! CAT1
      DATA CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), CONVERT_CONC(  83 ) /   83, 'GC', F /  ! CRON
      DATA CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), CONVERT_CONC(  84 ) /   84, 'GC', F /  ! OPAN
      DATA CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), CONVERT_CONC(  85 ) /   85, 'GC', F /  ! ECH4
      DATA CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), CONVERT_CONC(  86 ) /   86, 'GC', F /  ! CL2
      DATA CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), CONVERT_CONC(  87 ) /   87, 'GC', F /  ! CL
      DATA CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), CONVERT_CONC(  88 ) /   88, 'GC', F /  ! HOCL
      DATA CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), CONVERT_CONC(  89 ) /   89, 'GC', F /  ! CLO
      DATA CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), CONVERT_CONC(  90 ) /   90, 'GC', F /  ! FMCL
      DATA CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), CONVERT_CONC(  91 ) /   91, 'GC', F /  ! HCL
      DATA CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), CONVERT_CONC(  92 ) /   92, 'GC', F /  ! CLNO2
      DATA CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), CONVERT_CONC(  93 ) /   93, 'GC', F /  ! TOLNRXN
      DATA CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), CONVERT_CONC(  94 ) /   94, 'GC', F /  ! TOLHRXN
      DATA CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), CONVERT_CONC(  95 ) /   95, 'GC', F /  ! XYLNRXN
      DATA CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), CONVERT_CONC(  96 ) /   96, 'GC', F /  ! XYLHRXN
      DATA CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), CONVERT_CONC(  97 ) /   97, 'GC', F /  ! BNZNRXN
      DATA CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), CONVERT_CONC(  98 ) /   98, 'GC', F /  ! BNZHRXN
      DATA CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), CONVERT_CONC(  99 ) /   99, 'GC', F /  ! SESQ
      DATA CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), CONVERT_CONC( 100 ) /  100, 'GC', F /  ! SESQRXN
      DATA CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), CONVERT_CONC( 101 ) /  101, 'GC', F /  ! PAHNRXN
      DATA CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), CONVERT_CONC( 102 ) /  102, 'GC', F /  ! PAHHRXN
      DATA CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), CONVERT_CONC( 103 ) /  103, 'GC', F /  ! SOAALK
      DATA CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), CONVERT_CONC( 104 ) /  104, 'GC', F /  ! ALKRXN
      DATA CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), CONVERT_CONC( 105 ) /  105, 'GC', F /  ! H2NO3PIJ
      DATA CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), CONVERT_CONC( 106 ) /  106, 'GC', F /  ! H2NO3PK
      DATA CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), CONVERT_CONC( 107 ) /  182, 'AE', T /  ! ACLI
      DATA CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), CONVERT_CONC( 108 ) /  181, 'AE', T /  ! ACLJ
      DATA CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), CONVERT_CONC( 109 ) /  184, 'AE', T /  ! ACLK
      DATA CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), CONVERT_CONC( 110 ) /  190, 'AE', T /  ! AISO3J
      DATA CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), CONVERT_CONC( 111 ) /  193, 'AE', T /  ! AGLYJ
      DATA CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), CONVERT_CONC( 112 ) /  137, 'AE', T /  ! AXYL1J
      DATA CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), CONVERT_CONC( 113 ) /  191, 'AE', T /  ! AOLGAJ
      DATA CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), CONVERT_CONC( 114 ) /  138, 'AE', T /  ! AXYL2J
      DATA CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), CONVERT_CONC( 115 ) /  140, 'AE', T /  ! ATOL1J
      DATA CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), CONVERT_CONC( 116 ) /  141, 'AE', T /  ! ATOL2J
      DATA CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), CONVERT_CONC( 117 ) /  143, 'AE', T /  ! ABNZ1J
      DATA CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), CONVERT_CONC( 118 ) /  144, 'AE', T /  ! ABNZ2J
      DATA CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), CONVERT_CONC( 119 ) /  149, 'AE', T /  ! ATRP1J
      DATA CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), CONVERT_CONC( 120 ) /  192, 'AE', T /  ! AOLGBJ
      DATA CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), CONVERT_CONC( 121 ) /  150, 'AE', T /  ! ATRP2J
      DATA CGRID_INDEX( 122 ), SPECIES_TYPE( 122 ), CONVERT_CONC( 122 ) /  151, 'AE', T /  ! AISO1J
      DATA CGRID_INDEX( 123 ), SPECIES_TYPE( 123 ), CONVERT_CONC( 123 ) /  152, 'AE', T /  ! AISO2J
      DATA CGRID_INDEX( 124 ), SPECIES_TYPE( 124 ), CONVERT_CONC( 124 ) /  153, 'AE', T /  ! ASQTJ
      DATA CGRID_INDEX( 125 ), SPECIES_TYPE( 125 ), CONVERT_CONC( 125 ) /  146, 'AE', T /  ! APAH1J
      DATA CGRID_INDEX( 126 ), SPECIES_TYPE( 126 ), CONVERT_CONC( 126 ) /  147, 'AE', T /  ! APAH2J
      DATA CGRID_INDEX( 127 ), SPECIES_TYPE( 127 ), CONVERT_CONC( 127 ) /  135, 'AE', T /  ! AALK1J
      DATA CGRID_INDEX( 128 ), SPECIES_TYPE( 128 ), CONVERT_CONC( 128 ) /  136, 'AE', T /  ! AALK2J
      DATA CGRID_INDEX( 129 ), SPECIES_TYPE( 129 ), CONVERT_CONC( 129 ) /  117, 'GC', F /  ! PCVOC
      DATA CGRID_INDEX( 130 ), SPECIES_TYPE( 130 ), CONVERT_CONC( 130 ) /  118, 'GC', F /  ! PCSOARXN
      DATA CGRID_INDEX( 131 ), SPECIES_TYPE( 131 ), CONVERT_CONC( 131 ) /  107, 'GC', F /  ! VLVPO1
      DATA CGRID_INDEX( 132 ), SPECIES_TYPE( 132 ), CONVERT_CONC( 132 ) /  108, 'GC', F /  ! VSVPO1
      DATA CGRID_INDEX( 133 ), SPECIES_TYPE( 133 ), CONVERT_CONC( 133 ) /  109, 'GC', F /  ! VSVPO2
      DATA CGRID_INDEX( 134 ), SPECIES_TYPE( 134 ), CONVERT_CONC( 134 ) /  110, 'GC', F /  ! VSVPO3
      DATA CGRID_INDEX( 135 ), SPECIES_TYPE( 135 ), CONVERT_CONC( 135 ) /  111, 'GC', F /  ! VIVPO1
      DATA CGRID_INDEX( 136 ), SPECIES_TYPE( 136 ), CONVERT_CONC( 136 ) /  112, 'GC', F /  ! VLVOO1
      DATA CGRID_INDEX( 137 ), SPECIES_TYPE( 137 ), CONVERT_CONC( 137 ) /  113, 'GC', F /  ! VLVOO2
      DATA CGRID_INDEX( 138 ), SPECIES_TYPE( 138 ), CONVERT_CONC( 138 ) /  115, 'GC', F /  ! VSVOO2
      DATA CGRID_INDEX( 139 ), SPECIES_TYPE( 139 ), CONVERT_CONC( 139 ) /  116, 'GC', F /  ! VSVOO3
      DATA CGRID_INDEX( 140 ), SPECIES_TYPE( 140 ), CONVERT_CONC( 140 ) /  114, 'GC', F /  ! VSVOO1
      DATA CGRID_INDEX( 141 ), SPECIES_TYPE( 141 ), CONVERT_CONC( 141 ) /  119, 'GC', F /  ! FORM_PRIMARY
      DATA CGRID_INDEX( 142 ), SPECIES_TYPE( 142 ), CONVERT_CONC( 142 ) /  120, 'GC', F /  ! ALD2_PRIMARY
      DATA CGRID_INDEX( 143 ), SPECIES_TYPE( 143 ), CONVERT_CONC( 143 ) /  121, 'GC', F /  ! BUTADIENE13
      DATA CGRID_INDEX( 144 ), SPECIES_TYPE( 144 ), CONVERT_CONC( 144 ) /  122, 'GC', F /  ! ACROLEIN
      DATA CGRID_INDEX( 145 ), SPECIES_TYPE( 145 ), CONVERT_CONC( 145 ) /  123, 'GC', F /  ! ACRO_PRIMARY
      DATA CGRID_INDEX( 146 ), SPECIES_TYPE( 146 ), CONVERT_CONC( 146 ) /  124, 'GC', F /  ! TOLU
      DATA CGRID_INDEX( 147 ), SPECIES_TYPE( 147 ), CONVERT_CONC( 147 ) /  125, 'GC', F /  ! HG
      DATA CGRID_INDEX( 148 ), SPECIES_TYPE( 148 ), CONVERT_CONC( 148 ) /  126, 'GC', F /  ! HGIIAER
      DATA CGRID_INDEX( 149 ), SPECIES_TYPE( 149 ), CONVERT_CONC( 149 ) /  127, 'GC', F /  ! HGIIGAS

      INTEGER, PARAMETER :: N_ACT_SP = 149

      INTEGER, PARAMETER :: NRXNS = 329

      INTEGER, PARAMETER ::    ZERO_REACT_REACTIONS =   0

      INTEGER, PARAMETER ::     ONE_REACT_REACTIONS =  78

      INTEGER, PARAMETER ::     TWO_REACT_REACTIONS = 219

      INTEGER, PARAMETER ::   THREE_REACT_REACTIONS =   0

      INTEGER, PARAMETER ::         ONE_REACT_START =   1

      INTEGER, PARAMETER ::        ZERO_REACT_START =  78

      INTEGER, PARAMETER ::         TWO_REACT_START =  79

      INTEGER, PARAMETER ::       THREE_REACT_START = 297

      INTEGER, PARAMETER :: NSUNLIGHT_RXNS =  34

      INTEGER, PARAMETER :: NTHERMAL_RXNS  = 263

      INTEGER            :: KUNITS

      DATA  KUNITS /   2 /

      INTEGER IRXXN

      REAL( 8 )          :: RTDAT( 3,NRXNS )

      INTEGER, PARAMETER :: NFALLOFF =  21
      REAL( 8 )          :: RFDAT( 5,NFALLOFF )

      INTEGER            :: KTYPE( NRXNS )

      DATA ( KTYPE( IRXXN ), IRXXN = 1, NRXNS ) /  & 
     &      0,    2,    3,    2,    3,   10,    3,    0,    0,    3, & ! O   
     &      1,    3,    4,    3,    3,    4,   10,    3,    9,    9, & ! 1   
     &      0,    3,    3,    3,    3,    3,    0,    0,    3,    3, & ! 2   
     &      1,    1,    1,    1,    3,   10,   10,    0,    1,   10, & ! 3   
     &      1,    1,    0,    3,   10,    8,    0,   10,   10,    0, & ! 4   
     &      3,   10,    3,   10,   10,    0,    3,    3,    3,    3, & ! 5   
     &      3,    6,    6,    0,    3,    3,    3,    3,    3,    3, & ! 6   
     &      3,    3,    3,    6,    3,    3,    6,    6,    6,    6, & ! 7   
     &      6,    6,    6,    6,    6,    6,    3,    0,    3,    0, & ! 8   
     &      1,    0,    1,    3,    3,    3,    0,    0,    3,    1, & ! 9   
     &      3,    3,    1,    3,    3,    3,    3,    0,    3,    3, & ! O   
     &      1,    0,    1,    0,    3,    3,    0,    3,    0,    3, & ! 1   
     &      3,    3,    9,    3,    3,    3,    3,    0,    0,    3, & ! 2   
     &      3,    1,    3,    3,    3,   10,    3,   10,    3,    3, & ! 3   
     &      3,   10,    3,    3,    1,    3,    3,    1,    3,    1, & ! 4   
     &      3,    3,    6,    6,    3,    3,    3,    3,    3,    3, & ! 5   
     &      0,    3,    0,    3,    3,    3,    3,    6,    6,    1, & ! 6   
     &      1,    3,    3,    3,    3,    3,    6,    3,    6,    3, & ! 7   
     &      3,    6,    3,    6,    1,    1,    3,    3,    6,    6, & ! 8   
     &      3,    1,    1,    1,    1,    1,    0,    0,    1,    3, & ! 9   
     &      1,    0,    1,    3,    1,    1,    1,    1,    6,    6, & ! O   
     &      6,    6,    6,    1,    1,    3,   10,    1,   10,    1, & ! 1   
     &      0,    0,    3,    1,    3,    3,    1,    0,    3,    1, & ! 2   
     &      3,    1,    1,    1,    1,    3,    1,    1,    1,    3, & ! 3   
     &      4,    1,    1,    1,    1,    0,    3,    3,    3,    3, & ! 4   
     &      3,    3,    1,    1,    1,    3,    3,    3,   -1,   -1, & ! 5   
     &     -1,   -1,   -1,   -1,   -1,   -1,   -1,   12,   -1,   -1, & ! 6   
     &     -1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    3,    1,    3, & ! 9   
     &      0,    0,    3,    3,    3,    3,    0,    1,    3,    3, & ! O   
     &      1,    1,    1,    1,    1,    0,    1,    1,    1,    1, & ! 1   
     &      0,    1,    3,    1,    3,    1,    1,    1,    3/     !2   

      INTEGER            :: IRXBITS( NRXNS )

      DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,  276,  260,    8,    1,    8,    1, & ! 3   
     &      1,    0,    0,  128,   64,    0,   16,    0,    0,    1, & ! 4   
     &      0,    1,    0,   64,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  260, & ! 7   
     &      0,    1,    0,    0,    0,    0,    0,    0,    1,    0, & ! 8   
     &      0,    8,    0,    0,   16,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    1,    1,    8,    0,    0,    1, & ! O   
     &      0,    1,    0,    1,    0,    1,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    1,    0,    1,    0, & ! 7   
     &      0,    0,    1,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    2,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    0,    1,    1, & ! 6   
     &      1,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      2,    2,    0,    0,    0,    0,    2,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    2,    0,    0,    0,    0, & ! 1   
     &      2,    0,    0,    0,    0,    0,    0,    0,  260/     !2   

      INTEGER, PARAMETER :: NTERMS_JACOB =  1532

      INTEGER, PARAMETER :: NSTEPS_JACOB =   9881

      INTEGER            :: IORDER( NRXNS )

      DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    3,    2,    2,    1,    2,    1, & ! 3   
     &      1,    1,    1,    2,    2,    1,    2,    1,    1,    1, & ! 4   
     &      1,    1,    1,    2,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    2,    3, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    3,    2,    2,    3,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    3,    2,    2,    2, & ! O   
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
     &      2,    2,    2,    2,    2,    1,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    1, & ! 5   
     &      1,    1,    1,    2,    2,    2,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      1,    1,    2,    2,    2,    2,    1,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    1,    2,    2,    2,    2, & ! 1   
     &      1,    2,    2,    2,    2,    2,    2,    2,    3/     !2   

      INTEGER, PARAMETER :: KTN1 =  97
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     11,   31,   32,   33,   34,   39,   41,   42,   91,   93, & ! O   
     &    100,  103,  111,  113,  132,  145,  148,  150,  170,  171, & ! 1   
     &    185,  186,  192,  193,  194,  195,  196,  199,  201,  203, & ! 2   
     &    205,  206,  207,  208,  214,  215,  218,  220,  224,  227, & ! 3   
     &    230,  232,  233,  234,  235,  237,  238,  239,  242,  243, & ! 4   
     &    244,  245,  253,  254,  255,  272,  273,  274,  275,  276, & ! 5   
     &    277,  278,  279,  280,  281,  282,  283,  284,  285,  286, & ! 6   
     &    287,  288,  289,  290,  291,  292,  293,  294,  295,  296, & ! 7   
     &    297,  299,  308,  311,  312,  313,  314,  315,  317,  318, & ! 8   
     &    319,  320,  322,  324,  326,  327,  328/     !  9   

      INTEGER, PARAMETER :: KTN2 =   2
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &      2,    4/

      INTEGER, PARAMETER :: KTN3 = 128
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &      3,    5,    7,   10,   12,   14,   15,   18,   22,   23, & ! O   
     &     24,   25,   26,   29,   30,   35,   44,   51,   53,   57, & ! 1   
     &     58,   59,   60,   61,   65,   66,   67,   68,   69,   70, & ! 2   
     &     71,   72,   73,   75,   76,   87,   89,   94,   95,   96, & ! 3   
     &     99,  101,  102,  104,  105,  106,  107,  109,  110,  115, & ! 4   
     &    116,  118,  120,  121,  122,  124,  125,  126,  127,  130, & ! 5   
     &    131,  133,  134,  135,  137,  139,  140,  141,  143,  144, & ! 6   
     &    146,  147,  149,  151,  152,  155,  156,  157,  158,  159, & ! 7   
     &    160,  162,  164,  165,  166,  167,  172,  173,  174,  175, & ! 8   
     &    176,  178,  180,  181,  183,  187,  188,  191,  200,  204, & ! 9   
     &    216,  223,  225,  226,  229,  231,  236,  240,  247,  248, & ! O   
     &    249,  250,  251,  252,  256,  257,  258,  298,  300,  303, & ! 1   
     &    304,  305,  306,  309,  310,  323,  325,  329/     !  2   

      INTEGER, PARAMETER :: KTN4 =   3
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &     13,   16,  241/

      INTEGER, PARAMETER :: KTN5 =   0
      INTEGER            :: KRX5( 1 )

      DATA   KRX5( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN6 =  28
      INTEGER            :: KRX6( KTN6 )

      DATA ( KRX6( IRXXN ), IRXXN = 1, KTN6 ) / & 
     &     62,   63,   74,   77,   78,   79,   80,   81,   82,   83, & 
     &     84,   85,   86,  153,  154,  168,  169,  177,  179,  182, & 
     &    184,  189,  190,  209,  210,  211,  212,  213/

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   4
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &      2,    4,   10,  329/
>>>>>>> master
      REAL,    PARAMETER :: ATM_AIR = 1.00000E+06

      INTEGER, PARAMETER :: NWW =   4
      INTEGER            :: NRXWW( NWW )

      DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) / & 
     &     37,   39,   92,  107/

      INTEGER, PARAMETER :: NWO2 =   3
      INTEGER            :: NRXWO2( NWO2 )

      DATA ( NRXWO2( IRXXN ), IRXXN = 1, NWO2 ) / & 
     &     35,   47,   95/
      REAL,    PARAMETER :: ATM_O2 = 2.09500E+05

      INTEGER, PARAMETER :: NWN2 =   0
      INTEGER            :: NRXWN2( 1 )

      DATA   NRXWN2( 1 ) / 0 /
      REAL,    PARAMETER :: ATM_N2 = 7.80800E+05

      INTEGER, PARAMETER :: NWCH4 =   2
      INTEGER            :: NRXWCH4( NWCH4 )

      DATA ( NRXWCH4( IRXXN ), IRXXN = 1, NWCH4 ) / & 
     &     45,   54/
      REAL,    PARAMETER :: ATM_CH4 = 1.85000E+00

      INTEGER, PARAMETER :: NWH2 =   1
      INTEGER            :: NRXWH2( NWH2 )

      DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & 
     &     44/
      REAL,    PARAMETER :: ATM_H2 = 5.60000E-01

      INTEGER, PARAMETER :: MXPRD =  14
      INTEGER            :: IRR( NRXNS,MXPRD+3 )

      DATA ( IRR( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    4,    3,    3,    3,    3,    4,    4,    6, & ! O   
     &      6,    4,    4,    7,    8,    7,    7,    7,    8,    8, & ! 1   
     &      9,    9,    9,    2,    8,    1,    5,    5,    5,    5, & ! 2   
     &      5,    5,    5,    5,    5,    5,   10,   10,   10,    2, & ! 3   
     &      2,   12,   12,   12,    1,   11,   11,    8,   13,   13, & ! 4   
     &     13,   14,   17,   17,   20,   20,   17,   17,   17,   17, & ! 5   
     &     23,   23,   26,   26,   23,   23,   23,   19,   19,   19, & ! 6   
     &     18,   18,   18,   18,   25,   25,   25,   25,   31,   31, & ! 7   
     &     31,   31,   32,   32,   32,   32,   28,   28,   30,   30, & ! 8   
     &     33,   33,   35,   22,   21,   27,   27,   27,   27,   27, & ! 9   
     &     27,   37,   37,   37,   24,   24,   24,   24,   38,   38, & ! O   
     &     38,   38,   39,   39,   39,   40,   40,   40,   41,   41, & ! 1   
     &     41,    7,   36,    7,   42,   29,   43,   44,   46,   46, & ! 2   
     &     47,   45,   50,   50,   50,   51,   52,   52,   52,   52, & ! 3   
     &     53,   53,   53,   53,   54,   54,   54,   54,   55,   55, & ! 4   
     &     56,   56,   56,   56,   56,   55,   55,   58,   58,   58, & ! 5   
     &     58,   60,   61,   61,   63,   64,   64,   64,   64,   59, & ! 6   
     &     65,   65,   65,   65,   67,   69,   69,   69,   69,   72, & ! 7   
     &     73,   73,   73,   73,   76,   79,   77,   77,   77,   77, & ! 8   
     &     68,   68,   81,   81,   83,   83,   83,   75,   75,   75, & ! 9   
     &     75,   70,   70,   70,   70,   82,   82,   62,   62,   84, & ! O   
     &     62,   62,   62,   84,   26,   85,   48,   48,   49,   49, & ! 1   
     &     86,   88,   87,   89,   89,   89,    7,   90,   87,   87, & ! 2   
     &     87,   87,   87,   87,   87,   87,   87,   87,   87,   87, & ! 3   
     &     91,   87,   87,   87,   89,   92,   74,   74,   78,   78, & ! 4   
     &     71,   71,   99,   99,   99,   80,   80,  103,   34,   10, & ! 5   
     &     10,  105,  106,  105,  105,  106,    1,    4,   63,   40, & ! 6   
     &     41,  112,  114,  115,  116,  117,  118,  119,  121,  122, & ! 7   
     &    123,  124,  125,  126,  127,  128,  129,  131,  132,  133, & ! 8   
     &    134,  135,  136,  137,  140,  138,  139,  141,  141,  141, & ! 9   
     &    141,  141,  141,  142,  142,  142,  142,  142,  143,  143, & ! O   
     &    143,  143,  145,  145,  145,  145,  145,  144,  144,  144, & ! 1   
     &    144,  144,  146,  146,  147,  147,  147,  147,  147/     !2   

      DATA ( IRR( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    2,    2,    1,    1,    4,    0,    0,    0, & ! O   
     &      0,    7,    8,    3,    3,    7,    7,    8,    8,    8, & ! 1   
     &      0,    7,    3,    2,    2,    4,    0,    0,    2,    1, & ! 2   
     &      3,    7,    8,    4,    5,    1,    0,    0,    0,    7, & ! 3   
     &      1,   12,    0,    7,    7,    7,    0,    1,    0,    0, & ! 4   
     &      7,    7,    2,    1,    0,    0,    8,   19,   17,   23, & ! 5   
     &      2,    1,    0,    0,    8,   19,   23,    2,    8,   19, & ! 6   
     &      2,    8,   17,   19,    2,    8,   17,   19,    2,    8, & ! 7   
     &     17,   19,    2,    8,   17,   19,    7,    0,    7,    0, & ! 8   
     &      7,    0,    7,    7,    7,    7,    0,    0,    3,    5, & ! 9   
     &      8,    0,    2,    8,    3,    7,    5,    0,    3,    7, & ! O   
     &      5,    0,    7,    0,    5,    7,    0,    5,    0,    5, & ! 1   
     &      7,    0,    7,    0,    7,    7,    7,    0,    0,    7, & ! 2   
     &      7,    7,    0,    0,    1,    7,    3,    7,    4,    5, & ! 3   
     &      3,    7,    4,    5,    3,    7,    4,    5,    7,    3, & ! 4   
     &      2,    8,   17,   19,    0,    4,    5,    7,    4,    5, & ! 5   
     &      0,    7,    0,    5,    7,    8,    2,   17,   19,    7, & ! 6   
     &      3,    7,    4,    5,    7,    2,   17,    8,   19,    7, & ! 7   
     &      2,   17,    8,   19,    7,    7,    2,    8,   17,   19, & ! 8   
     &      7,    5,    1,    8,    7,    5,    0,    0,    7,    4, & ! 9   
     &      5,    0,    7,    4,    5,    7,    5,    2,    1,    0, & ! O   
     &      8,   17,   19,    7,    7,    7,    0,    0,    0,    0, & ! 1   
     &      0,    0,    4,   89,    2,    8,   90,    0,    0,   45, & ! 2   
     &     42,   52,   53,   54,   55,   27,   24,   38,   29,   43, & ! 3   
     &      7,   72,   76,   79,    1,    0,    2,    8,    2,    8, & ! 4   
     &      2,    8,    4,    7,    5,    2,    8,    7,    0,    0, & ! 5   
     &      0,    0,    0,  107,  108,  109,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    7,    7,    7,    7, & ! 8   
     &      7,    7,    7,    7,    7,    7,    7,    7,    5,    3, & ! 9   
     &      0,    0,   87,    7,    5,    3,    0,   87,    7,    4, & ! O   
     &      5,   87,    7,    4,    5,    0,   87,    7,    4,    5, & ! 1   
     &      0,   87,    7,   87,    4,   86,    9,    7,   87/     !2   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !2   

      DATA ( IRR( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &    130,  118,   17,  140,  137,  130,  137,  130,  140,  139, & ! O   
     &    137,  137,  128,  139,  137,  139,  133,  128,  126,  131, & ! 1   
     &    139,  134,  126,  133,  139,  140,   68,  125,  112,  127, & ! 2   
     &    140,  127,  127,    0,  121,  118,  140,  135,  122,  139, & ! 3   
     &    137,  137,  131,  139,  128,   21,   21,  139,  112,  129, & ! 4   
     &     83,  129,  123,  115,  122,  122,  122,  122,  122,   68, & ! 5   
     &      1,   40,   40,  106,  106,  106,  106,  106,  106,   78, & ! 6   
     &     78,   78,   78,   78,  106,  106,  106,  106,  137,  137, & ! 7   
     &    130,  135,    0,  139,  140,  139,  140,  118,   91,    0, & ! 8   
     &     91,   91,  139,  140,  137,  140,  135,  137,  130,  137, & ! 9   
     &    139,  140,  137,  137,   43,   68,   68,  130,  137,  122, & ! O   
     &    135,   58,  137,   19,  137,   44,   80,  134,  128,  128, & ! 1   
     &    137,   59,   80,  126,  126,  130,  139,    0,  131,   69, & ! 2   
     &    131,  131,  137,   82,  139,  139,  137,   82,  128,  138, & ! 3   
     &    105,   82,  139,  138,  128,  136,  117,  139,  128,  134, & ! 4   
     &    139,  140,  122,   60,  110,   69,  134,  134,  134,  120, & ! 5   
     &    120,  120,  125,  122,  133,  122,  122,  134,  139,  126, & ! 6   
     &    131,  126,  131,   47,   48,  105,  125,  131,  136,  131, & ! 7   
     &    137,  126,  131,  126,  137,  126,  126,  126,  137,   99, & ! 8   
     &    113,   61,   62,  131,  131,  131,  137,  129,  126,  122, & ! 9   
     &     51,  122,   93,  111,  111,  111,  111,  132,  123,  136, & ! O   
     &    140,  137,  104,  137,  125,    0,  125,  104,  137,  125, & ! 1   
     &      0,  125,  104,  104,  137,    0,  125,  125,  125,  101, & ! 2   
     &     77,  104,  117,  117,  119,  119,  137,  112,  125,  112, & ! 3   
     &    131,  101,  137,   64,   80,  128,  136,  137,  126,  128, & ! 4   
     &     86,   26,  127,   55,  127,  115,  115,   96,   96,  115, & ! 5   
     &    115,  115,  115,  115,  115,  115,  127,  115,  115,  115, & ! 6   
     &    102,  130,  139,  130,  139,  130,  139,  121,  140,  135, & ! 7   
     &    130,  139,  140,  102,  102,  102,  140,  140,  140,  140, & ! 8   
     &    140,  140,  140,  140,  140,  140,  140/     !  9   
     &      2,    4,    1,    1,    2,    5,    0,    3,    6,    3, & ! O   
     &      7,    8,    7,    8,    7,    3,    9,    0,    9,    9, & ! 1   
     &      7,    8,    7,    1,    7,    5,    1,    2,    1,    2, & ! 2   
     &      1,    8,    7,    1,    1,   10,    5,    1,   11,   12, & ! 3   
     &     12,    2,    2,    1,   11,    5,    7,   13,    8,    8, & ! 4   
     &      1,   15,    1,   20,    1,    1,   21,   17,   18,   18, & ! 5   
     &      1,   26,    1,    1,   21,   24,   24,    2,    8,    0, & ! 6   
     &     27,   28,   27,   27,    1,   30,    8,    8,    1,   30, & ! 7   
     &     18,   19,   33,   30,    8,   19,   18,   18,   25,    8, & ! 8   
     &     34,    1,    8,   18,   17,    8,    8,   36,    7,   11, & ! 9   
     &     37,   27,   35,   28,   17,   17,   17,   18,   23,   23, & ! O   
     &     23,   24,   40,   27,   11,   36,    8,   11,   17,   11, & ! 1   
     &     17,    8,    8,   18,   24,   27,   24,   24,   36,   27, & ! 2   
     &     48,   49,   44,   44,   33,   40,   27,   25,   27,    1, & ! 3   
     &     24,   27,   24,    1,   24,   24,   24,    1,   56,   58, & ! 4   
     &     59,   60,   27,   27,    8,   27,    1,   32,   24,   11, & ! 5   
     &      8,   63,    7,   11,   64,   39,   39,   39,   39,   31, & ! 6   
     &     38,   25,    7,    1,   68,    1,   40,    0,   40,   68, & ! 7   
     &      1,   40,    0,   40,   68,   68,    1,    0,   40,   40, & ! 8   
     &     40,   81,   83,   68,   34,   34,   12,   40,   41,   41, & ! 9   
     &      1,   62,   62,   40,   62,   27,   81,    1,   84,   62, & ! O   
     &     21,   18,   25,    1,   24,   18,   32,   46,   32,   38, & ! 1   
     &     87,    7,   89,   86,   87,   88,   87,   87,   91,   91, & ! 2   
     &     91,   90,   90,   91,   91,   91,   91,   91,   91,   91, & ! 3   
     &     87,   91,   91,   91,   92,   87,    2,    8,    2,    8, & ! 4   
     &      2,    8,    4,    7,    5,    2,    8,    7,   11,   11, & ! 5   
     &     11,   11,   11,   92,   92,   92,   12,    0,  110,  111, & ! 6   
     &    111,  113,  113,  113,  113,  113,  113,  120,  120,  120, & ! 7   
     &    120,  120,  113,  113,  113,  113,    7,    7,    7,    7, & ! 8   
     &      7,    7,    7,    7,    7,    7,    7,    7,    5,    3, & ! 9   
     &      0,    0,   87,    7,    5,    3,    0,   87,    7,    4, & ! O   
     &      5,   87,    7,    4,    5,    0,   87,    7,    4,    5, & ! 1   
     &      0,   87,    7,   87,  148,  149,  149,  148,  147/     !2   
>>>>>>> master

      DATA ( IRR( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &    118,    0,    0,    0,  118,    0,  135,  140,  137,  137, & ! O   
     &    134,  120,  138,  140,    0,  133,    0,  138,  136,  133, & ! 1   
     &    133,  139,  134,  128,  136,  113,  139,  136,  139,    0, & ! 2   
     &    127,  133,  137,    0,    0,    0,    0,  137,    0,  137, & ! 3   
     &    134,  120,  139,    0,  138,   83,  139,   50,  137,  138, & ! 4   
     &    123,  138,   70,  128,    0,   67,   56,    0,    0,  122, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
<<<<<<< HEAD
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,  139,    0,  137,    0,    0,  137,    0, & ! 9   
     &    137,  137,    0,    0,    0,    0,    0,  137,    0,    0, & ! O   
     &      0,    0,    0,  139,  128,    0,  116,    0,  138,  126, & ! 1   
     &    126,    0,  116,  136,  136,    0,    0,    0,  139,  131, & ! 2   
     &    139,   81,  139,    0,  128,  138,    0,    0,  116,    0, & ! 3   
     &    117,    0,  128,    0,  138,  129,    0,    0,  138,    0, & ! 4   
     &    133,  139,  139,    0,  137,  110,  140,    0,  122,  140, & ! 5   
     &      0,  122,  139,  134,  132,  133,  134,  133,    0,  136, & ! 6   
     &    139,  139,  134,    0,    0,    0,  140,  139,  138,  133, & ! 7   
     &    105,  123,  126,  131,  105,  123,  123,  123,  105,  138, & ! 8   
     &    131,  137,  140,  113,  113,  113,  117,  132,  131,  117, & ! 9   
     &    140,  113,  138,  125,  125,  125,  125,  136,  124,  132, & ! O   
     &    136,  136,   94,  117,  114,    0,  114,   95,  117,  119, & ! 1   
     &      0,  119,  100,  100,  117,    0,  119,  119,  114,  122, & ! 2   
     &      0,    0,  101,  101,  125,  140,  117,  136,  119,  122, & ! 3   
     &    139,  122,  125,    0,  116,  132,  123,  125,  137,  138, & ! 4   
     &      0,  127,  137,    0,  133,  132,  126,  132,  126,   96, & ! 5   
     &    132,  139,  134,  120,  139,  139,    0,  132,  132,  132, & ! 6   
     &      0,   27,   28,   29,   30,   31,   32,   66,   66,   66, & ! 7   
     &     33,   34,   36,    0,    0,    0,   42,   87,   87,   87, & ! 8   
     &     87,   87,  108,  108,  108,  108,  108/     !  9   
=======
     &      0,    0,    0,    0,    0,    0,  130,  131,  131,  131, & ! 8   
     &    131,  131,  136,  136,  136,  136,  136,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,  144,  144, & ! O   
     &    144,  144,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,  149,   86,    9,  149,  149/     !2   
>>>>>>> master

      DATA ( IRR( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  140, & ! O   
     &    135,  135,  140,    0,    0,    0,    0,  133,  138,  139, & ! 1   
     &      0,  133,  136,  138,  132,    0,  131,  139,  133,    0, & ! 2   
     &      0,  139,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,  126,    0,    0,    0,    0, & ! 4   
     &    124,    0,  136,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
<<<<<<< HEAD
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,   20,  138,    0,  121,    0,    0,  136, & ! 1   
     &    136,    0,  121,  138,  138,    0,    0,    0,  137,    0, & ! 2   
     &    128,  139,    0,    0,  116,    0,    0,    0,  138,    0, & ! 3   
     &      0,    0,  116,    0,  131,  138,    0,    0,    0,    0, & ! 4   
     &      0,  133,  133,    0,  139,  140,    0,    0,    0,    0, & ! 5   
     &      0,    0,  134,    0,  138,  132,  132,    0,    0,  129, & ! 6   
     &      0,  136,  132,    0,    0,    0,  110,  133,  131,  139, & ! 7   
     &    136,  139,  123,  123,  132,  136,  136,  131,  132,   49, & ! 8   
     &    132,  131,  139,  139,  139,  123,  136,  119,  119,  137, & ! 9   
     &     99,    0,    0,  119,  119,  119,  119,  138,   72,  129, & ! O   
     &    132,  132,  138,  125,  139,    0,  139,  138,  125,  114, & ! 1   
     &      0,  114,  138,  138,  125,    0,  114,  114,  139,  132, & ! 2   
     &      0,    0,    0,  122,  136,  134,  136,  138,  140,    0, & ! 3   
     &    101,    0,  133,    0,  121,  123,  138,  133,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,  129,  132,  139,  123,  126, & ! 5   
     &    139,  133,    0,    0,  131,  126,    0,  139,  139,  139, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,   88,   88,   88, & ! 8   
     &     88,   88,  109,  109,  109,  109,  109/     !  9   
=======
     &      0,    0,    0,    0,    0,    0,    0,  132,  132,  132, & ! 8   
     &    132,  132,  137,  137,  137,  137,  137,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    4,    0,    0,    7,   87/     !2   
>>>>>>> master

      DATA ( IRR( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  135, & ! O   
     &    128,  126,    0,    0,    0,    0,    0,  139,  133,   81, & ! 1   
     &      0,    0,  120,  134,  128,    0,  114,  133,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,  123,    0,    0,    0,    0, & ! 4   
     &    136,    0,  132,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
<<<<<<< HEAD
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,  128,    0,    0,  138, & ! 1   
     &    138,    0,  126,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &    116,  138,    0,    0,  138,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,  138,    0,  140,  140,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,  139,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,  139,  138,  138,    0,    0,  138, & ! 6   
     &      0,  138,  138,    0,    0,    0,  133,  136,  111,  140, & ! 7   
     &    132,  136,  136,  136,  136,  138,  138,  133,  136,    0, & ! 8   
     &    138,  113,  131,  136,  136,  120,  132,  128,  125,  132, & ! 9   
     &    138,    0,    0,  140,  140,  140,  140,  137,    0,  138, & ! O   
     &    129,  129,  114,  114,  128,    0,  138,  114,  119,  107, & ! 1   
     &      0,  107,  107,  107,  119,    0,  107,  107,  101,  136, & ! 2   
     &      0,    0,    0,    0,  138,  126,  132,  125,  134,    0, & ! 3   
     &      0,    0,  139,    0,  123,  138,  116,  117,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,  139,  129,  131,  132,  123, & ! 5   
     &     96,    0,    0,    0,    0,    0,    0,  129,  129,  129, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,   89,   89,   89, & ! 8   
     &     89,   89,   90,   90,   90,   90,   90/     !  9   
=======
     &      0,    0,    0,    0,    0,    0,    0,  133,  133,  133, & ! 8   
     &    133,  133,  140,  140,  140,  140,  140,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !2   
>>>>>>> master

      DATA ( IRR( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &    138,  136,    0,    0,    0,    0,    0,    0,  139,  140, & ! 1   
     &      0,    0,  128,    0,  134,    0,    0,  134,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,  129,    0,    0,    0,    0, & ! 4   
     &    138,    0,  138,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,  138,    0,    0,    0, & ! 1   
     &      0,    0,  136,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &    138,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,  139,    0,    0,    0,    0, & ! 6   
     &      0,  131,    0,    0,    0,    0,  139,  138,    0,  110, & ! 7   
     &    138,  133,  132,  138,  129,  133,    0,  140,  129,    0, & ! 8   
     &    139,  139,  113,  128,  138,  124,  129,  111,   83,  136, & ! 9   
     &     92,    0,    0,  139,  139,  139,  139,  135,    0,  131, & ! O   
     &    138,  138,  140,  139,  138,    0,    0,  140,  114,  139, & ! 1   
     &      0,  139,  140,  140,  114,    0,  139,  139,   63,  125, & ! 2   
     &      0,    0,    0,    0,    0,  133,  129,    0,  131,    0, & ! 3   
     &      0,    0,  120,    0,  136,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,  126,  139,    0,  139,  103, & ! 5   
     &    113,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
<<<<<<< HEAD
     &      0,    0,    0,    0,    0,    0,    0,   79,   79,   79, & ! 8   
     &     79,  108,   97,   97,   97,   97,   97/     !  9   
=======
     &      0,    0,    0,    0,    0,    0,    0,  134,  134,  134, & ! 8   
     &    134,  136,  138,  138,  138,  138,  138,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !2   
>>>>>>> master

      DATA ( IRR( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,  138,    0,    0,    0,    0,    0,    0,    0,  125, & ! 1   
     &      0,    0,  138,    0,  131,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,  136,    0,    0,    0,    0, & ! 4   
     &      0,    0,  124,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,  140,    0,    0,    0, & ! 1   
     &      0,    0,  138,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,  111,    0,    0,    0,    0,    0,  140,    0,    0, & ! 7   
     &    131,  131,  129,  140,  138,  124,    0,  136,  138,    0, & ! 8   
     &    120,  136,    0,  116,    0,  140,  138,  134,  133,  111, & ! 9   
     &    123,    0,    0,  131,  131,  131,  131,   61,    0,  124, & ! O   
     &    131,  123,  139,    0,    0,    0,    0,  136,  107,  128, & ! 1   
     &      0,  138,  136,  136,  107,    0,  128,  138,  129,  119, & ! 2   
     &      0,    0,    0,    0,    0,  136,  138,    0,  126,    0, & ! 3   
     &      0,    0,    0,    0,  138,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,  124,    0,    0,  124,  124, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
<<<<<<< HEAD
     &      0,    0,    0,    0,    0,    0,    0,   57,  108,  108, & ! 8   
     &    108,  109,   98,   98,   98,   98,   98/     !  9   
=======
     &      0,    0,    0,    0,    0,    0,    0,  135,  136,  136, & ! 8   
     &    136,  137,  139,  139,  139,  139,  139,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !2   
>>>>>>> master

      DATA ( IRR( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  136, & ! 1   
     &      0,    0,  124,    0,  103,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,  138,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,  140,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
<<<<<<< HEAD
     &      0,  129,  138,  139,  131,    0,    0,  138,  126,    0, & ! 8   
     &    124,  138,    0,  138,    0,  132,  131,  112,  140,  119, & ! 9   
     &      0,    0,    0,  110,  137,  133,  133,  131,    0,  123, & ! O   
     &    133,  117,   73,    0,    0,    0,    0,  139,  139,  138, & ! 1   
     &      0,    0,  139,  139,  139,    0,  138,    0,  138,  112, & ! 2   
     &      0,    0,    0,    0,    0,  138,  114,    0,  133,    0, & ! 3   
     &      0,    0,    0,    0,  140,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,   70,    0,    0,    0,  132, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,  108,  109,  109, & ! 8   
     &    109,    0,    0,    0,    0,    0,    0/     !  9   
=======
     &      0,    0,    0,    0,    0,    0,    0,  136,  137,  137, & ! 8   
     &    137,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !2   
>>>>>>> master

      DATA ( IRR( IRXXN, 11 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  138, & ! 1   
     &      0,    0,    0,    0,  124,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,   70,    0,    0,    0,    0, & ! 4   
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
<<<<<<< HEAD
     &      0,  138,  124,  133,  126,    0,    0,  125,  123,    0, & ! 8   
     &      0,    0,    0,    0,    0,  138,  113,  124,  110,  124, & ! 9   
     &      0,    0,    0,  133,  133,  124,  124,  111,    0,   72, & ! O   
     &    124,   72,    0,    0,    0,    0,    0,   74,    0,    0, & ! 1   
     &      0,    0,   75,   76,    0,    0,    0,    0,    0,  129, & ! 2   
     &      0,    0,    0,    0,    0,    0,  119,    0,  139,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,  123,    0,    0,    0,  139, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,  109,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0/     !  9   
=======
     &      0,    0,    0,    0,    0,    0,    0,  137,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !2   
>>>>>>> master

      DATA ( IRR( IRXXN, 12 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,   83,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,  124,    0,    0,    0,    0, & ! 4   
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
<<<<<<< HEAD
     &      0,  124,    0,  125,  123,    0,    0,  119,  124,    0, & ! 8   
     &      0,    0,    0,    0,    0,  139,   49,   83,  139,  120, & ! 9   
     &      0,    0,    0,  124,  124,  128,  138,  110,    0,    0, & ! O   
     &    123,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  138, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,   97,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0/     !  9   
=======
     &      0,    0,    0,    0,    0,    0,    0,  138,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !2   
>>>>>>> master

      DATA ( IRR( IRXXN, 13 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,  111,    0,    0,    0,    0,    0, & ! 2   
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
     &      0,  140,    0,  119,  124,    0,    0,  124,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,  133,    0,  133,  134,  138, & ! 9   
     &      0,    0,    0,    0,    0,  116,    0,  124,    0,    0, & ! O   
     &    120,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
<<<<<<< HEAD
     &      0,    0,    0,    0,    0,    0,    0,   98,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0/     !  9   
=======
     &      0,    0,    0,    0,    0,    0,    0,  139,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !2   
>>>>>>> master

      DATA ( IRR( IRXXN, 14 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,  138,    0,    0,    0,    0,    0, & ! 2   
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
     &      0,    0,    0,  110,    0,    0,    0,  116,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,  139,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,  138,    0,  103,    0,    0, & ! O   
     &     72,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !2   

      DATA ( IRR( IRXXN, 15 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,    0,  116,    0,    0,    0,   91,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,  138,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,  123,    0,    0, & ! O   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !2   

      DATA ( IRR( IRXXN, 16 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,    0,   91,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,  117,    0,    0, & ! O   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !2   

      DATA ( IRR( IRXXN, 17 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,    0,  124,    0,    0,    0,    0,    0,    0, & ! 8   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !2   

      REAL( 8 )               :: SC( NRXNS,MXPRD )

      DATA ( SC( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.9000D-01, & ! +   
     &     6.0000D-01, 6.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 7.4000D-01, & ! +   
     &     2.0000D+00, 1.0000D+00, 5.0000D-01, 3.8000D-01, 7.6000D-01, & ! 2   
     &     1.0000D+00, 1.0000D+00, 4.0000D-01, 1.0000D+00, 2.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 2.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     2.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     7.3200D-01, 1.0000D+00, 1.2600D-01, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.1428D+00, 1.1428D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 8.5714D-01, 8.5714D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 5.0000D-01, 5.0000D-01, 1.5000D+00, 1.4286D+00, & ! 7   
     &     1.4286D+00, 1.7143D+00, 1.7143D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 4.1000D-01, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 4.1000D-01, 8.0000D-01, 2.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 9.0000D-01, & ! +   
     &     1.0000D+00, 6.8500D-01, 1.0000D+00, 1.0000D+00, 8.0000D-01, & ! 3   
     &     6.0000D-01, 1.0000D+00, 1.0000D+00, 8.0000D-01, 1.0000D+00, & ! +   
     &     5.0000D-01, 1.0000D+00, 8.0000D-01, 1.0000D+00, 6.0000D-01, & ! 4   
     &     5.4000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     5.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D-01, 1.0000D+00, 1.8000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 9.9100D-01, & ! +   
     &     1.0000D+00, 9.5000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 7.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     5.0000D-01, 2.0000D-01, 7.8100D-01, 2.9500D-01, 5.0000D-01, & ! 8   
     &     1.2400D+00, 1.3000D+00, 7.3200D-01, 5.0000D-01, 1.0000D+00, & ! +   
     &     7.5000D-01, 1.0000D-01, 8.8000D-01, 5.9800D-01, 5.9800D-01, & ! 9   
     &     6.0000D-01, 3.5000D-01, 2.2000D-02, 4.0000D-02, 7.1700D-01, & ! +   
     &     9.0400D-01, 1.0000D+00, 1.0000D+00, 2.7500D-01, 2.7500D-01, & ! O   
     &     2.2000D-01, 2.7500D-01, 6.3000D-01, 1.5000D-01, 7.5000D-01, & ! +   
     &     5.7000D-01, 4.7000D-01, 5.3000D-01, 9.1800D-01, 1.0000D+00, & ! 1   
     &     0.0000D+00, 1.0000D+00, 1.8000D-01, 8.6000D-01, 4.8000D-01, & ! +   
     &     0.0000D+00, 4.8000D-01, 1.5500D-01, 1.5500D-01, 8.6000D-01, & ! 2   
     &     0.0000D+00, 2.6000D-01, 2.6000D-01, 2.5000D-02, 3.0000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.2000D+00, 5.0000D-01, 6.0000D-01, 1.4000D+00, 1.0000D+00, & ! +   
     &     1.4000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 4.1000D-01, & ! 4   
     &     1.0000D+00, 8.0000D-01, 5.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 3.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 3.0000D-01, & ! +   
     &     1.5000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00/           !        +   

      DATA ( SC( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! O   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.9000D-01, & ! +   
     &     6.0000D-01, 6.0000D-01, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! 1   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 8.9000D-01, & ! +   
     &     2.0000D+00, 1.0000D+00, 5.0000D-01, 1.3800D+00, 3.4000D-01, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! 4   
     &     4.2000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     2.6800D-01, 1.0000D+00, 8.7400D-01, 1.0000D+00, 0.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! 9   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     0.0000D+00, 1.5000D-01, 0.0000D+00, 2.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.5000D-01, 8.0000D-01, 2.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D-01, & ! +   
     &     9.0000D-01, 3.1500D-01, 1.0000D+00, 0.0000D+00, 8.0000D-01, & ! 3   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D-01, 0.0000D+00, & ! +   
     &     5.0000D-01, 0.0000D+00, 8.0000D-01, 0.0000D+00, 6.0000D-01, & ! 4   
     &     6.0000D-02, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! 5   
     &     5.0000D-01, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 2.0000D-01, 1.0000D+00, 2.0000D-01, & ! 6   
     &     1.5000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 9.9100D-01, & ! +   
     &     1.0000D+00, 9.0000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 7.0000D-01, 1.0000D+00, 1.0000D+00, 5.1000D-01, & ! +   
     &     5.0000D-01, 3.0000D-01, 4.8800D-01, 5.5500D-01, 5.0000D-01, & ! 8   
     &     6.6000D-01, 7.0000D-01, 4.4200D-01, 5.0000D-01, 1.0000D+00, & ! +   
     &     5.0000D-01, 9.0000D-01, 1.2000D-01, 1.0000D+00, 1.0000D+00, & ! 9   
     &     6.5000D-01, 6.5000D-01, 5.2100D-01, 2.3100D-01, 1.4200D-01, & ! +   
     &     9.3300D-01, 1.0000D+00, 1.0000D+00, 2.7500D-01, 2.7500D-01, & ! O   
     &     2.2000D-01, 2.7500D-01, 3.7000D-01, 5.1200D+00, 5.0000D-01, & ! +   
     &     7.0000D-02, 2.8000D-01, 3.5200D-01, 8.2000D-02, 1.0000D+00, & ! 1   
     &     0.0000D+00, 1.0000D+00, 6.5000D-01, 1.4000D-01, 5.2000D-01, & ! +   
     &     0.0000D+00, 5.2000D-01, 5.4400D-01, 5.4400D-01, 1.4000D-01, & ! 2   
     &     0.0000D+00, 7.7000D-01, 7.7000D-01, 2.5000D-02, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.0000D-01, 5.0000D-01, 4.0000D-01, & ! 3   
     &     5.0000D-01, 5.0000D-01, 4.0000D-01, 2.4000D-01, 1.0000D+00, & ! +   
     &     2.0000D-01, 1.0000D+00, 5.0000D-01, 0.0000D+00, 1.5000D-01, & ! 4   
     &     1.0000D+00, 8.0000D-01, 5.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.4000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! 5   
     &     8.7000D-01, 9.9100D-01, 2.0000D+00, 3.3000D-01, 7.0000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
<<<<<<< HEAD
     &     1.0000D+00, 0.0000D+00, 8.8000D-01, 8.4000D-01, 8.4000D-01, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 4.7000D-01, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 1.0000D+00, 4.8570D-01, 3.0030D-01, 3.8560D-01, & ! +   
     &     2.1810D-01, 2.4120D-01, 6.6640D-01, 2.8580D-01, 3.3030D-01, & ! 9   
     &     3.4440D-01, 3.8860D-01/           !        +   

      DATA ( SC( IRXXN,  3 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.1000D-01, & ! +   
     &     4.0000D-01, 4.0000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.4000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 5.0000D-01, 1.3800D+00, 1.6000D-01, & ! 2   
     &     0.0000D+00, 1.0000D+00, 7.0000D-01, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     7.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.6800D-01, 0.0000D+00, 1.2600D-01, 0.0000D+00, 0.0000D+00, & ! 5   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     0.0000D+00, 1.5000D-01, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.5000D-01, 8.0000D-01, 2.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     9.0000D-01, 3.7000D-01, 0.0000D+00, 0.0000D+00, 2.0000D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 8.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.0000D-01, 0.0000D+00, 4.0000D-01, & ! 4   
     &     6.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! 5   
     &     2.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 8.0000D-01, 0.0000D+00, 2.0000D-01, & ! 6   
     &     5.0000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, 9.0000D-03, & ! +   
     &     0.0000D+00, 1.0000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 3.0000D-01, 1.0000D+00, 1.5600D+00, 1.6000D-01, & ! +   
     &     5.0000D-01, 1.0000D-01, 4.8800D-01, 2.7000D-01, 4.8000D-01, & ! 8   
     &     1.0000D-01, 1.0000D+00, 1.2800D-01, 4.8000D-01, 1.0000D+00, & ! +   
     &     2.5000D-01, 6.7300D-01, 1.2000D-01, 7.2800D-01, 7.2800D-01, & ! 9   
     &     1.5000D-01, 6.4000D-01, 1.1500D-01, 5.3100D-01, 1.4200D-01, & ! +   
     &     6.7000D-02, 0.0000D+00, 0.0000D+00, 2.7500D-01, 2.7500D-01, & ! O   
     &     2.2000D-01, 2.7500D-01, 1.0000D+00, 1.0000D+00, 2.5000D-01, & ! +   
     &     6.9000D-01, 7.5000D-01, 3.5200D-01, 9.1800D-01, 1.0000D+00, & ! 1   
     &     0.0000D+00, 1.0000D+00, 7.2000D-01, 4.1700D-01, 7.7000D-01, & ! +   
     &     0.0000D+00, 7.7000D-01, 6.0200D-01, 6.0200D-01, 2.2100D-01, & ! 2   
     &     0.0000D+00, 3.5000D-01, 3.5000D-01, 1.0000D+00, 4.8000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 2.0000D+00, & ! 3   
     &     6.0000D-01, 4.5000D-01, 4.0000D-01, 5.0000D-01, 0.0000D+00, & ! +   
     &     5.0000D-01, 0.0000D+00, 5.0000D-01, 0.0000D+00, 1.5000D-01, & ! 4   
     &     1.0000D+00, 1.8000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     1.3000D-01, 9.9100D-01, 1.0000D+00, 6.7000D-01, 4.5000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 0.0000D+00, 8.8000D-01, 8.4000D-01, 8.4000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
=======
     &     1.0000D+00, 1.0000D+00, 1.0000D-40, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 7   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 8   
     &     9.4882D-06, 1.2500D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! +   
     &     4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! 9   
     &     4.0000D-11, 4.0000D-11, 5.4000D-12, 5.5000D-16, 3.4000D-11, & ! +   
     &     1.0000D+00, 1.0000D+00, 8.2000D-11, 4.7000D-12, 1.4000D-12, & ! O   
     &     1.8000D-11, 1.0000D+00, 7.9000D-11, 1.4800D-11, 1.3400D-14, & ! +   
     &     1.7900D-13, 2.5100D-10, 2.0000D-11, 2.6100D-19, 1.1500D-15, & ! 1   
     &     1.0000D+00, 2.3700D-10, 2.0000D-11, 2.6100D-19, 1.1500D-15, & ! +   
     &     1.0000D+00, 2.3700D-10, 1.8000D-12, 6.1000D-11, 2.1100D-18, & ! 2   
     &     2.6000D-18, 8.5000D-19, 7.7000D-14, 2.2500D-33/           !+   

      DATA ( RTDAT( 2,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00,-2.6000D+00, 0.0000D+00,-1.6000D+00, 0.0000D+00, & ! O   
     &    -1.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.5700D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     2.6000D+00,-8.0000D-01, 0.0000D+00, 6.0000D+02, 2.8000D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
>>>>>>> master
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 6.2000D-03, 2.8620D-01, 9.5000D-02, & ! +   
     &     3.0630D-01, 2.0890D-01, 1.4300D-02, 3.9310D-01, 2.2720D-01, & ! 9   
     &     2.7490D-01, 2.4210D-01/           !        +   

      DATA ( SC( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.1000D-01, & ! +   
     &     4.0000D-01, 4.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.5000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.0000D-01, 6.2000D-01, 3.4000D-01, & ! 2   
     &     0.0000D+00, 1.0000D+00, 7.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     3.7000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 8.7400D-01, 0.0000D+00, 0.0000D+00, & ! 5   
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
     &     0.0000D+00, 4.4000D-01, 0.0000D+00, 0.0000D+00, 2.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 4.4000D-01, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, 8.0000D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 8.0000D-01, 0.0000D+00, 4.0000D-01, & ! 4   
     &     4.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     2.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! 6   
     &     5.0000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 3.0000D-01, 7.0000D-01, 2.2000D-01, 1.6000D-01, & ! +   
     &     5.0000D-01, 2.0000D-01, 9.7600D-01, 1.5000D-01, 4.8000D-01, & ! 8   
     &     1.0000D-01, 1.0000D+00, 2.4500D-01, 4.8000D-01, 0.0000D+00, & ! +   
     &     2.5000D-01, 9.0000D-01, 1.2000D-01, 7.2000D-02, 7.2000D-02, & ! 9   
     &     2.0000D-01, 3.3000D-01, 1.1500D-01, 1.7000D-01, 1.4200D-01, & ! +   
     &     6.7000D-02, 0.0000D+00, 0.0000D+00, 1.1250D+00, 1.2500D-01, & ! O   
     &     1.0000D-01, 1.2500D-01, 4.4400D-01, 0.0000D+00, 1.5000D+00, & ! +   
     &     1.8000D-01, 2.5000D-01, 1.1800D-01, 9.1800D-01, 1.0000D+00, & ! 1   
     &     0.0000D+00, 1.0000D+00, 1.0000D-01, 4.4300D-01, 2.3000D-01, & ! +   
     &     0.0000D+00, 2.3000D-01, 2.4400D-01, 2.4400D-01, 6.7500D-01, & ! 2   
     &     0.0000D+00, 6.5000D-01, 6.5000D-01, 2.0000D-01, 1.2000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D+00, & ! 3   
     &     1.0000D-01, 4.5000D-01, 4.0000D-01, 1.2000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 8.0000D-01, 0.0000D+00, 4.4000D-01, & ! 4   
     &     2.0000D+00, 2.0000D-01, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     1.1000D-01, 9.0000D-03, 1.0000D+00, 2.0000D+00, 5.5000D-01, & ! +   
     &     8.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 1.2000D-01, 1.6000D-01, 1.6000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 2.5000D-03, 4.1000D-03, 1.3730D-01, & ! +   
     &     1.5300D-02, 3.0000D-01, 1.2300D-02, 1.3900D-02, 2.6070D-01, & ! 9   
     &     4.9100D-02, 6.4000D-02/           !        +   

      DATA ( SC( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     4.0000D-01, 4.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.9000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, 2.0800D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 3.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     4.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
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
     &     0.0000D+00, 4.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.4000D-01, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     9.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 7.8000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 3.0000D-01, 7.0000D-01, 0.0000D+00, 3.7000D-01, & ! +   
     &     1.0000D+00, 2.0000D-01, 1.9500D-01, 1.5000D-01, 4.0000D-02, & ! 8   
     &     1.0000D-01, 0.0000D+00, 5.0000D-01, 4.0000D-02, 0.0000D+00, & ! +   
     &     2.5000D-01, 8.1800D-01, 1.2000D-01, 8.0000D-01, 7.2000D-02, & ! 9   
     &     3.5000D-01, 3.0000D-02, 2.6900D-01, 1.7000D-01, 1.4200D-01, & ! +   
     &     2.9000D-02, 0.0000D+00, 0.0000D+00, 8.2500D-01, 8.2500D-01, & ! O   
     &     6.6000D-01, 8.2500D-01, 1.8500D-01, 0.0000D+00, 2.8000D-01, & ! +   
     &     9.4000D-01, 1.2800D+00, 1.1800D-01, 9.1800D-01, 1.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 1.0000D-01, 6.6000D-01, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 2.4400D-01, 2.4400D-01, 3.0000D-01, & ! 2   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 7.3200D-01, 2.4000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     5.0000D-01, 1.0000D-01, 0.0000D+00, 8.0000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.0000D-01, 0.0000D+00, 4.4000D-01, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     6.0000D-02, 1.0000D+00, 0.0000D+00, 1.0000D+00, 3.0000D-01, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
<<<<<<< HEAD
     &     0.0000D+00, 0.0000D+00, 2.6000D-03, 3.5000D-03, 5.0000D-04, & ! +   
     &     1.0430D-01, 2.0280D-01, 1.2390D-01, 1.0270D-01, 7.0200D-02, & ! 9   
     &     2.5770D-01, 3.8500D-02/           !        +   
=======
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !+   
>>>>>>> master

      DATA ( SC( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 4.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.1000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 2.6000D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     9.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-1.2600D-01, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
<<<<<<< HEAD
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 4.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.4000D-01, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.1000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 3.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.1250D+00, 2.0000D-01, 2.4000D-02, 3.3400D-01, 1.0000D+00, & ! 8   
     &     1.0000D-01, 0.0000D+00, 3.0000D-01, 1.0000D+00, 0.0000D+00, & ! +   
     &     2.5000D-01, 8.2000D-02, 0.0000D+00, 2.0000D-01, 0.0000D+00, & ! 9   
     &     2.6600D-01, 1.0000D+00, 2.6900D-01, 5.4300D-01, 1.1300D-01, & ! +   
     &     2.9000D-02, 0.0000D+00, 0.0000D+00, 3.7500D-01, 3.7500D-01, & ! O   
     &     3.0000D-01, 3.7500D-01, 1.0400D-01, 0.0000D+00, 1.6600D+00, & ! +   
     &     2.4000D-01, 4.7000D-01, 5.3000D-01, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 7.0000D-02, 2.0000D-01, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 5.8000D-02, 5.8000D-02, 5.6000D-01, & ! 2   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D-02, 2.4000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     3.0000D-01, 1.0000D+00, 0.0000D+00, 2.0000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.4000D-01, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &    -1.1000D-01, 0.0000D+00, 0.0000D+00,-1.0000D+00, 3.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 2.3000D-03, 2.2390D-01, 2.0510D-01, & ! +   
     &     1.8930D-01, 4.7100D-02, 1.8310D-01, 2.0450D-01, 1.1160D-01, & ! 9   
     &     7.3900D-02, 2.6670D-01/           !        +   

      DATA ( SC( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.1000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00,-2.5000D+00, 0.0000D+00, 2.4000D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     9.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 4.4000D-01, 0.0000D+00, 0.0000D+00, & ! 2   
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
     &     0.0000D+00, 1.0000D-02, 1.1950D+00, 8.0000D-02, 5.0000D-01, & ! 8   
     &     0.0000D+00, 0.0000D+00, 3.0000D-01, 5.0000D-01, 0.0000D+00, & ! +   
     &     2.5000D-01, 8.2000D-02, 0.0000D+00, 8.7200D-01, 0.0000D+00, & ! 9   
     &     2.0000D-01, 3.5000D-01, 4.5700D-01, 4.6100D-01, 1.1300D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 7.4000D-02, 1.0000D+00, & ! O   
     &     2.0000D-01, 2.5100D-01, 5.9200D-01, 0.0000D+00, 4.7000D-01, & ! +   
     &     1.0000D-03, 5.3000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 1.8000D-01, 8.6000D-01, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.5500D-01, 1.5500D-01, 8.6000D-01, & ! 2   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 2.0000D-02, 4.8000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     3.0000D-01, 2.5000D-01, 0.0000D+00, 1.9800D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.4000D-01, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     7.6000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.7000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 2.9440D-01, 1.8200D-01, 1.7640D-01, & ! +   
     &     1.6680D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00/           !        +   

      DATA ( SC( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.1000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.4000D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     2.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 2.1000D-01,-7.3000D-01, 3.7800D-01, 2.5000D-01, & ! 8   
     &     0.0000D+00, 0.0000D+00, 2.4000D-01, 6.2500D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     2.0000D-01, 3.5000D-01, 1.1700D-01, 1.5000D-01, 7.1700D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5100D-01, 2.5100D-01, & ! O   
     &     1.7400D+00, 2.1750D+00, 3.3100D-01, 0.0000D+00, 1.0000D+00, & ! +   
     &     7.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 2.5000D-01, 0.0000D+00, 5.6000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     5.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 2.0210D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00/           !        +   

      DATA ( SC( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.7000D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &    -2.7000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 2.0000D-01, 0.0000D+00, 7.5000D-02, 3.7500D-01, & ! 8   
     &     0.0000D+00, 0.0000D+00, 6.0000D-02, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     6.6000D-02, 1.0000D+00, 1.3700D-01, 3.9800D-01, 7.1700D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.1750D+00, 2.1750D+00, & ! O   
     &     8.0000D-01, 1.0000D+00, 1.8500D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.1000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.0000D-01, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 1.9000D-03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00/           !        +   

      DATA ( SC( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.2800D-01, & ! 2   
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
     &     0.0000D+00, 1.0000D-01, 0.0000D+00, 7.5000D-02,-1.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 2.9000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     6.6000D-02, 0.0000D+00, 1.3700D-01, 1.4300D-01, 2.8400D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     2.0000D-01, 0.0000D+00, 2.7000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.9000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
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
     &     0.0000D+00, 0.0000D+00, 2.3000D-03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00/           !        +   

      DATA ( SC( IRXXN, 11 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 8.4000D-01, & ! 2   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 9.0000D-02, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 8.0000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 1.3700D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     8.0000D-01, 0.0000D+00, 9.8000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
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
     &     0.0000D+00, 0.0000D+00/           !        +   

      DATA ( SC( IRXXN, 12 ), IRXXN = 1, NRXNS ) / & 
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.3000D-01, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 8.0000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 6.5800D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 7.8000D-02, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00/           !        +   

      DATA ( SC( IRXXN, 13 ), IRXXN = 1, NRXNS ) / & 
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.0000D-02, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 2.6600D-01, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00/           !        +   

      DATA ( SC( IRXXN, 14 ), IRXXN = 1, NRXNS ) / & 
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-7.9000D-01, 0.0000D+00, & ! 8   
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
     &     0.0000D+00, 0.0000D+00/           !        +   
=======
     &     0.0000D+00, 0.0000D+00, 1.3500D+02, 0.0000D+00,-1.6000D+03, & ! +   
     &     0.0000D+00, 0.0000D+00,-3.4000D+01, 3.4500D+02,-1.8600D+03, & ! O   
     &    -1.1000D+03, 0.0000D+00, 0.0000D+00, 4.4800D+02,-2.2830D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.4000D+02, 0.0000D+00,-1.2565D+03, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 6.8000D+02/           !+   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &      6,   17,   19,   20,   36,   37,   40,   45,   46,   48, & 
     &     49,   52,   54,   55,  123,  136,  138,  142,  217,  219, & 
     &    268/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     2.3000D-11, 2.6000D-11, 9.8000D+02, 3.1800D+03, 1.9000D-12, & 
     &     9.7000D+14, 3.3000D-11, 2.8000D-11, 2.1990D+03, 4.7000D-12, & 
     &     4.8000D+15, 1.3000D-12, 1.2000D-11, 5.4000D+16, 0.0000D+00, & 
     &     1.0000D-12, 9.0000D-12, 3.0000D-11, 4.3000D-01, 4.3000D-01, & 
     &     7.8426D+01/

      DATA ( RFDAT( 2,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     2.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D-01, & 
     &     1.0000D-01,-3.0000D-01, 0.0000D+00, 6.5000D-34, 0.0000D+00, & 
     &     0.0000D+00,-7.0000D-01,-9.0000D-01, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00,-8.5000D-01,-1.0000D+00,-8.0000D+00,-8.0000D+00, & 
     &     5.8212D+00/

      DATA ( RFDAT( 3,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &    -1.1080D+04, 0.0000D+00, 0.0000D+00, 1.3350D+03, 0.0000D+00, & 
     &    -1.1170D+04, 0.0000D+00, 0.0000D+00,-1.3830D+04, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00/

      DATA ( RFDAT( 4,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     6.0000D-01, 5.0000D-01, 0.0000D+00, 0.0000D+00, 3.5000D-01, & 
     &     3.5000D-01, 8.1000D-01, 6.0000D-01, 0.0000D+00, 6.0000D-01, & 
     &     6.0000D-01, 5.3000D-01, 3.0000D-01, 3.0000D-01, 0.0000D+00, & 
     &     3.7000D-01, 4.8000D-01, 5.0000D-01, 4.1000D-01, 4.1000D-01, & 
     &     0.0000D+00/

      DATA ( RFDAT( 5,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     1.0000D+00, 1.1300D+00, 0.0000D+00, 0.0000D+00, 1.3300D+00, & 
     &     1.3300D+00, 8.7000D-01, 1.0000D+00, 0.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 1.1000D+00, 1.4100D+00, 1.4100D+00, 0.0000D+00, & 
     &     1.3000D+00, 1.1500D+00, 1.1300D+00, 1.0000D+00, 1.0000D+00, & 
     &     0.0000D+00/

      REAL               :: SC( NRXNS,MXPRD )

      DATA ( SC( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000,    2.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    2.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    2.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.59000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        0.60000,    0.41000,    1.00000,    2.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.60000,    0.41000, & ! 6   
     &        0.80000,    2.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.90000,    1.00000,    0.68500,    1.00000, & ! 7   
     &        1.00000,    0.80000,    0.60000,    1.00000,    1.00000, & ! +   
     &        0.80000,    1.00000,    0.50000,    1.00000,    0.80000, & ! 8   
     &        1.00000,    0.60000,    1.00000,    0.54000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    2.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.50000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.20000,    0.74000,    1.00000, & ! 1   
     &        1.80000,    2.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.99100, & ! 2   
     &        1.00000,    0.95000,    0.50000,    0.38000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.20000,    1.00000,    1.00000, & ! 3   
     &        0.70000,    1.00000,    1.00000,    1.00000,    0.50000, & ! +   
     &        0.20000,    0.78100,    0.29500,    0.50000,    1.24000, & ! 4   
     &        1.30000,    0.73200,    0.50000,    1.00000,    0.75000, & ! +   
     &        0.10000,    0.88000,    0.59800,    0.59800,    1.00000, & ! 5   
     &        0.60000,    0.35000,    0.02200,    0.04000,    0.71700, & ! +   
     &        0.76000,    0.90400,    1.00000,    1.00000,    1.00000, & ! 6   
     &        0.27500,    0.27500,    0.22000,    0.27500,    0.63000, & ! +   
     &        0.15000,    0.75000,    0.57000,    0.47000,    0.53000, & ! 7   
     &        0.91800,    1.00000,    0.00000,    1.00000,    0.18000, & ! +   
     &        0.86000,    0.48000,    0.00000,    0.48000,    0.15500, & ! 8   
     &        0.15500,    0.86000,    0.00000,    0.26000,    0.26000, & ! +   
     &        0.02500,    0.30000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    0.40000,    1.00000,    1.20000, & ! +   
     &        0.50000,    1.00000,    0.60000,    1.40000,    1.00000, & ! O   
     &        0.14000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.41000,    1.00000,    0.80000,    0.50000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    0.73200,    1.00000,    0.12600, & ! +   
     &        2.00000,    1.00000,    1.00000,    0.30000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.30000,    0.15000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    0.50000,    0.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.85710,    1.14290,    0.85710,    1.14290, & ! 7   
     &        0.71430,    0.71430,    0.80000,    0.90000,    0.50000, & ! +   
     &        0.50000,    1.50000,    1.42860,    1.42860,    1.71430, & ! 8   
     &        1.71430,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    1.00000,    0.50000, & ! 2   
     &        1.00000,    1.00000,    0.50000,    0.50000/           ! &  

      DATA ( SC( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &        1.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 2   
     &        0.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.59000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! 5   
     &        0.60000,    0.15000,    0.00000,    2.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.60000,    0.15000, & ! 6   
     &        0.80000,    2.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.10000,    0.90000,    0.31500,    1.00000, & ! 7   
     &        0.00000,    0.80000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.20000,    0.00000,    0.50000,    0.00000,    0.80000, & ! 8   
     &        0.00000,    0.60000,    1.00000,    0.06000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 9   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.50000,    1.00000, & ! O   
     &        0.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.20000,    0.89000,    1.00000, & ! 1   
     &        0.20000,    2.00000,    1.50000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    1.00000,    0.99100, & ! 2   
     &        1.00000,    0.90000,    0.50000,    1.38000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.42000,    1.00000,    0.00000, & ! 3   
     &        0.70000,    1.00000,    1.00000,    0.51000,    0.50000, & ! +   
     &        0.30000,    0.48800,    0.55500,    0.50000,    0.66000, & ! 4   
     &        0.70000,    0.44200,    0.50000,    1.00000,    0.50000, & ! +   
     &        0.90000,    0.12000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        0.65000,    0.65000,    0.52100,    0.23100,    0.14200, & ! +   
     &        0.34000,    0.93300,    1.00000,    1.00000,    1.00000, & ! 6   
     &        0.27500,    0.27500,    0.22000,    0.27500,    0.37000, & ! +   
     &        5.12000,    0.50000,    0.07000,    0.28000,    0.35200, & ! 7   
     &        0.08200,    1.00000,    0.00000,    1.00000,    0.65000, & ! +   
     &        0.14000,    0.52000,    0.00000,    0.52000,    0.54400, & ! 8   
     &        0.54400,    0.14000,    0.00000,    0.77000,    0.77000, & ! +   
     &        0.02500,    1.00000,    0.00000,    0.00000,    0.50000, & ! 9   
     &        0.50000,    1.00000,    1.00000,    0.40000,    0.50000, & ! +   
     &        0.50000,    1.00000,    0.40000,    0.24000,    1.00000, & ! O   
     &        0.20000,    1.00000,    0.50000,    0.00000,    1.00000, & ! +   
     &        0.15000,    1.00000,    0.80000,    0.50000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    0.26800,    1.00000,    0.87400, & ! +   
     &        0.00000,    1.00000,    0.00000,    1.40000,    1.00000, & ! 2   
     &        0.00000,    1.00000,    1.00000,    1.00000,    0.87000, & ! +   
     &        0.99100,    2.00000,    0.33000,    0.70000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.88000,    0.84000,    0.84000,    0.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    0.47000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.50000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    1.00000,    0.48570,    0.30030,    0.38560, & ! +   
     &        0.21810,    0.24120,    0.66640,    0.28580,    0.33030, & ! 9   
     &        0.34440,    0.38860,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.58000,    0.52000, & ! +   
     &        0.04500,    0.58000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.50000, & ! 2   
     &        1.00000,    1.00000,    0.50000,    0.50000/           ! &  

      DATA ( SC( IRXXN,  3 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.41000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 5   
     &        0.40000,    0.15000,    0.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.40000,    0.15000, & ! 6   
     &        0.80000,    2.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.90000,    0.37000,    0.00000, & ! 7   
     &        0.00000,    0.20000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.80000,    0.00000,    0.00000,    0.00000,    0.20000, & ! 8   
     &        0.00000,    0.40000,    1.00000,    0.60000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.20000,    0.00000, & ! O   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.80000,    1.40000,    0.00000, & ! 1   
     &        0.20000,    0.00000,    0.50000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00900, & ! 2   
     &        0.00000,    0.10000,    0.50000,    1.38000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.74000,    0.00000,    0.00000, & ! 3   
     &        0.30000,    1.00000,    1.56000,    0.16000,    0.50000, & ! +   
     &        0.10000,    0.48800,    0.27000,    0.48000,    0.10000, & ! 4   
     &        1.00000,    0.12800,    0.48000,    1.00000,    0.25000, & ! +   
     &        0.67300,    0.12000,    0.72800,    0.72800,    0.00000, & ! 5   
     &        0.15000,    0.64000,    0.11500,    0.53100,    0.14200, & ! +   
     &        0.16000,    0.06700,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.27500,    0.27500,    0.22000,    0.27500,    1.00000, & ! +   
     &        1.00000,    0.25000,    0.69000,    0.75000,    0.35200, & ! 7   
     &        0.91800,    1.00000,    0.00000,    1.00000,    0.72000, & ! +   
     &        0.41700,    0.77000,    0.00000,    0.77000,    0.60200, & ! 8   
     &        0.60200,    0.22100,    0.00000,    0.35000,    0.35000, & ! +   
     &        1.00000,    0.48000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        1.00000,    1.00000,    0.70000,    2.00000,    0.60000, & ! +   
     &        0.45000,    1.00000,    0.40000,    0.50000,    0.00000, & ! O   
     &        0.50000,    0.00000,    0.50000,    0.00000,    0.00000, & ! +   
     &        0.15000,    1.00000,    1.80000,    1.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.26800,    0.00000,    0.12600, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.13000, & ! +   
     &        0.99100,    1.00000,    0.67000,    0.45000,    1.00000, & ! 3   
     &        1.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.88000,    0.84000,    0.84000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00620,    0.28620,    0.09500, & ! +   
     &        0.30630,    0.20890,    0.01430,    0.39310,    0.22720, & ! 9   
     &        0.27490,    0.24210,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 2   
     &        0.00000,    0.00000,    1.00000,    1.00000/           ! &  

      DATA ( SC( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.41000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.40000,    0.44000,    0.00000,    0.00000,    2.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.40000,    0.44000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.10000,    1.00000,    0.00000, & ! 7   
     &        0.00000,    0.80000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.80000, & ! 8   
     &        0.00000,    0.40000,    0.00000,    0.40000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.20000,    0.00000, & ! O   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.15000,    0.00000, & ! 1   
     &        1.00000,    0.00000,    0.50000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 2   
     &        0.00000,    0.10000,    0.50000,    0.62000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.37000,    0.00000,    0.00000, & ! 3   
     &        0.30000,    0.70000,    0.22000,    0.16000,    0.50000, & ! +   
     &        0.20000,    0.97600,    0.15000,    0.48000,    0.10000, & ! 4   
     &        1.00000,    0.24500,    0.48000,    0.00000,    0.25000, & ! +   
     &        0.90000,    0.12000,    0.07200,    0.07200,    0.00000, & ! 5   
     &        0.20000,    0.33000,    0.11500,    0.17000,    0.14200, & ! +   
     &        0.34000,    0.06700,    0.00000,    0.00000,    0.00000, & ! 6   
     &        1.12500,    0.12500,    0.10000,    0.12500,    0.44400, & ! +   
     &        0.00000,    1.50000,    0.18000,    0.25000,    0.11800, & ! 7   
     &        0.91800,    1.00000,    0.00000,    1.00000,    0.10000, & ! +   
     &        0.44300,    0.23000,    0.00000,    0.23000,    0.24400, & ! 8   
     &        0.24400,    0.67500,    0.00000,    0.65000,    0.65000, & ! +   
     &        0.20000,    0.12000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    1.00000,    0.70000,    2.00000,    0.10000, & ! +   
     &        0.45000,    0.00000,    0.40000,    0.12000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.80000,    0.00000,    0.00000, & ! +   
     &        0.44000,    2.00000,    0.20000,    0.50000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.87400, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.11000, & ! +   
     &        0.00900,    1.00000,    2.00000,    0.55000,    0.85000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.12000,    0.16000,    0.16000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00250,    0.00410,    0.13730, & ! +   
     &        0.01530,    0.30000,    0.01230,    0.01390,    0.26070, & ! 9   
     &        0.04910,    0.06400,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    1.00000/           ! &  

      DATA ( SC( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.40000,    0.44000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.40000,    0.44000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.90000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.19000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.07800,    0.50000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.04000,    0.00000,    0.00000, & ! 3   
     &        0.30000,    0.70000,    0.00000,    0.37000,    1.00000, & ! +   
     &        0.20000,    0.19500,    0.15000,    0.04000,    0.10000, & ! 4   
     &        0.00000,    0.50000,    0.04000,    0.00000,    0.25000, & ! +   
     &        0.81800,    0.12000,    0.80000,    1.07200,    0.00000, & ! 5   
     &        0.35000,    0.03000,    0.26900,    0.17000,    0.14200, & ! +   
     &        0.20800,    0.02900,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.82500,    0.82500,    0.66000,    0.82500,    0.18500, & ! +   
     &        0.00000,    0.28000,    0.94000,    1.28000,    0.11800, & ! 7   
     &        0.91800,    1.00000,    0.00000,    0.00000,    0.10000, & ! +   
     &        0.66000,    1.00000,    0.00000,    1.00000,    0.24400, & ! 8   
     &        0.24400,    0.30000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.73200,    0.24000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.30000,    0.00000,    0.50000, & ! +   
     &        0.10000,    0.00000,    0.00000,    0.08000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.20000,    0.00000,    0.00000, & ! +   
     &        0.44000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.06000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.30000,    1.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00260,    0.00350,    0.00050, & ! +   
     &        0.10430,    0.20280,    0.12390,    0.10270,    0.07020, & ! 9   
     &        0.25770,    0.03850,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.44000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.40000,    0.44000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.11000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.01100,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.94000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.30000,    0.00000,    0.00000,    1.12500, & ! +   
     &        0.20000,    0.02400,    0.33400,    1.00000,    0.10000, & ! 4   
     &        0.00000,    0.30000,    1.00000,    0.00000,    0.25000, & ! +   
     &        0.08200,    0.00000,    0.20000,    0.00000,    0.00000, & ! 5   
     &        0.26600,    1.00000,    0.26900,    0.54300,    0.11300, & ! +   
     &        0.26000,    0.02900,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.37500,    0.37500,    0.30000,    0.37500,    0.10400, & ! +   
     &        0.00000,    1.66000,    0.24000,    0.47000,    0.53000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.07000, & ! +   
     &        0.20000,    1.00000,    0.00000,    1.00000,    0.05800, & ! 8   
     &        0.05800,    0.56000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.02000,    0.24000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.30000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.02000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.44000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,   -0.12600, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,   -0.11000, & ! +   
     &        0.00000,    0.00000,   -1.00000,    0.30000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00230,    0.22390,    0.20510, & ! +   
     &        0.18930,    0.04710,    0.18310,    0.20450,    0.11160, & ! 9   
     &        0.07390,    0.26670,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.44000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.11000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,   -2.50000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.98000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.01000,    1.19500,    0.08000,    0.50000,    0.00000, & ! 4   
     &        0.00000,    0.30000,    0.50000,    0.00000,    0.25000, & ! +   
     &        0.08200,    0.00000,    0.87200,    0.00000,    0.00000, & ! 5   
     &        0.20000,    0.35000,    0.45700,    0.46100,    0.11300, & ! +   
     &        0.24000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.07400,    1.00000,    0.20000,    0.25100,    0.59200, & ! +   
     &        0.00000,    0.47000,    0.00100,    0.53000,    1.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.18000, & ! +   
     &        0.86000,    1.00000,    0.00000,    0.00000,    0.15500, & ! 8   
     &        0.15500,    0.86000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.02000,    0.48000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.30000, & ! +   
     &        0.25000,    0.00000,    0.00000,    1.98000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.44000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.76000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.70000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.29440,    0.18200,    0.17640, & ! +   
     &        0.16680,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.11000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.02000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.21000,   -0.73000,    0.37800,    0.25000,    0.00000, & ! 4   
     &        0.00000,    0.24000,    0.62500,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.20000,    0.35000,    0.11700,    0.15000,    0.71700, & ! +   
     &        0.24000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.25100,    0.25100,    1.74000,    2.17500,    0.33100, & ! +   
     &        0.00000,    1.00000,    7.00000,    1.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 8   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.10000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.25000,    0.00000,    0.00000,    0.56000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.05000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.20210,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,   -2.70000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.20000,    0.00000,    0.07500,    0.37500,    0.00000, & ! 4   
     &        0.00000,    0.06000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.06600,    1.00000,    0.13700,    0.39800,    0.71700, & ! +   
     &        0.17000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        2.17500,    2.17500,    0.80000,    1.00000,    0.18500, & ! +   
     &        0.00000,    0.00000,    0.21000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.70000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00190,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.10000,    0.00000,    0.07500,   -1.00000,    0.00000, & ! 4   
     &        0.00000,    0.29000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.06600,    0.00000,    0.13700,    0.14300,    0.28400, & ! +   
     &        0.12800,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.20000,    0.00000,    2.70000, & ! +   
     &        0.00000,    0.00000,    0.39000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00230,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN, 11 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.09000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.08000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.13700,    0.00000,    0.00000, & ! +   
     &        0.84000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.80000,    0.00000,    0.09800, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN, 12 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.13000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.08000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.65800,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.07800, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN, 13 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.04000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.26600, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN, 14 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,   -0.79000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  
>>>>>>> master

      INTEGER            :: NREACT( NRXNS )

      DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    2,    2, & ! 7   
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
<<<<<<< HEAD
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2/     !  9   
=======
     &      2,    2,    2,    2,    2,    1,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    1, & ! 5   
     &      1,    1,    1,    2,    2,    2,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      1,    1,    2,    2,    2,    2,    1,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    1,    2,    2,    2,    2, & ! 1   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2/     !2   
>>>>>>> master
      INTEGER            :: NPRDCT( NRXNS )

      DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    1,    1,    1,    2,    1,    2,    2,    2,    4, & ! O   
     &      5,    6,    3,    2,    1,    2,    1,    4,    5,    8, & ! 1   
     &      2,    3,    7,    4,   11,    2,    4,    5,    3,    1, & ! 2   
     &      2,    3,    2,    0,    1,    1,    1,    2,    1,    2, & ! 3   
     &      2,    2,    2,    1,    2,    9,    2,    2,    2,    2, & ! 4   
     &      5,    2,    6,    2,    1,    2,    2,    1,    1,    2, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
<<<<<<< HEAD
     &      1,    1,    0,    1,    1,    1,    1,    1,    1,    0, & ! 8   
     &      1,    1,    1,    2,    1,    2,    1,    1,    2,    1, & ! 9   
     &      2,    2,    1,    1,    1,    1,    1,    2,    1,    1, & ! O   
     &      1,    1,    1,    3,    3,    1,    6,    1,    2,    4, & ! 1   
     &      4,    1,    7,    3,    3,    1,    1,    0,    3,    2, & ! 2   
     &      5,    4,    2,    1,    4,    2,    1,    1,    3,    1, & ! 3   
     &      2,    1,    4,    1,    4,    4,    1,    1,    2,    1, & ! 4   
     &      2,    3,    3,    1,    3,    4,    2,    1,    2,    2, & ! 5   
     &      1,    2,    3,    2,    4,    5,    4,    2,    1,    4, & ! 6   
     &      2,    6,    4,    1,    1,    1,    5,    6,    4,    5, & ! 7   
     &      6,   10,    8,   14,   10,    6,    4,   12,    9,    3, & ! 8   
     &      7,    7,    5,    7,    5,   10,    9,   12,   10,   10, & ! 9   
     &      6,    2,    2,    9,    9,   11,    9,   13,    3,    8, & ! O   
     &     11,    8,    7,    5,    5,    0,    4,    8,    7,    7, & ! 1   
     &      0,    6,    8,    8,    7,    0,    7,    6,    7,    9, & ! 2   
     &      1,    1,    2,    3,    4,    7,    8,    4,    8,    2, & ! 3   
     &      3,    2,    5,    1,    7,    4,    4,    4,    2,    2, & ! 4   
     &      1,    2,    2,    1,    2,    8,    5,    4,    6,    8, & ! 5   
     &      5,    3,    2,    2,    3,    3,    1,    4,    4,    4, & ! 6   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    1,    1,    1,    2,   10,    7,    7, & ! 8   
     &      7,    6,    6,    6,    6,    6,    6/     !  9   

=======
     &      1,    1,    1,    1,    1,    1,    2,   10,    7,    7, & ! 8   
     &      7,    6,    6,    6,    6,    6,    6,    1,    1,    1, & ! 9   
     &      0,    0,    1,    1,    1,    1,    0,    1,    2,    2, & ! O   
     &      2,    2,    1,    1,    1,    0,    1,    1,    1,    1, & ! 1   
     &      0,    1,    1,    1,    3,    2,    2,    3,    3/     !2   

      INTEGER, PARAMETER :: NMPHOT =  38
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    8,    9,   21,   27,   28,   38,   43,   47,   50, & 
     &     56,   64,   88,   90,   92,   97,   98,  108,  112,  114, & 
     &    117,  119,  128,  129,  161,  163,  197,  198,  202,  221, & 
     &    222,  228,  246,  301,  302,  307,  316,  321/

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   11,   12,   12,   13,   14,   15,   16,   17,   18, & 
     &     19,   20,   21,   22,   23,   24,   13,    1,    1,   25, & 
     &     26,   27,   28,   14,   15,   16,   29,   29/

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33,   34,   35,   36,   37,   38/

>>>>>>> master
      INTEGER, PARAMETER :: MHETERO =  12
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &     55,   56,   57,   58,   59,   60,   61,   62,   63,  284, & 
     &    285,  286/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2,    3,    4,    5,    8,    9,   10,   11,    6, & 
     &      6,    7/

      INTEGER, PARAMETER :: NPHOTAB =  29
      CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

      DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
     &   'NO2_IUPAC10     ', 'O3_O3P_IUPAC10  ', 'O3_O1D_IUPAC10  ', & 
     &   'H2O2_IUPAC10    ', 'NO3NO2_06       ', 'NO3NO_06        ', & 
     &   'N2O5_IUPAC10    ', 'HONO_IUPAC10    ', 'HNO3_IUPAC10    ', & 
     &   'PNA_IUPAC10     ', 'PAN_IUPAC10     ', 'MEPX_IUPAC10    ', & 
     &   'NTR_IUPAC10     ', 'FORM_R_IUPAC10  ', 'FORM_M_IUPAC10  ', & 
     &   'ALD2_R_IUPAC10  ', 'ALDX_R_IUPAC10  ', 'GLYD_IUPAC10    ', & 
     &   'GLY_R_IUPAC10   ', 'MGLY_IUPAC10    ', 'KET_IUPAC10     ', & 
     &   'ACET_IUPAC10    ', 'ISPD            ', 'HPALD           ', & 
     &   'CL2_IUPAC04     ', 'HOCL_IUPAC04    ', 'FMCL_IUPAC04    ', & 
     &   'CLNO2           ', 'ACRO_09         '/

      INTEGER, PARAMETER :: NHETERO =  11
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_NTR2     ', 'HETERO_N2O5IJ   ', 'HETERO_N2O5K    ', &
     &   'HETERO_H2NO3PAIJ', 'HETERO_H2NO3PAK ', 'HETERO_H2NO3PBIJ', &
     &   'HETERO_H2NO3PBK ', 'HETERO_NO2      ', 'HETERO_IEPOX    ', &
     &   'HETERO_GLY      ', 'HETERO_MGLY     '/

      CHARACTER( 16 )    :: RXLABEL( NRXNS )

      DATA ( RXLABEL( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &    'R1              ', 'R8              ', 'R9              ', & ! 0   
     &    'R21             ', 'R27             ', 'R28             ', & ! 1   
     &    'R38             ', 'R43             ', 'R47             ', & ! 2   
     &    'R50             ', 'R56             ', 'R64             ', & ! 3   
     &    'R88             ', 'R90             ', 'R92             ', & ! 4   
     &    'R97             ', 'R98             ', 'R108            ', & ! 5   
     &    'R112            ', 'R114            ', 'R117            ', & ! 6   
     &    'R119            ', 'R128            ', 'R129            ', & ! 7   
     &    'R161            ', 'R163            ', 'R196            ', & ! 8   
     &    'R197            ', 'R201            ', 'CL1             ', & ! 9   
     &    'CL2             ', 'CL8             ', 'CL25            ', & ! 0   
     &    'HAL_Ozone       ', 'R2              ', 'R10             ', & ! 1   
     &    'R11             ', 'R37             ', 'R39             ', & ! 2   
     &    'R49             ', 'R55             ', 'R63             ', & ! 3   
     &    'R102            ', 'R122            ', 'R124            ', & ! 4   
     &    'R133            ', 'R134            ', 'R155            ', & ! 5   
     &    'R209            ', 'R217            ', 'R218            ', & ! 6   
     &    'R219            ', 'R220            ', 'CL9             ', & ! 7   
     &    'HET_NTR2        ', 'HET_N2O5IJ      ', 'HET_N2O5K       ', & ! 8   
     &    'HET_H2NO3PIJA   ', 'HET_H2NO3PKA    ', 'HET_N02         ', & ! 9   
     &    'HET_IEPOX       ', 'HET_GLY         ', 'HET_MGLY        ', & ! 0   
     &    'OLIG_XYLENE1    ', 'OLIG_XYLENE2    ', 'OLIG_TOLUENE1   ', & ! 1   
     &    'OLIG_TOLUENE2   ', 'OLIG_BENZENE1   ', 'OLIG_BENZENE2   ', & ! 2   
     &    'OLIG_TERPENE1   ', 'OLIG_TERPENE2   ', 'OLIG_ISOPRENE1  ', & ! 3   
     &    'OLIG_ISOPRENE2  ', 'OLIG_SESQT1     ', 'OLIG_PAH1       ', & ! 4   
     &    'OLIG_PAH2       ', 'OLIG_ALK1       ', 'OLIG_ALK2       ', & ! 5   
     &    'R3              ', 'R4              ', 'R5              ', & ! 6   
     &    'R6              ', 'R7              ', 'R12             ', & ! 7   
     &    'R13             ', 'R14             ', 'R15             ', & ! 8   
     &    'R16             ', 'R17             ', 'R18             ', & ! 9   
     &    'R19             ', 'R20             ', 'R22             ', & ! 0   
     &    'R23             ', 'R24             ', 'R25             ', & ! 1   
     &    'R26             ', 'R29             ', 'R30             ', & ! 2   
     &    'R31             ', 'R32             ', 'R33             ', & ! 3   
     &    'R34             ', 'R35             ', 'R36             ', & ! 4   
     &    'R40             ', 'R41             ', 'R42             ', & ! 5   
     &    'R44             ', 'R45             ', 'R46             ', & ! 6   
     &    'R48             ', 'R51             ', 'R52             ', & ! 7   
     &    'R53             ', 'R54             ', 'R57             ', & ! 8   
     &    'R58             ', 'R59             ', 'R60             ', & ! 9   
     &    'R61             ', 'R62             ', 'R65             ', & ! 0   
     &    'R66             ', 'R67             ', 'R68             ', & ! 1   
     &    'R69             ', 'R70             ', 'R71             ', & ! 2   
     &    'R72             ', 'R73             ', 'R74             ', & ! 3   
     &    'R75             ', 'R76             ', 'R77             ', & ! 4   
     &    'R78             ', 'R79             ', 'R80             ', & ! 5   
     &    'R81             ', 'R82             ', 'R83             ', & ! 6   
     &    'R84             ', 'R85             ', 'R86             ', & ! 7   
     &    'R87             ', 'R89             ', 'R91             ', & ! 8   
     &    'R93             ', 'R94             ', 'R95             ', & ! 9   
     &    'R96             ', 'R99             ', 'R100            ', & ! 0   
     &    'R101            ', 'R103            ', 'R104            ', & ! 1   
     &    'R105            ', 'R106            ', 'R107            ', & ! 2   
     &    'R109            ', 'R110            ', 'R111            ', & ! 3   
     &    'R113            ', 'R115            ', 'R116            ', & ! 4   
     &    'R118            ', 'R120            ', 'R121            ', & ! 5   
     &    'R123            ', 'R125            ', 'R126            ', & ! 6   
     &    'R127            ', 'R130            ', 'R131            ', & ! 7   
     &    'R132            ', 'R135            ', 'R136            ', & ! 8   
     &    'R137            ', 'R138            ', 'R139            ', & ! 9   
     &    'R140            ', 'R141            ', 'R142            ', & ! 0   
     &    'R143            ', 'R144            ', 'R145            ', & ! 1   
     &    'R146            ', 'R147            ', 'R148            ', & ! 2   
     &    'R149            ', 'R150            ', 'R151            ', & ! 3   
     &    'R152            ', 'R153            ', 'R154            ', & ! 4   
     &    'R156            ', 'R157            ', 'R158            ', & ! 5   
     &    'R159            ', 'R160            ', 'R162            ', & ! 6   
     &    'R164            ', 'R165            ', 'R166            ', & ! 7   
     &    'R167            ', 'R168            ', 'R169            ', & ! 8   
     &    'R170            ', 'R171            ', 'R172            ', & ! 9   
     &    'R173            ', 'R174            ', 'R175            ', & ! 0   
     &    'R176            ', 'R177            ', 'R178            ', & ! 1   
     &    'R179            ', 'R180            ', 'R181            ', & ! 2   
     &    'R182            ', 'R183            ', 'R184            ', & ! 3   
     &    'R185            ', 'R185a           ', 'R186            ', & ! 4   
     &    'R187            ', 'R188            ', 'R189            ', & ! 5   
     &    'R190            ', 'R191            ', 'R192            ', & ! 6   
     &    'R193            ', 'R194            ', 'R195            ', & ! 7   
     &    'R198            ', 'R199            ', 'R200            ', & ! 8   
     &    'R202            ', 'R203            ', 'R204            ', & ! 9   
     &    'R205            ', 'R206            ', 'R207            ', & ! 0   
     &    'R208            ', 'R210            ', 'R211            ', & ! 1   
     &    'R212            ', 'R213            ', 'R214            ', & ! 2   
     &    'R216            ', 'CL3             ', 'CL4             ', & ! 3   
     &    'CL5             ', 'CL6             ', 'CL7             ', & ! 4   
     &    'CL10            ', 'CL11            ', 'CL12            ', & ! 5   
     &    'CL13            ', 'CL14            ', 'CL15            ', & ! 6   
     &    'CL16            ', 'CL17            ', 'CL18            ', & ! 7   
     &    'CL19            ', 'CL20            ', 'CL21            ', & ! 8   
     &    'CL22            ', 'CL23            ', 'CL23a           ', & ! 9   
     &    'CL24            ', 'SA01            ', 'SA02            ', & ! 0   
     &    'SA03            ', 'SA04            ', 'SA06            ', & ! 1   
     &    'SA07            ', 'SA08            ', 'SA09            ', & ! 2   
     &    'SA10            ', 'SA11            ', 'SA12            ', & ! 3   
     &    'SA13            ', 'HET_H2NO3PIB    ', 'HET_H2NO3PJB    ', & ! 4   
     &    'HET_H2NO3PKB    ', 'PCSOA           ', 'POA_AGE1        ', & ! 5   
     &    'POA_AGE2        ', 'POA_AGE3        ', 'POA_AGE4        ', & ! 6   
     &    'POA_AGE5        ', 'POA_AGE6        ', 'POA_AGE7        ', & ! 7   
     &    'POA_AGE8        ', 'POA_AGE9        ', 'POA_AGE10       ', & ! 8   
     &    'T01             ', 'T02             ', 'T03             ', & ! 9   
     &    'T04             ', 'T05             ', 'TCL1            ', & ! 0   
     &    'T06             ', 'T07             ', 'T08             ', & ! 1   
     &    'T09             ', 'TCL2            ', 'T10             ', & ! 2   
     &    'T11             ', 'T12             ', 'TCL3            ', & ! 3   
     &    'T13             ', 'T14             ', 'T15             ', & ! 4   
     &    'T16             ', 'TCL4            ', 'T17             ', & ! 5   
     &    'T18             ', 'T19             ', 'T20             ', & ! 6   
     &    'TCL5            ', 'T21             ', 'TCL6            ', & ! 7   
     &    'HG1             ', 'HG2             ', 'HG3             ', & ! 8   
     &    'HG4             ', 'HG5             '/                   !    

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

! Special Rate information not available ..
      INTEGER, PARAMETER :: NSPECIAL_RXN = 0
      INTEGER            :: ISPECIAL( 1, 2 )

! Special Rate information not available ...
      INTEGER, PARAMETER :: NSPECIAL = 0

! Special Rate information not available ...
      CHARACTER( 16 )    :: SPECIAL( 1 )

      INTEGER, PARAMETER :: MAXSPECTERMS =   1
      REAL( 8 )          :: KC_COEFFS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: INDEX_KTERMS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: INDEX_CTERMS( NSPECIAL + 1, MAXSPECTERMS)
      REAL( 8 )          :: OPERATOR_COEFFS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: OPERATORS( NSPECIAL + 1, MAXSPECTERMS)


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
       LOGICAL,  PARAMETER :: USE_SPECIAL_RATES = .FALSE.
! pointers and names to specific photolysis rates
       INTEGER, PARAMETER  :: IJ_NO2_IUPAC10      =   1
       INTEGER, PARAMETER  :: IJ_O3_O3P_IUPAC10   =   2
       INTEGER, PARAMETER  :: IJ_O3_O1D_IUPAC10   =   3
       INTEGER, PARAMETER  :: IJ_H2O2_IUPAC10     =   4
       INTEGER, PARAMETER  :: IJ_NO3NO2_06        =   5
       INTEGER, PARAMETER  :: IJ_NO3NO_06         =   6
       INTEGER, PARAMETER  :: IJ_N2O5_IUPAC10     =   7
       INTEGER, PARAMETER  :: IJ_HONO_IUPAC10     =   8
       INTEGER, PARAMETER  :: IJ_HNO3_IUPAC10     =   9
       INTEGER, PARAMETER  :: IJ_PNA_IUPAC10      =  10
       INTEGER, PARAMETER  :: IJ_PAN_IUPAC10      =  11
       INTEGER, PARAMETER  :: IJ_MEPX_IUPAC10     =  12
       INTEGER, PARAMETER  :: IJ_NTR_IUPAC10      =  13
       INTEGER, PARAMETER  :: IJ_FORM_R_IUPAC10   =  14
       INTEGER, PARAMETER  :: IJ_FORM_M_IUPAC10   =  15
       INTEGER, PARAMETER  :: IJ_ALD2_R_IUPAC10   =  16
       INTEGER, PARAMETER  :: IJ_ALDX_R_IUPAC10   =  17
       INTEGER, PARAMETER  :: IJ_GLYD_IUPAC10     =  18
       INTEGER, PARAMETER  :: IJ_GLY_R_IUPAC10    =  19
       INTEGER, PARAMETER  :: IJ_MGLY_IUPAC10     =  20
       INTEGER, PARAMETER  :: IJ_KET_IUPAC10      =  21
       INTEGER, PARAMETER  :: IJ_ACET_IUPAC10     =  22
       INTEGER, PARAMETER  :: IJ_ISPD             =  23
       INTEGER, PARAMETER  :: IJ_HPALD            =  24
       INTEGER, PARAMETER  :: IJ_CL2_IUPAC04      =  25
       INTEGER, PARAMETER  :: IJ_HOCL_IUPAC04     =  26
       INTEGER, PARAMETER  :: IJ_FMCL_IUPAC04     =  27
       INTEGER, PARAMETER  :: IJ_CLNO2            =  28
       INTEGER, PARAMETER  :: IJ_ACRO_09          =  29
       INTEGER, PARAMETER  :: IK_HETERO_NTR2      =   1
       INTEGER, PARAMETER  :: IK_HETERO_N2O5IJ    =   2
       INTEGER, PARAMETER  :: IK_HETERO_N2O5K     =   3
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAIJ =   4
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAK  =   5
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBIJ =   6
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBK  =   7
       INTEGER, PARAMETER  :: IK_HETERO_NO2       =   8
       INTEGER, PARAMETER  :: IK_HETERO_IEPOX     =   9
       INTEGER, PARAMETER  :: IK_HETERO_GLY       =  10
       INTEGER, PARAMETER  :: IK_HETERO_MGLY      =  11
       END MODULE RXNS_DATA
