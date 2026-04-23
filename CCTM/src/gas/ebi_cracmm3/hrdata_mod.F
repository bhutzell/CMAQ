
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

      MODULE  HRDATA

C*************************************************************************
C
C  FUNCTION:  Mechanism & solver data for EBI solver
C
C  PRECONDITIONS: For the CRACMM3 mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Apr 10, 2026
C
C   18 Jul 14 B.Hutzell: revised to use real(8) variables for solution
C*************************************************************************

c..EBI solver fixed parameters
      INTEGER, PARAMETER    ::  NEBITER       = 1000      ! No. of iterations for EBI
      INTEGER, PARAMETER    ::  MAX_CELLS_EBI = 1         ! max # cells in local conc arrays
      REAL( 8 ), PARAMETER  ::  DELTAT        = 2.500D+00 ! EBI time step (min)

c..Mechanism specific variables
      INTEGER   N_SPEC       ! No. of species in mechanism
      INTEGER   N_RXNS       ! No. of reactions in mechanism
      INTEGER   N_EBISP      ! No. of species solved by EBI
      INTEGER   NING1        ! No. of species in group 1
      INTEGER   NING2        ! No. of species in group 2
      INTEGER   N_SPCL_RKS   ! No. of special rate constants
      INTEGER   N_ACTSP      ! No. of species in mechanism (non-ss spec)
      INTEGER   NCELL        ! cell number in EBI Block

c..Control flags
      LOGICAL   L_AE_VRSN    ! Flag for aerosol version of mech
      LOGICAL   L_AQ_VRSN    ! Flag for aqueous chemistry version of mech
      LOGICAL   CALL_IRR     ! Flag initializing for IRR and calling PA_IRR

  ! SWITCH for calling DEGRADE routine
      LOGICAL, SAVE :: CALL_DEG = .TRUE.

c..Miscellaneous variables
      INTEGER   N_EBI_IT      ! No. of iterations for EBI
      INTEGER   N_EBI_STEPS   ! No. of time steps for EBI
      INTEGER   N_INR_STEPS   ! No. of inner time steps for EBI
      INTEGER   PECOL_OFFSET  ! Local Column Offset for processor
      INTEGER   PEROW_OFFSET  ! Local Column Offset for processor
  
      INTEGER, SAVE ::   MODEL_SPECIES ! No. of species in CGRID

      REAL( 8 )    EBI_TMSTEP          ! Time step for EBI loops (min)
      REAL( 8 )    CHEMSTEP            ! Chemistry integration interval (min)

c...Allocatable arrays
      INTEGER, ALLOCATABLE :: EBISP( : )         ! Index of EBI species
      INTEGER, ALLOCATABLE :: SSASP( : )         ! Index of ss species
      INTEGER, ALLOCATABLE :: ACTSP( : )         ! Index of active species
      INTEGER, ALLOCATABLE :: IOLD2NEW( :,: )    ! YC species map, used if NSPECIAL > 0

      REAL( 8 ), ALLOCATABLE ::  RKI(  : )     ! Rate constants
      REAL( 8 ), ALLOCATABLE ::  RKI_SAV(  :,  : )  ! Saved values of RKI for SS species calcs

      REAL( 8 ), ALLOCATABLE ::  YC (  : )      ! Species concentrations
      REAL( 8 ), ALLOCATABLE ::  YC0(  : )      ! Species concentrations
      REAL( 8 ), ALLOCATABLE ::  YCP(  : )      ! Species concentrations
      REAL( 8 ), ALLOCATABLE ::  SYC(  :, : )   ! Species concentrations

      REAL, ALLOCATABLE      :: FORWARD_CONV( : )  ! CGRID to CHEM Species conversion factor
      REAL, ALLOCATABLE      :: REVERSE_CONV( : )  ! CHEM to CGRID Species conversion factor

      REAL( 8 ), ALLOCATABLE ::  RXRAT( : )     ! Reaction rates
      REAL( 8 ), ALLOCATABLE ::  RTOL(  : )        ! Species tolerances
      REAL( 8 ), ALLOCATABLE ::  YCCELL( : )       ! ALL Species in cgrid
      REAL( 8 ), ALLOCATABLE ::  PROD(  : )     ! Prod of species
      REAL( 8 ), ALLOCATABLE ::  LOSS(  : )     ! Loss of species

      REAL, ALLOCATABLE      :: PRES( :, :, : )          ! pressure (Pa)
      REAL, ALLOCATABLE      :: QV  ( :, :, : )          ! water vapor (Kg/Kg air)
      REAL, ALLOCATABLE      :: TA  ( :, :, : )          ! temperature (K)

      REAL, ALLOCATABLE      :: SEAICE ( :, :)            ! fractional seaice cover, [-]
      REAL, ALLOCATABLE      :: DENS( :, :, : )           ! mass density (Kg/m^3)

      REAL, ALLOCATABLE      :: SEAWATER_ZONE   ( :,: )       ! fractional area of OPEN+SURF
      LOGICAL, ALLOCATABLE   :: LFLAGIRR    ( :,:,: )     ! Conduct IRR for cell

      REAL( 8 ), ALLOCATABLE ::  ATMPRES ( : )     ! Cell pressure, Atm
!      REAL( 8 ), ALLOCATABLE ::  PRESCELL( : )         ! Cell pressure, Pa
      REAL( 8 ), ALLOCATABLE ::  H2O     ( : )     ! Cell H2O mixing ratio (ppmV)
      REAL( 8 ), ALLOCATABLE ::  TEMP    ( : )     ! Cell Temperature
      REAL( 8 ), ALLOCATABLE ::  DENSITY ( : )     ! Cell mass density (Kg/m3)
      REAL( 8 ), ALLOCATABLE ::  HET     ( :, : )  ! cell heterogeneous reaction rates
      REAL( 8 ), ALLOCATABLE ::  RJIN    ( :, : )  ! J-values for a cell
      REAL( 8 ), ALLOCATABLE ::  SEAWATER ( : )    ! fractional area of OPEN+SURF for specific cell

c..Species indices
      INTEGER, PARAMETER :: O3               =    1
      INTEGER, PARAMETER :: O3P              =    2
      INTEGER, PARAMETER :: O1D              =    3
      INTEGER, PARAMETER :: H2O2             =    4
      INTEGER, PARAMETER :: HO               =    5
      INTEGER, PARAMETER :: NO2              =    6
      INTEGER, PARAMETER :: NO               =    7
      INTEGER, PARAMETER :: NO3              =    8
      INTEGER, PARAMETER :: HONO             =    9
      INTEGER, PARAMETER :: HNO3             =   10
      INTEGER, PARAMETER :: HNO4             =   11
      INTEGER, PARAMETER :: HO2              =   12
      INTEGER, PARAMETER :: HCHO             =   13
      INTEGER, PARAMETER :: CO               =   14
      INTEGER, PARAMETER :: ACD              =   15
      INTEGER, PARAMETER :: MO2              =   16
      INTEGER, PARAMETER :: ALD              =   17
      INTEGER, PARAMETER :: ETHP             =   18
      INTEGER, PARAMETER :: ACT              =   19
      INTEGER, PARAMETER :: ACO3             =   20
      INTEGER, PARAMETER :: UALD             =   21
      INTEGER, PARAMETER :: KET              =   22
      INTEGER, PARAMETER :: PINAL            =   23
      INTEGER, PARAMETER :: PINALP           =   24
      INTEGER, PARAMETER :: LIMAL            =   25
      INTEGER, PARAMETER :: LIMALP           =   26
      INTEGER, PARAMETER :: MEK              =   27
      INTEGER, PARAMETER :: HKET             =   28
      INTEGER, PARAMETER :: MACR             =   29
      INTEGER, PARAMETER :: MACP             =   30
      INTEGER, PARAMETER :: XO2              =   31
      INTEGER, PARAMETER :: MVK              =   32
      INTEGER, PARAMETER :: GLY              =   33
      INTEGER, PARAMETER :: MGLY             =   34
      INTEGER, PARAMETER :: DCB1             =   35
      INTEGER, PARAMETER :: DCB2             =   36
      INTEGER, PARAMETER :: BALD             =   37
      INTEGER, PARAMETER :: BEN              =   38
      INTEGER, PARAMETER :: BAL1             =   39
      INTEGER, PARAMETER :: OP1              =   40
      INTEGER, PARAMETER :: OP2              =   41
      INTEGER, PARAMETER :: OPB              =   42
      INTEGER, PARAMETER :: VOP3             =   43
      INTEGER, PARAMETER :: PAA              =   44
      INTEGER, PARAMETER :: ONIT             =   45
      INTEGER, PARAMETER :: PAN              =   46
      INTEGER, PARAMETER :: CO2              =   47
      INTEGER, PARAMETER :: PPN              =   48
      INTEGER, PARAMETER :: RCO3             =   49
      INTEGER, PARAMETER :: HC3P             =   50
      INTEGER, PARAMETER :: VTRPN            =   51
      INTEGER, PARAMETER :: VHONIT           =   52
      INTEGER, PARAMETER :: N2O5             =   53
      INTEGER, PARAMETER :: SO2              =   54
      INTEGER, PARAMETER :: SULF             =   55
      INTEGER, PARAMETER :: SULRXN           =   56
      INTEGER, PARAMETER :: ETH              =   57
      INTEGER, PARAMETER :: HC3              =   58
      INTEGER, PARAMETER :: ASOATJ           =   59
      INTEGER, PARAMETER :: HC5              =   60
      INTEGER, PARAMETER :: HC5P             =   61
      INTEGER, PARAMETER :: ETE              =   62
      INTEGER, PARAMETER :: ETEP             =   63
      INTEGER, PARAMETER :: OLT              =   64
      INTEGER, PARAMETER :: OLTP             =   65
      INTEGER, PARAMETER :: OLI              =   66
      INTEGER, PARAMETER :: OLIP             =   67
      INTEGER, PARAMETER :: ACE              =   68
      INTEGER, PARAMETER :: ORA1             =   69
      INTEGER, PARAMETER :: BENP             =   70
      INTEGER, PARAMETER :: PHEN             =   71
      INTEGER, PARAMETER :: TOL              =   72
      INTEGER, PARAMETER :: TOLP             =   73
      INTEGER, PARAMETER :: CSL              =   74
      INTEGER, PARAMETER :: XYL              =   75
      INTEGER, PARAMETER :: XYLP             =   76
      INTEGER, PARAMETER :: EBZ              =   77
      INTEGER, PARAMETER :: EBZP             =   78
      INTEGER, PARAMETER :: ISO              =   79
      INTEGER, PARAMETER :: ISON             =   80
      INTEGER, PARAMETER :: ISONP            =   81
      INTEGER, PARAMETER :: ISOP             =   82
      INTEGER, PARAMETER :: ISHP             =   83
      INTEGER, PARAMETER :: IEPOX            =   84
      INTEGER, PARAMETER :: IPX              =   85
      INTEGER, PARAMETER :: INALD            =   86
      INTEGER, PARAMETER :: ROH              =   87
      INTEGER, PARAMETER :: API              =   88
      INTEGER, PARAMETER :: APIP1            =   89
      INTEGER, PARAMETER :: APIP2            =   90
      INTEGER, PARAMETER :: LIM              =   91
      INTEGER, PARAMETER :: LIMP1            =   92
      INTEGER, PARAMETER :: LIMP2            =   93
      INTEGER, PARAMETER :: ACTP             =   94
      INTEGER, PARAMETER :: MEKP             =   95
      INTEGER, PARAMETER :: KETP             =   96
      INTEGER, PARAMETER :: MCP              =   97
      INTEGER, PARAMETER :: MVKP             =   98
      INTEGER, PARAMETER :: UALP             =   99
      INTEGER, PARAMETER :: DCB3             =  100
      INTEGER, PARAMETER :: BALP             =  101
      INTEGER, PARAMETER :: ADDC             =  102
      INTEGER, PARAMETER :: CHO              =  103
      INTEGER, PARAMETER :: MCT              =  104
      INTEGER, PARAMETER :: MCTO             =  105
      INTEGER, PARAMETER :: MOH              =  106
      INTEGER, PARAMETER :: EOH              =  107
      INTEGER, PARAMETER :: ETEG             =  108
      INTEGER, PARAMETER :: HC10P            =  109
      INTEGER, PARAMETER :: MAHP             =  110
      INTEGER, PARAMETER :: ORA2             =  111
      INTEGER, PARAMETER :: ORAP             =  112
      INTEGER, PARAMETER :: MPAN             =  113
      INTEGER, PARAMETER :: MCTP             =  114
      INTEGER, PARAMETER :: OLNN             =  115
      INTEGER, PARAMETER :: OLND             =  116
      INTEGER, PARAMETER :: APINP1           =  117
      INTEGER, PARAMETER :: APINP2           =  118
      INTEGER, PARAMETER :: LIMNP1           =  119
      INTEGER, PARAMETER :: LIMNP2           =  120
      INTEGER, PARAMETER :: ADCN             =  121
      INTEGER, PARAMETER :: VHOM             =  122
      INTEGER, PARAMETER :: VROCP4OXY2       =  123
      INTEGER, PARAMETER :: VROCN1OXY6       =  124
      INTEGER, PARAMETER :: FURANONE         =  125
      INTEGER, PARAMETER :: VROCP3OXY2       =  126
      INTEGER, PARAMETER :: VROCP0OXY4       =  127
      INTEGER, PARAMETER :: BAL2             =  128
      INTEGER, PARAMETER :: VELHOM           =  129
      INTEGER, PARAMETER :: VROCIOXY         =  130
      INTEGER, PARAMETER :: SLOWROC          =  131
      INTEGER, PARAMETER :: ACRO             =  132
      INTEGER, PARAMETER :: BDE13            =  133
      INTEGER, PARAMETER :: BDE13P           =  134
      INTEGER, PARAMETER :: FURAN            =  135
      INTEGER, PARAMETER :: FURANO2          =  136
      INTEGER, PARAMETER :: PROG             =  137
      INTEGER, PARAMETER :: SESQ             =  138
      INTEGER, PARAMETER :: SESQNRO2         =  139
      INTEGER, PARAMETER :: VROCN2OXY2       =  140
      INTEGER, PARAMETER :: SESQRO2          =  141
      INTEGER, PARAMETER :: VROCP0OXY2       =  142
      INTEGER, PARAMETER :: VROCP1OXY3       =  143
      INTEGER, PARAMETER :: AGLYOLIGJ        =  144
      INTEGER, PARAMETER :: IEPOXP           =  145
      INTEGER, PARAMETER :: AISO3NOSJ        =  146
      INTEGER, PARAMETER :: ASO4J            =  147
      INTEGER, PARAMETER :: AISO3OSJ         =  148
      INTEGER, PARAMETER :: AISO4J           =  149
      INTEGER, PARAMETER :: AISO5J           =  150
      INTEGER, PARAMETER :: VROCP6ALK        =  151
      INTEGER, PARAMETER :: VROCP6ALKP       =  152
      INTEGER, PARAMETER :: VROCP5ALK        =  153
      INTEGER, PARAMETER :: VROCP5ALKP       =  154
      INTEGER, PARAMETER :: VROCP4ALK        =  155
      INTEGER, PARAMETER :: VROCP4ALKP       =  156
      INTEGER, PARAMETER :: VROCP3ALK        =  157
      INTEGER, PARAMETER :: VROCP3ALKP       =  158
      INTEGER, PARAMETER :: VROCP2ALK        =  159
      INTEGER, PARAMETER :: VROCP2ALKP       =  160
      INTEGER, PARAMETER :: VROCP1ALK        =  161
      INTEGER, PARAMETER :: VROCP1ALKP       =  162
      INTEGER, PARAMETER :: HC10             =  163
      INTEGER, PARAMETER :: VROCP6ALKP2      =  164
      INTEGER, PARAMETER :: VROCP5ALKP2      =  165
      INTEGER, PARAMETER :: VROCP4ALKP2      =  166
      INTEGER, PARAMETER :: VROCP2OXY2       =  167
      INTEGER, PARAMETER :: VROCP3ALKP2      =  168
      INTEGER, PARAMETER :: VROCP1OXY1       =  169
      INTEGER, PARAMETER :: VROCP2ALKP2      =  170
      INTEGER, PARAMETER :: VROCP1ALKP2      =  171
      INTEGER, PARAMETER :: VROCN1OXY1       =  172
      INTEGER, PARAMETER :: HC10P2           =  173
      INTEGER, PARAMETER :: VROCP6ARO        =  174
      INTEGER, PARAMETER :: VROCP6AROP       =  175
      INTEGER, PARAMETER :: VROCN2OXY4       =  176
      INTEGER, PARAMETER :: VROCN1OXY3       =  177
      INTEGER, PARAMETER :: VROCP5ARO        =  178
      INTEGER, PARAMETER :: VROCP5AROP       =  179
      INTEGER, PARAMETER :: NAPH             =  180
      INTEGER, PARAMETER :: NAPHP            =  181
      INTEGER, PARAMETER :: VROCN2OXY8       =  182
      INTEGER, PARAMETER :: VROCP5OXY1       =  183
      INTEGER, PARAMETER :: VROCP6OXY1       =  184
      INTEGER, PARAMETER :: ECH4             =  185
      INTEGER, PARAMETER :: ATRPNJ           =  186
      INTEGER, PARAMETER :: AHOMJ            =  187
      INTEGER, PARAMETER :: AHONITJ          =  188
      INTEGER, PARAMETER :: STY              =  189
      INTEGER, PARAMETER :: STYP             =  190
      INTEGER, PARAMETER :: ANO3I            =  191
      INTEGER, PARAMETER :: ANO3J            =  192
      INTEGER, PARAMETER :: CL2              =  193
      INTEGER, PARAMETER :: CL               =  194
      INTEGER, PARAMETER :: CLO              =  195
      INTEGER, PARAMETER :: OCLO             =  196
      INTEGER, PARAMETER :: CL2O2            =  197
      INTEGER, PARAMETER :: CLOO             =  198
      INTEGER, PARAMETER :: HOCL             =  199
      INTEGER, PARAMETER :: CLNO             =  200
      INTEGER, PARAMETER :: CLNO2            =  201
      INTEGER, PARAMETER :: CLNO3            =  202
      INTEGER, PARAMETER :: HCL              =  203
      INTEGER, PARAMETER :: NO2PIJ           =  204
      INTEGER, PARAMETER :: NO2PK            =  205
      INTEGER, PARAMETER :: ACLI             =  206
      INTEGER, PARAMETER :: ACLJ             =  207
      INTEGER, PARAMETER :: ACLK             =  208
      INTEGER, PARAMETER :: AMTN1J           =  209
      INTEGER, PARAMETER :: SO4HRXN          =  210
      INTEGER, PARAMETER :: AFEJ             =  211
      INTEGER, PARAMETER :: AMNJ             =  212
      INTEGER, PARAMETER :: AHMSJ            =  213

      END MODULE HRDATA
