
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
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      MODULE  HRDATA

C*************************************************************************
C
C  FUNCTION:  Mechanism & solver data for EBI solver
C
C  PRECONDITIONS: For the SAPRC07TB family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Aug 18, 2016
C
C   18 Jul 14 B.Hutzell: revised to use real(8) variables for solution
C*************************************************************************

c..EBI solver fixed parameters
      INTEGER, PARAMETER    ::  NEBITER       = 1000     ! No. of iterations for EBI
      INTEGER, PARAMETER    ::  MAX_CELLS_EBI = 1        ! max # cells in local conc arrays
      REAL( 8 ), PARAMETER  ::  DELTAT        = 2.5D+00  ! EBI time step

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


c..Miscellaneous variables
      INTEGER   LOGDEV        ! Unit number of output log
      INTEGER   N_EBI_IT      ! No. of iterations for EBI
      INTEGER   N_EBI_STEPS   ! No. of time steps for EBI
      INTEGER   N_INR_STEPS   ! No. of inner time steps for EBI

      REAL( 8 )    EBI_TMSTEP          ! Time step for EBI loops (min)

c...Allocatable arrays
      INTEGER, ALLOCATABLE :: EBISP( : )         ! Index of EBI species
      INTEGER, ALLOCATABLE :: SSASP( : )         ! Index of ss species
      INTEGER, ALLOCATABLE :: ACTSP( : )         ! Index of active species

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
      REAL( 8 ), ALLOCATABLE ::  PROD(  : )     ! Prod of species
      REAL( 8 ), ALLOCATABLE ::  LOSS(  : )     ! Loss of species

      REAL, ALLOCATABLE      :: PRES( :, :, : )          ! pressure (Pa)
      REAL, ALLOCATABLE      :: QV  ( :, :, : )          ! water vapor (Kg/Kg air)
      REAL, ALLOCATABLE      :: TA  ( :, :, : )          ! temperature (K)
      REAL, ALLOCATABLE      :: RJ  ( :, :, :, : )       ! J-values (min-1)

      REAL, ALLOCATABLE      :: SEAICE ( :, :)            ! fractional seaice cover, [-]
      REAL, ALLOCATABLE      :: DENS( :, :, : )           ! mass density (Kg/m^3)

      REAL,    ALLOCATABLE   :: OPEN_ZONE   ( :,: )       ! fractional sea water cover, [-]
      REAL,    ALLOCATABLE   :: SURF_ZONE   ( :,: )       ! fractional surf-zone cover, [-]
      LOGICAL, ALLOCATABLE   :: LAND_ZONE   ( :,: )       ! Is the surface totally land?

      REAL( 8 ), ALLOCATABLE ::  ATMPRES ( : )     ! Cell pressure, Atm
!      REAL( 8 ), ALLOCATABLE ::  PRESCELL( : )         ! Cell pressure, Pa
      REAL( 8 ), ALLOCATABLE ::  H2O     ( : )     ! Cell H2O mixing ratio (ppmV)
      REAL( 8 ), ALLOCATABLE ::  TEMP    ( : )     ! Cell Temperature
      REAL( 8 ), ALLOCATABLE ::  HET     ( :, : )  ! cell heterogeneous reaction rates
      REAL( 8 ), ALLOCATABLE ::  RJIN    ( :, : )  ! J-values for a cell
      LOGICAL, ALLOCATABLE   ::  LAND    ( : )     ! land_zone value for specific cell

c..Species indices
      INTEGER, PARAMETER :: AXYL1J           =    1
      INTEGER, PARAMETER :: AXYL2J           =    2
      INTEGER, PARAMETER :: ATOL1J           =    3
      INTEGER, PARAMETER :: ATOL2J           =    4
      INTEGER, PARAMETER :: ABNZ1J           =    5
      INTEGER, PARAMETER :: ABNZ2J           =    6
      INTEGER, PARAMETER :: ATRP1J           =    7
      INTEGER, PARAMETER :: ATRP2J           =    8
      INTEGER, PARAMETER :: AISO1J           =    9
      INTEGER, PARAMETER :: AISO2J           =   10
      INTEGER, PARAMETER :: ASQTJ            =   11
      INTEGER, PARAMETER :: APAH1J           =   12
      INTEGER, PARAMETER :: APAH2J           =   13
      INTEGER, PARAMETER :: AALK1J           =   14
      INTEGER, PARAMETER :: AALK2J           =   15
      INTEGER, PARAMETER :: AISO3J           =   16
      INTEGER, PARAMETER :: O1D              =   17
      INTEGER, PARAMETER :: SO2              =   18
      INTEGER, PARAMETER :: SULF             =   19
      INTEGER, PARAMETER :: SULRXN           =   20
      INTEGER, PARAMETER :: BACL             =   21
      INTEGER, PARAMETER :: ISOPRXN          =   22
      INTEGER, PARAMETER :: BENZENE          =   23
      INTEGER, PARAMETER :: SOAALK           =   24
      INTEGER, PARAMETER :: ALKRXN           =   25
      INTEGER, PARAMETER :: CLACET           =   26
      INTEGER, PARAMETER :: CLCHO            =   27
      INTEGER, PARAMETER :: BNZNRXN          =   28
      INTEGER, PARAMETER :: BNZHRXN          =   29
      INTEGER, PARAMETER :: XYLNRXN          =   30
      INTEGER, PARAMETER :: XYLHRXN          =   31
      INTEGER, PARAMETER :: TOLNRXN          =   32
      INTEGER, PARAMETER :: TOLHRXN          =   33
      INTEGER, PARAMETER :: PAHNRXN          =   34
      INTEGER, PARAMETER :: PAHHRXN          =   35
      INTEGER, PARAMETER :: ACLI             =   36
      INTEGER, PARAMETER :: ACLJ             =   37
      INTEGER, PARAMETER :: ACLK             =   38
      INTEGER, PARAMETER :: PCVOC            =   39
      INTEGER, PARAMETER :: PCSOARXN         =   40
      INTEGER, PARAMETER :: N2O5             =   41
      INTEGER, PARAMETER :: HO2H             =   42
      INTEGER, PARAMETER :: PAN              =   43
      INTEGER, PARAMETER :: PAN2             =   44
      INTEGER, PARAMETER :: PBZN             =   45
      INTEGER, PARAMETER :: MAPAN            =   46
      INTEGER, PARAMETER :: TBUO             =   47
      INTEGER, PARAMETER :: ISOPOOH          =   48
      INTEGER, PARAMETER :: IEPOX            =   49
      INTEGER, PARAMETER :: TOLUENE          =   50
      INTEGER, PARAMETER :: MXYL             =   51
      INTEGER, PARAMETER :: OXYL             =   52
      INTEGER, PARAMETER :: PXYL             =   53
      INTEGER, PARAMETER :: TMBENZ124        =   54
      INTEGER, PARAMETER :: ETOH             =   55
      INTEGER, PARAMETER :: ALK1             =   56
      INTEGER, PARAMETER :: ALK2             =   57
      INTEGER, PARAMETER :: ALK3             =   58
      INTEGER, PARAMETER :: ALK4             =   59
      INTEGER, PARAMETER :: ALK5             =   60
      INTEGER, PARAMETER :: ARO1             =   61
      INTEGER, PARAMETER :: NAPHTHAL         =   62
      INTEGER, PARAMETER :: CLNO             =   63
      INTEGER, PARAMETER :: CLONO            =   64
      INTEGER, PARAMETER :: HOCL             =   65
      INTEGER, PARAMETER :: H2NO3PK          =   66
      INTEGER, PARAMETER :: VIVPO1           =   67
      INTEGER, PARAMETER :: HNO4             =   68
      INTEGER, PARAMETER :: COOH             =   69
      INTEGER, PARAMETER :: CCOOOH           =   70
      INTEGER, PARAMETER :: NPHE             =   71
      INTEGER, PARAMETER :: xTBUO            =   72
      INTEGER, PARAMETER :: ROOH             =   73
      INTEGER, PARAMETER :: R6OOH            =   74
      INTEGER, PARAMETER :: RAOOH            =   75
      INTEGER, PARAMETER :: yISOPOOH         =   76
      INTEGER, PARAMETER :: ARO2MN           =   77
      INTEGER, PARAMETER :: ACETYLENE        =   78
      INTEGER, PARAMETER :: CL2              =   79
      INTEGER, PARAMETER :: CLONO2           =   80
      INTEGER, PARAMETER :: CLCCHO           =   81
      INTEGER, PARAMETER :: HCHO_PRIMARY     =   82
      INTEGER, PARAMETER :: CCHO_PRIMARY     =   83
      INTEGER, PARAMETER :: H2NO3PIJ         =   84
      INTEGER, PARAMETER :: HONO             =   85
      INTEGER, PARAMETER :: BALD             =   86
      INTEGER, PARAMETER :: BENZRO2          =   87
      INTEGER, PARAMETER :: PAHRO2           =   88
      INTEGER, PARAMETER :: SESQRXN          =   89
      INTEGER, PARAMETER :: AOLGBJ           =   90
      INTEGER, PARAMETER :: VSVPO3           =   91
      INTEGER, PARAMETER :: RCOOOH           =   92
      INTEGER, PARAMETER :: ETHENE           =   93
      INTEGER, PARAMETER :: PROPENE          =   94
      INTEGER, PARAMETER :: BUTADIENE13      =   95
      INTEGER, PARAMETER :: ISOPRENE         =   96
      INTEGER, PARAMETER :: APIN             =   97
      INTEGER, PARAMETER :: TRPRXN           =   98
      INTEGER, PARAMETER :: TOLRO2           =   99
      INTEGER, PARAMETER :: OLE1             =  100
      INTEGER, PARAMETER :: OLE2             =  101
      INTEGER, PARAMETER :: TERP             =  102
      INTEGER, PARAMETER :: SESQ             =  103
      INTEGER, PARAMETER :: ACRO_PRIMARY     =  104
      INTEGER, PARAMETER :: VLVPO1           =  105
      INTEGER, PARAMETER :: VSVPO1           =  106
      INTEGER, PARAMETER :: VSVPO2           =  107
      INTEGER, PARAMETER :: VSVOO1           =  108
      INTEGER, PARAMETER :: MEOH             =  109
      INTEGER, PARAMETER :: RNO3             =  110
      INTEGER, PARAMETER :: ACETONE          =  111
      INTEGER, PARAMETER :: xMEO2            =  112
      INTEGER, PARAMETER :: HCOCO3           =  113
      INTEGER, PARAMETER :: xACROLEIN        =  114
      INTEGER, PARAMETER :: xCLACET          =  115
      INTEGER, PARAMETER :: VSVOO2           =  116
      INTEGER, PARAMETER :: VSVOO3           =  117
      INTEGER, PARAMETER :: xOH              =  118
      INTEGER, PARAMETER :: AFG1             =  119
      INTEGER, PARAMETER :: AFG2             =  120
      INTEGER, PARAMETER :: ACROLEIN         =  121
      INTEGER, PARAMETER :: yRAOOH           =  122
      INTEGER, PARAMETER :: IEPOXOO          =  123
      INTEGER, PARAMETER :: CLNO2            =  124
      INTEGER, PARAMETER :: CLO              =  125
      INTEGER, PARAMETER :: CCOOH            =  126
      INTEGER, PARAMETER :: xMACO3           =  127
      INTEGER, PARAMETER :: IPRD             =  128
      INTEGER, PARAMETER :: XYLRO2           =  129
      INTEGER, PARAMETER :: MACR             =  130
      INTEGER, PARAMETER :: MVK              =  131
      INTEGER, PARAMETER :: xAFG3            =  132
      INTEGER, PARAMETER :: AOLGAJ           =  133
      INTEGER, PARAMETER :: xNO2             =  134
      INTEGER, PARAMETER :: xHOCCHO          =  135
      INTEGER, PARAMETER :: HOCCHO           =  136
      INTEGER, PARAMETER :: xBALD            =  137
      INTEGER, PARAMETER :: xMACR            =  138
      INTEGER, PARAMETER :: xCL              =  139
      INTEGER, PARAMETER :: VLVOO1           =  140
      INTEGER, PARAMETER :: VLVOO2           =  141
      INTEGER, PARAMETER :: RCOOH            =  142
      INTEGER, PARAMETER :: xMVK             =  143
      INTEGER, PARAMETER :: CCHO             =  144
      INTEGER, PARAMETER :: xBACL            =  145
      INTEGER, PARAMETER :: AFG3             =  146
      INTEGER, PARAMETER :: xCLCCHO          =  147
      INTEGER, PARAMETER :: xRNO3            =  148
      INTEGER, PARAMETER :: BZCO3            =  149
      INTEGER, PARAMETER :: CRES             =  150
      INTEGER, PARAMETER :: xIPRD            =  151
      INTEGER, PARAMETER :: xACETONE         =  152
      INTEGER, PARAMETER :: BZO              =  153
      INTEGER, PARAMETER :: xMECO3           =  154
      INTEGER, PARAMETER :: xRCO3            =  155
      INTEGER, PARAMETER :: xPROD2           =  156
      INTEGER, PARAMETER :: O3P              =  157
      INTEGER, PARAMETER :: HNO3             =  158
      INTEGER, PARAMETER :: xCO              =  159
      INTEGER, PARAMETER :: MGLY             =  160
      INTEGER, PARAMETER :: xMEK             =  161
      INTEGER, PARAMETER :: RCHO             =  162
      INTEGER, PARAMETER :: xAFG1            =  163
      INTEGER, PARAMETER :: xAFG2            =  164
      INTEGER, PARAMETER :: GLY              =  165
      INTEGER, PARAMETER :: RCO3             =  166
      INTEGER, PARAMETER :: MACO3            =  167
      INTEGER, PARAMETER :: MEK              =  168
      INTEGER, PARAMETER :: HCOOH            =  169
      INTEGER, PARAMETER :: NO               =  170
      INTEGER, PARAMETER :: xGLY             =  171
      INTEGER, PARAMETER :: xMGLY            =  172
      INTEGER, PARAMETER :: PRD2             =  173
      INTEGER, PARAMETER :: xRCHO            =  174
      INTEGER, PARAMETER :: O3               =  175
      INTEGER, PARAMETER :: MEO2             =  176
      INTEGER, PARAMETER :: MECO3            =  177
      INTEGER, PARAMETER :: xCCHO            =  178
      INTEGER, PARAMETER :: yR6OOH           =  179
      INTEGER, PARAMETER :: NO2              =  180
      INTEGER, PARAMETER :: xHCHO            =  181
      INTEGER, PARAMETER :: HCHO             =  182
      INTEGER, PARAMETER :: HCL              =  183
      INTEGER, PARAMETER :: CO               =  184
      INTEGER, PARAMETER :: yROOH            =  185
      INTEGER, PARAMETER :: zRNO3            =  186
      INTEGER, PARAMETER :: CO2              =  187
      INTEGER, PARAMETER :: NO3              =  188
      INTEGER, PARAMETER :: RO2XC            =  189
      INTEGER, PARAMETER :: xHO2             =  190
      INTEGER, PARAMETER :: CL               =  191
      INTEGER, PARAMETER :: RO2C             =  192
      INTEGER, PARAMETER :: HO2              =  193
      INTEGER, PARAMETER :: OH               =  194

      END MODULE HRDATA
