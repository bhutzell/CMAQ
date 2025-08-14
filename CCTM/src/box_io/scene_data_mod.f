      MODULE SCENE_DATA
       
        IMPLICIT NONE
C......................................................................
C
C  CONTAINS: Miscellaneous data for box model scenario
C
C......................................................................
C


         INTEGER, PARAMETER :: MXSTEPS = 10000 ! Max number of model time steps

         INTEGER, PARAMETER :: MXEMHRS = 100   ! max number of emission hrs

         INTEGER, PARAMETER :: MXEMSP = 100    ! Max number of emission species
         
         INTEGER, PARAMETER :: IPVDIM = 2      ! Dimension for integer physical variable array

         INTEGER, PARAMETER :: PVDIM = 4       ! Dimension for real physical variable array

         INTEGER, PARAMETER :: MAX_LAYS = 100
         
         CHARACTER(16)   EMSPEC( MXEMSP )

         INTEGER  IPHYDAT ( MXSTEPS , IPVDIM )   !  Integer physical data
c            IPVDIM = 1  ====> Date (YYYYDDD)
c            IPVDIM = 2  ====> Time (HHMMSS)
         INTEGER  NPHYVALS    ! No. of physical data values
         INTEGER  EMSTDATE    ! Start date of hourly emissions
         INTEGER  EMSTTIME    ! Start time of hourly emissions
         INTEGER  EMTYPE      ! Type of emissions to process
         INTEGER  NEMSP       ! Number of emission species
         INTEGER  NEMHRS      ! Number of hours of emissions

         INTEGER EM_GC_SPC_IND( MXEMSP )  ! Pointer to GC spec

         LOGICAL    LASCII    ! Flag for physical data in ASCII file
         LOGICAL    LDEFAULT  ! Flag to use default physical data
         LOGICAL    LIOAPI    ! Flag for physical data in IOAPI file

         REAL       CELL_TEMP ! Temperature,  (deg K)
         REAL       CELL_PRES ! Pressure,  (mb)
         REAL       QV        ! Water mixing ratio,  (Kg/Kg)
         REAL       MH2O      ! Water Vapor Mixing, (ppmV)
         REAL       JACOBM    ! Jacobian at layer middle scaled by MSFX2
         REAL       JACOBF    ! Jacobian at layer face scaled by MSFX2 
         REAL       DENS      ! Air density,  (Kg/m3)
         REAL       DENS_J    ! Air density times J
         REAL       LAT       ! Latitude (deg)
         REAL       LON       ! Longitude (deg)
         REAL       HT        ! Surface Height (m)
         REAL       WBAR      ! Average Cloud liquid water content (g/ m**3)
         REAL       CLDT      ! Cloud Tops (Km)
         REAL       CLDB      ! Cloud Bottoms (Km)
         REAL       CFRAC     ! 'Cloud fraction

         REAL       CELL_AIR      ! Input air, ppmV
         REAL       CELL_O2       ! Input molecular oxygen gas, ppmV
         REAL       CELL_H2       ! Input molecular hydrogen gas, ppmV
         REAL       CELL_CH4      ! Input methane, ppmV
         REAL       CELL_N2       ! Input molecular nitrogen gas, ppmV

C values defined in box data namelist such gc_chem_inputs.dat
C for air conditions ! default values

         REAL BXM_TEMP     ! = 300.0, Temperature (deg K)                    <
         REAL BXM_PRES     ! = 101325.0, Pressure (Pa) 1 atm                    <
         REAL BXM_QV       ! = 0.01244, Water vapor mixing ratio (Kg/Kg air)   <
         REAL BXM_QC       ! = 0.000, Cloud Water mixing ratio (Kg/Kg air)   <
         REAL BXM_QR       ! = 0.000, Rain Water mixing ratio (Kg/Kg air)   <
         REAL BXM_SLTYP    ! = 5.0, Soil Type (Loam based on WRF 3.8.1 PX categories)
         REAL BXM_RN       ! = 0.0, Nonconvective Rainfall (cm)
         REAL BXM_MOLI     ! = 6.0, inverse of Monin-Obukhov length(1/m)

C Does not override values in RXNS_DATA_MODULE.F90
         REAL BXM_AIR      ! = 1000000.0, Air mixing (ppmV)
         REAL BXM_N2       ! = 771000.0, molecular nitrogen gas (ppmV)
         REAL BXM_O2       ! = 209000.0, molecular oxygen gas (ppmV)
         REAL BXM_H2       ! = 0.0, molecular hydrogen gas (ppmV)
         REAL BXM_CH4      ! = 2,  methane (ppmV) 
         REAL BXM_DENS     ! = 1.225, Air density (Kg/m^3)                   <
  
C for photolysis      !  default values       

         REAL  BXM_LAT      ! = 45.0  ! Latitude (deg)                         <
         REAL  BXM_LON      ! = 0     ! Longitude (deg)                        <
         REAL  BXM_HT       ! = 0.000 ! Surface Height for J values (m)               <
         REAL  BXM_ZH       ! = 50.0  ! mid-layer height above ground
         REAL  BXM_ZF       ! = 100.0 ! full-layer height above ground
         REAL  BXM_WBAR     ! = 0.0   ! Cloud liq. H2O content (gm/m^3)        <
         REAL  BXM_CLDT     ! = 3.0   ! Cloud tops (Km)                        <
         REAL  BXM_CLDB     ! = 2.0   ! Cloud bottoms (Km)                     <
         REAL  BXM_CFRAC    ! = 0.0   ! Cloud fraction                         <


C land surface data
         REAL  BXM_OPEN     ! = 0.0 ! fraction open ocean
         REAL  BXM_SURF     ! = 0.0 ! fraction surf zone
         REAL  BXM_DMS      ! = 0.0 ! Water Dimethyl Sulfide Concentraton (nM)
         REAL  BXM_CHLO     ! = 0.0 ! Water Chlorophyll Concentration (ng/m3), OCI Algorithm
         REAL  BXM_SEAICE   ! = 0.0 ! fraction sea ice
         REAL  BXM_SNOCOV   ! = 0.0 ! snow cover

        CHARACTER(LEN=16) BXM_LU ! = 'LUFRAC_04' ! landuse category based NLCD40 scheme

         REAL      PHYDAT ( MXSTEPS , PVDIM )   !   Real Physical data
c         PVDIM = 1  ====> Temperature (deg K)
c         PVDIM = 2  ====> Pstar (mb)
c         PVDIM = 3  ====> Qv  (kg H2O / kg air)
c         PVDIM = 4  ====> Density ( kg / m**3)

         REAL      EMIS( MXEMSP, MXEMHRS )

         INTEGER :: NUMB_INIT_CONC = 0
         CHARACTER(LEN=16), ALLOCATABLE :: INIT_CONC_SPCS( : )
         REAL,              ALLOCATABLE :: INIT_CONC_VALUES( : )

         INTEGER :: NUMB_MET_DATA = 0
         CHARACTER(LEN=16), ALLOCATABLE :: MET_DATA_SPCS( : )
         REAL,              ALLOCATABLE :: MET_DATA_VALUES( : )

      END MODULE SCENE_DATA

