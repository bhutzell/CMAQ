      MODULE SCENE_DATA
       
        IMPLICIT NONE
C......................................................................
C
C  CONTAINS: Miscellaneous data for box model scenario
C
C  DEPENDENT UPON: HGRD3.EXT
C                  VGRD3.EXT
C                  CHEMPARMS.EXT
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, July, 1997
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
         REAL       HT        ! Height (Km)
         REAL       WBAR      ! Average Cloud liquid water content (g/ m**3)
         REAL       CLDT      ! Cloud Tops (Km)
         REAL       CLDB      ! Cloud Bottoms (Km)
         REAL       CFRAC     ! 'Cloud fraction

         REAL       CELL_AIR      ! Input air, ppmV
         REAL       CELL_O2       ! Input molecular oxygen gas, ppmV
         REAL       CELL_H2       ! Input molecular hydrogen gas, ppmV
         REAL       CELL_CH4      ! Input methane, ppmV
         REAL       CELL_N2       ! Input molecular nitrogen gas, ppmV

         REAL      PHYDAT ( MXSTEPS , PVDIM )   !   Real Physical data
c         PVDIM = 1  ====> Temperature (deg K)
c         PVDIM = 2  ====> Pstar (mb)
c         PVDIM = 3  ====> Qv  (kg H2O / kg air)
c         PVDIM = 4  ====> Density ( kg / m**3)

         REAL      EMIS( MXEMSP, MXEMHRS )

      END MODULE SCENE_DATA
