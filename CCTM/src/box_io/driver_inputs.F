      MODULE DRIVER_INPUTS

       IMPLICIT NONE

! AQCHEM driver inputs

       CHARACTER(LEN = 32) :: WHICH_AQCHEM
       REAL                :: LATITUDE
       REAL                :: LONGITUDE
       INTEGER             :: CALENDAR_DATE
       REAL                :: GREENWICH_TIME
       REAL                :: OPEN_ZONE
       REAL                :: SURF_ZONE
       REAL                :: SEA_ICE

       REAL         CLOUD_THINKESS  ! thinkness of cloudy layer (m)
       REAL         SCAV_AKN_NUM      ! scav coef for aitken aerosol number
       REAL         SCAV_AKN_SRF      ! scav coef for aitken aerosol sfc area
       REAL         SCAV_AKN_MAS      ! scav coef for aitken aerosol mass
       REAL         PRECIP_RATE     ! precip rate (mm/hr)
       REAL         PRESSURE        ! pressure (Pa)
       REAL         DT_CLOUD        ! timestep for cloud (s)
       REAL         TEMPERATURE     ! temperature (K)
       REAL         WATER_CONTENT   ! liquid water content (kg/m3)
       REAL         WATER_TOTAL     ! total water content (kg/m3)

! aerosol mass concentration (ug/m3) in each mode
! AKN is aitken mode
! ACC is accumulation mode
! COR is coarse mode

       REAL         SO4AKN   ! sulfate ion
       REAL         SO4ACC
       REAL         SO4COR

       REAL         NH4AKN   ! ammonium ion
       REAL         NH4ACC
       REAL         NH4COR

       REAL         NO3AKN   ! nitrate ion
       REAL         NO3ACC
       REAL         NO3COR

       REAL         NAAKN    ! sodium ion
       REAL         NAACC 
       REAL         NACOR

       REAL         CLAKN    ! chloride ion
       REAL         CLACC 
       REAL         CLCOR 

       REAL         ECAKN    ! elemental carbon
       REAL         ECACC    ! elemental carbon

! aerosol number density (1/m3) in each mode 

       REAL         NUMAKN
       REAL         NUMACC
       REAL         NUMCOR

! aerosol surface area per volume air (1/m) in each mode

       REAL         SRFAKN
       REAL         SRFACC
       REAL         SRFCOR

C...concentrations (ppmV) for the AQCHEM array GAS

       REAL         SO2    ! Sulfur Dioxide
       REAL         HNO3   ! Nitric Acid
       REAL         N2O5   ! Dinitrogen Pentoxide
       REAL         CO2    ! Carbon Dioxide
       REAL         NH3    ! Ammonia
       REAL         H2O2   ! Hydrogen Perioxide
       REAL         O3     ! Ozone
       REAL         FOA    ! Formic Acid
       REAL         MHP    ! Methyl Hydrogen Peroxide
       REAL         PAA    ! Peroxyacidic Acid
       REAL         H2SO4  ! Sulfuric Acid
       REAL         HCL    ! Hydrogen Chloride
       REAL         GLY    ! Glyoxal
       REAL         MGLY   ! Methylglyoxal
       REAL         HO     ! OH, hydroxyl radical
       REAL         HO2    ! HO2, hydrogen Peroxyl radical
       REAL         CL2    ! CL2, chlorine gas
       REAL         HOCL   ! HOCL, Hypochlorous acid
CWTH mercury chemistry
       REAL         HG     ! Elemental Mercury
       REAL         HGCL2  ! Mercuric Chloride

! GAS Phase Chemistry Controls

C for time controls ! default values

      INTEGER BXM_STDATE    ! = 1985182  ! beginning date for this run            <
      INTEGER BXM_STTIME    ! = 120000   ! beginning hour for this run            <
      INTEGER BXM_RUNLEN    ! = 120000   ! time duration (HHMMSS) for this run    <
      INTEGER BXM_TSTEP     ! = 001500   ! output time step interval (HHMMSS)     <
      INTEGER BXM_MSTEP     ! = 000100   ! model time step interval (HHMMSS)      <
      INTEGER BXM_EMTYPE    ! = 0        ! Determines type of emission inputs

      REAL GEAR_ATOL        ! = 1.0E-12  ! absolute tolerance for SMV Gear solution
      REAL GEAR_RTOL        ! = 1.0E-06  ! relative tolerance for SMV Gear solution
      REAL RB_ATOL          ! = 1.0E-12  ! absolute tolerance for Rosenbrock solution
      REAL RB_RTOL          ! = 1.0E-06  ! relative tolerance for Rosenbrock solution

C for data selection flags ! defaults

      LOGICAL ASCII    ! = TRUE    ! Flag for ASCII input file              <
      LOGICAL IOAPI    ! = FALSE   ! Flag for Models-3 data                 <
      LOGICAL M3PHOT   ! = TRUE    ! Flag for Models-3 type photolysis      <
      LOGICAL PHZATAB  ! = FALSE   ! Flag for zenith angle phot table       <
      LOGICAL PHNONE   ! = FALSE   ! Flag for all darkness                  <

C for air conditions ! default values

      REAL BXM_TEMP     ! = 300.0, Temperature (deg K)                    <
      REAL BXM_PRES     ! = 101325.0, Pressure (Pa) 1 atm                    <
      REAL BXM_QV       ! = 0.01244, Water vapor mixing ratio (Kg/Kg air)   <
      REAL BXM_AIR      ! = 1000000.0, Air mixing (ppmV)
      REAL BXM_N2       ! = 771000.0, molecular nitrogen gas (ppmV)
      REAL BXM_O2       ! = 209000.0, molecular oxygen gas (ppmV)
      REAL BXM_H2       ! = 0.0, molecular hydrogen gas (ppmV)
      REAL BXM_CH4      ! = 2,  methane (ppmV) 
      REAL BXM_DENS     ! = 1.225, Air density (Kg/m^3)                   <
  
C for photolysis      !  default values       

       REAL  BXM_LAT      ! = 45.0  ! Latitude (deg)                         <
       REAL  BXM_LON      ! = 0     ! Longitude (deg)                        <
       REAL  BXM_HT       ! = 0.000 ! Height for J values (Km)               <
       REAL  BXM_WBAR     ! = 0.0   ! Cloud liq. H2O content (gm/m^3)        <
       REAL  BXM_CLDT     ! = 3.0   ! Cloud tops (Km)                        <
       REAL  BXM_CLDB     ! = 2.0   ! Cloud bottoms (Km)                     <
       REAL  BXM_CFRAC    ! = 0.0   ! Cloud fraction                         <


C land surface data
       REAL  BXM_OPEN     ! = 0.0 ! fraction opoen ocean
       REAL  BXM_SURF     ! = 0.0 ! fraction surf zone
       REAL  BXM_SEAICE   ! = 0.0 ! fraction sea ice
 
C for input and output files

      LOGICAL  UNIX_OS   ! .TRUE. ! flag whether unix system

      CHARACTER(LEN=656)  INDIR    ! input file directory
      CHARACTER(LEN=656)  INFILE   ! name for input file
      CHARACTER(LEN=656)  ASCII_IC ! full name for input file

      CHARACTER(LEN=656)  JTABDIR   ! JTABLE file directory
      CHARACTER(LEN=656)  JTABLE    ! name for JTABLE file
      CHARACTER(LEN=256)  XJ_DATA   ! full file name 

      CHARACTER(LEN=  3)  BXM_SAS     ! = NO     ! SAS compatible file
      CHARACTER(LEN=  3)  BXM_MACTAB  ! = YES    ! TAB deliminate ASCII file
      CHARACTER(LEN=656)  OUTDIR      ! output file directory
      CHARACTER(LEN=656)  OUTFILE     ! name for MACTAB file
      CHARACTER(LEN=656)  MACTAB_FILE ! full file name 
      CHARACTER(LEN=656)  EMIS_FILE   ! full name of emissions file

      CONTAINS

      SUBROUTINE SET_BOX_INPUTS()

        IMPLICIT NONE

        LOGICAL, SAVE :: BOX_INPUTS_SET = .FALSE.

        IF( BOX_INPUTS_SET ) RETURN
!       CALL AQCHEM_INPUTS()
        CALL CHEM_INPUTS()
        BOX_INPUTS_SET = .TRUE.
        RETURN

      END SUBROUTINE SET_BOX_INPUTS
      SUBROUTINE AQCHEM_INPUTS()

         USE SCENE_DATA

         IMPLICIT NONE

          CHARACTER(LEN =  32) :: INPUT_FILE = 'aqchem_inputs.dat'
          INTEGER              :: IN_UNIT    =  20
          INTEGER              :: IOST                ! io status


          LOGICAL             :: EXIST_STATUS = .FALSE.

          NAMELIST /CONTROL_LIST/ WHICH_AQCHEM, LATITUDE, LONGITUDE,
     &                            CALENDAR_DATE, GREENWICH_TIME

          NAMELIST /ATMOSPHERE/ CLOUD_THINKESS, SCAV_AKN_NUM, SCAV_AKN_SRF, SCAV_AKN_MAS,
     &                          PRECIP_RATE, PRESSURE, DT_CLOUD, TEMPERATURE,  
     &                          WATER_CONTENT, WATER_TOTAL 

          NAMELIST / GASES / SO2, HNO3, N2O5, CO2, NH3, H2O2, O3, FOA,
     &                       MHP, PAA, H2SO4, HCL, GLY, MGLY, HO, HO2, 
     &                       CL2, HOCL, HG, HGCL2

          NAMELIST /AEROSOLS/ SO4AKN, SO4ACC, SO4COR,
     &                        NH4AKN, NH4ACC, NH4COR,
     &                        NO3AKN, NO3ACC, NO3COR,
     &                        NAAKN, NAACC, NACOR,
     &                        CLAKN, CLACC, CLCOR,
     &                        ECAKN, ECACC,
     &                        NUMAKN, NUMACC, NUMCOR,
     &                        SRFAKN, SRFACC, SRFCOR

          WHICH_AQCHEM   = 'HG_VERSION'
          LATITUDE       = 40.0     ! lat(col,row)
          LONGITUDE      = 75.0     ! lon(col,row)
          CALENDAR_DATE  = 20010706 ! YEAR * 10000 + MONTH * 100 + DAY
          GREENWICH_TIME = 17.00    ! float(ihr) + imin/60. + isec/3600.     

          CLOUD_THINKESS = 1000.0
          SCAV_AKN_NUM     = 0.0
          SCAV_AKN_SRF     = 0.0
          SCAV_AKN_MAS     = 0.0
          PRECIP_RATE    = 0.0
          PRESSURE       = 101325.0
          DT_CLOUD       = 3600.0
          TEMPERATURE    = 283.0
          WATER_CONTENT  = 0.2
          WATER_TOTAL    = 0.2

          SO2   = 0.008
          HNO3  = 0.1414424E-4
          N2O5  = 0.1567967E-5
          CO2   = 340.0
          NH3   = 0.5479954E-3
          H2O2  = 0.01
          O3    = 0.03
          FOA   = 0.0
          MHP   = 0.0 
          PAA   = 0.0
          H2SO4 = 8.55687E-3
          HCL   = 0.0 ! 1.0E-6
          GLY   = 0.10E-5
          MGLY  = 1.0E-4
          HO    = 1.0E-8
          HO2   = 1.0E-6
          CL2   = 1.0E-6
          HOCL  = 0.0
          HG    = 0.0
          HGCL2 = 0.0

          SO4AKN = 0.0
          SO4ACC = 0.0
          SO4COR = 0.0
          NH4AKN = 0.0
          NH4ACC = 6.65
          NH4COR = 0.0
          NO3AKN = 0.0
          NO3ACC = 10.0
          NO3COR = 0.0
          NAAKN = 0.0
          NAACC = 0.0
          NACOR = 0.0  ! 1.71
          CLAKN = 0.0
          CLACC = 0.0
          CLCOR = 0.0  ! 1.71
          ECAKN = 0.0
          ECACC = 0.0
          NUMAKN = 0.0
          NUMACC = 0.0
          NUMCOR = 0.0
          SRFAKN = 0.0
          SRFACC = 0.0
          SRFCOR = 0.0


          INQUIRE( FILE = TRIM( INPUT_FILE ), EXIST = EXIST_STATUS )
          IF ( .NOT. EXIST_STATUS ) THEN
            WRITE(6,'(A)')'The input file, BOX_DATA, not found at:'
            WRITE(6,'(A)')TRIM( INPUT_FILE )
            STOP
          END IF 
          OPEN(NEWUNIT = IN_UNIT,FILE = INPUT_FILE, FORM ='FORMATTED')

          READ(IN_UNIT, NML=CONTROL_LIST)

          READ(IN_UNIT, NML=ATMOSPHERE)

          READ(IN_UNIT, NML=GASES)

          READ(IN_UNIT, NML=AEROSOLS)


          WRITE(6, NML=CONTROL_LIST)

          WRITE(6, NML=ATMOSPHERE)

          WRITE(6, NML=GASES)

          WRITE(6, NML=AEROSOLS)
           
          CLOSE(IN_UNIT)

          RETURN

      END SUBROUTINE AQCHEM_INPUTS

      SUBROUTINE CHEM_INPUTS()

          USE SCENE_DATA

          IMPLICIT NONE

          CHARACTER(LEN =  16) :: BOX_DATA     = 'BOX_DATA'
          CHARACTER(LEN = 999) :: INPUT_FILE  != 'gc_chem_inputs.dat'
          CHARACTER(LEN = 999) :: FILE_LINE
          INTEGER              :: IN_UNIT    =  20
          INTEGER              :: IOST                ! io status

          INTEGER             :: I, J, K

          INTEGER, PARAMETER  :: SLASH  = 47 ! ASCII code for text
          INTEGER, PARAMETER  :: BSLASH = 92 ! ASCII code for text
          LOGICAL             :: EXIST_STATUS = .FALSE.




          NAMELIST /TIME_CONTROLS/  BXM_STDATE, BXM_STTIME, BXM_RUNLEN, 
     &                              BXM_TSTEP,  BXM_MSTEP,   
     &                              GEAR_ATOL, GEAR_RTOL,
     &                              RB_ATOL, RB_RTOL

          NAMELIST /DATA_CONTROLS/  ASCII, IOAPI, M3PHOT, PHZATAB, PHNONE,
     &                              BXM_EMTYPE

          NAMELIST /AIR_CONTROLS/   BXM_TEMP, BXM_PRES, BXM_QV, BXM_AIR,
     &                              BXM_N2,   BXM_O2,   BXM_H2, BXM_CH4,
     &                              BXM_DENS

          NAMELIST /SURFACE_CONTROLS/ BXM_OPEN, BXM_SURF, BXM_SEAICE

          NAMELIST /PHOT_CONTROLS/  BXM_LAT,  BXM_LON,  BXM_HT, BXM_WBAR, 
     &                              BXM_CLDT, BXM_CLDB, BXM_CFRAC    

          NAMELIST /FILE_CONTROLS/  UNIX_OS, INDIR,   INFILE,     JTABDIR,
     &                              JTABLE,  BXM_SAS, BXM_MACTAB, OUTDIR,
     &                              OUTFILE, EMIS_FILE

          CALL NAMEVAL( BOX_DATA, INPUT_FILE)

          INQUIRE( FILE = TRIM( INPUT_FILE ), EXIST = EXIST_STATUS )
          WRITE(6,'(A)')TRIM( INPUT_FILE )
          IF ( .NOT. EXIST_STATUS ) THEN
            WRITE(6,'(A)')'The input file, BOX_DATA, not found:'
            STOP
          END IF 

          OPEN(NEWUNIT = IN_UNIT,FILE = INPUT_FILE, FORM ='FORMATTED', STATUS='OLD' )
           BXM_STDATE   = 1985182     
           BXM_STTIME   = 120000      
           BXM_RUNLEN   = 120000     
           BXM_TSTEP    = 001500    
           BXM_MSTEP    = 000100
           GEAR_ATOL    = 1.0E-09    
           GEAR_RTOL    = 1.0E-03
           RB_ATOL      = 1.0E-07    
           RB_RTOL      = 1.0E-03

           ASCII    = .TRUE.       
           IOAPI    = .FALSE.    
           M3PHOT   = .TRUE.      
           PHZATAB  = .FALSE.       
           PHNONE   = .FALSE.     
           BXM_EMTYPE   = 0

           BXM_TEMP     = 300.0      ! Temperature (deg K)                    <
           BXM_PRES     = 101325.0    ! Pressure (Pa) 1 atm                    <
           BXM_QV       = 0.01244     ! Water vapor mixing ratio (Kg/Kg air)   <
           BXM_AIR      = 1000000.0   ! Air mixing (ppmV)
           BXM_N2       = 771000.0    ! molecular nitrogen gas (ppmV)
           BXM_O2       = 209000.0    ! molecular oxygen gas (ppmV)
           BXM_H2       = 0.0         ! molecular hydrogen gas (ppmV)
           BXM_CH4      = 2           ! methane (ppmV) 
           BXM_DENS     = 1.225       ! Air density (Kg/m^3)                   <

           BXM_LAT      = 45.0        ! Latitude (deg)                         <
           BXM_LON      = 0           ! Longitude (deg)                        <
           BXM_HT       = 0.000       ! Height for J values (Km)               <
           BXM_WBAR     = 0.0         ! Cloud liq. H2O content (gm/m^3)        <
           BXM_CLDT     = 3.0         ! Cloud tops (Km)                        <
           BXM_CLDB     = 2.0         ! Cloud bottoms (Km)                     <
           BXM_CFRAC    = 0.0         ! Cloud fraction                         <

           BXM_OPEN   = 0.0
           BXM_SURF   = 0.0
           BXM_SEAICE = 0.0

           UNIX_OS      = .TRUE.
           INDIR        = ''
           ASCII_IC     = 'ASCII_IC'
           JTABDIR      = ''
           JTABLE       = 'JTABLE'
           OUTDIR       = ''
           OUTFILE      = 'OUTFILE'
           BXM_SAS      = 'NO'
           BXM_MACTAB   = 'NO'
           INFILE       = 'BOX_IC_FILE'
           EMIS_FILE    = 'EMIS_DATA'

           FILE_LINE    = ''



          I = 0
          Do 
            I = I + 1
            Read(IN_UNIT,*,IOSTAT=IOST)FILE_LINE
            If( IOST .NE. 0 )Exit
            Write(6,'(a)')Trim(FILE_LINE)
          End Do

          REWIND(IN_UNIT)

          READ(IN_UNIT, NML=TIME_CONTROLS,IOSTAT=IOST)
          If( IOST .NE. 0 )Then
            Write(6,'(a)')'Error Reading TIME_CONTROLS namelist'
            Write(6,'(a)')'Using Below Default Values'
          End If

          WRITE(6, NML=TIME_CONTROLS)

          READ(IN_UNIT, NML=AIR_CONTROLS,IOSTAT=IOST)
          If( IOST .NE. 0 )Then
            Write(6,'(a)')'Error Reading AIR_CONTROLS namelist'
            Write(6,'(a)')'Using Below Default Values'
          End If

          WRITE(6, NML=AIR_CONTROLS)

          READ(IN_UNIT, NML=PHOT_CONTROLS,IOSTAT=IOST)
          If( IOST .NE. 0 )Then
            Write(6,'(a)')'Error Reading PHOT_CONTROLS namelist'
            Write(6,'(a)')'Using Below Default Values'
          End If

          WRITE(6, NML=PHOT_CONTROLS,IOSTAT=IOST)

          READ(IN_UNIT, NML=FILE_CONTROLS,IOSTAT=IOST)
          If( IOST .NE. 0 )Then
            Write(6,'(a)')'Error Reading FILE_CONTROLS namelist'
            Write(6,'(a)')'Using Below Default Values'
          End If

          WRITE(6, NML=FILE_CONTROLS,IOSTAT=IOST)

          READ(IN_UNIT, NML=SURFACE_CONTROLS,IOSTAT=IOST)
          If( IOST .NE. 0 )Then
            Write(6,'(a)')'Error Reading SURFACE_CONTROLS namelist'
            Write(6,'(a)')'Using Below Default Values'
          End If
          WRITE(6, NML=SURFACE_CONTROLS)

          IF(UNIX_OS)THEN
             J = BSLASH
             K = SLASH
          ELSE
             J = SLASH
             K = BSLASH
          ENDIF
          DO I = 1, LEN_TRIM(INDIR)
             IF(ICHAR(INDIR(I:I))   .EQ. J)INDIR(I:I)   = ACHAR(K)
          ENDDO
          DO I = 1, LEN_TRIM(JTABDIR)
             IF(ICHAR(JTABDIR(I:I)) .EQ. J)JTABDIR(I:I) = ACHAR(K)
          ENDDO
          DO I = 1, LEN_TRIM(OUTDIR)
             IF(ICHAR(OUTDIR(I:I))  .EQ. J)OUTDIR(I:I)  = ACHAR(K)
          ENDDO

!          ASCII_IC      = INDIR(  1:LEN_TRIM(INDIR)) 
!     &                 // ACHAR(K) // INFILE
!          XJ_DATA       = JTABDIR(1:LEN_TRIM(JTABDIR)) 
!     &                 // ACHAR(K) // JTABLE
!          MACTAB_FILE   = OUTDIR( 1:LEN_TRIM(OUTDIR))  
!     &                 // ACHAR(K) // OUTFILE

          CLOSE(IN_UNIT)

          ! 'Air Temperature (K)'
          CELL_TEMP = BXM_TEMP
  
          ! 'Pressure (Pa)'
          CELL_PRES = BXM_PRES
  
          ! 'Qv (Kg/Kg)'
          QV = BXM_QV
  
          ! 'Density (Kg/m3)'
          DENS = BXM_DENS
  
          ! Set Jacobian scaled by map scaled factor for geometric height coordinated
          JACOBF = 1.0
          JACOBM = 1.0

          ! 'J-weighted air Density (Kg/m2)'
          DENS_J = BXM_DENS * JACOBM
  
          MH2O = QV * ( 28.9644 / 18.0153 ) * 1.0E+06
  
          ! 'Mixing ratio of Air (ppmV)'
          CELL_AIR = BXM_AIR
  
          ! 'Mixing ratio of molecular oxygen (ppmV)'
          CELL_O2  = BXM_O2
  
          ! 'Mixing ratio of molecular hydrogen (ppmV)'
          CELL_H2 =  BXM_H2
  
          ! 'Mixing ratio of methane (ppmV)'
          CELL_CH4 = BXM_CH4
  
          ! 'Mixing ratio of molecular nitrogen (ppmV)'
          CELL_N2 = BXM_N2
  
          ! 'Cell longitude (Degrees)'
          LON = BXM_LON
          ! 'Cell latitude (Degrees)'
          LAT = BXM_LAT
          ! 'Height at middle of Cell (m)'
          HT  = BXM_HT

          CALL NAMEVAL( INFILE,  ASCII_IC )
!         CALL NAMEVAL( JTABLE,  XJ_DATA )
          CALL NAMEVAL( OUTFILE, MACTAB_FILE )

          RETURN

      END SUBROUTINE CHEM_INPUTS

      END MODULE DRIVER_INPUTS

