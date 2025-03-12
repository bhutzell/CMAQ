
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

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE ELMO_DATA

      USE AERO_DATA, ONLY : N_MODE, IM_ALL, IM_UF, IM_F, IM_C, IM_AIT,
     &                      IM_ACC, IM_NUC
      USE SOA_DEFN, ONLY  : IOA_PRI, IOA_SEC, IOA_ANTSEC, IOA_BIOSEC
      USE UTILIO_DEFN

      IMPLICIT NONE

#ifdef mpas
      INTEGER, PARAMETER :: M3REAL  =   5
#endif

      ! Define Logical values as T and F for the aerospc table
      Logical, Parameter, Private :: T = .true.
      Logical, Parameter, Private :: F = .false.

      LOGICAL, SAVE :: L_ELMO
      INTEGER, SAVE :: ELMO_NSTEP = 0
 
      ! Define ELMO Variable Types
      INTEGER, PARAMETER :: ET_CONC  = 1, ET_WDEP      = 2, ET_DDEP = 3, 
     &                      ET_CMPST = 4, ET_CMPST_DEP = 5, ET_DRVD = 6,
     &                      ET_DRVD_DDEP = 7, ET_DRVD_WDEP = 8,
     &                      ET_ISAM_CONC = 11, ET_ISAM_WDEP = 12, 
     &                      ET_ISAM_DDEP = 13, ET_ISAM_CMPST= 14, 
     &                      ET_ISAM_CMPST_DEP = 15, ET_ISAM_DRVD = 16, 
     &                      ET_ISAM_DRVD_DDEP = 17, ET_ISAM_DRVD_WDEP = 18,
     &                      ET_DDM_CONC = 21, ET_DDM_WDEP = 22, 
     &                      ET_DDM_DDEP = 23, ET_DDM_CMPST= 24, 
     &                      ET_DDM_CMPST_DEP = 25, ET_DDM_DRVD = 26, 
     &                      ET_DDM_DRVD_DDEP = 27, ET_DDM_DRVD_WDEP = 28,
     &                      ET_AEROPROP = 30, ET_CHEM = 31, ET_MET = 32,
     &                      ET_PHOT = 33

      CHARACTER( 14 ), PARAMETER :: CETYPE(33) = 
     &    (/'CONC          ','WET DEP       ','DRY DEP       ',
     &      'COMPOSITE CONC','COMPOSITE DEP ','DERIVED CONC  ',
     &      'DERIVED DDEP  ','DERIVED WDEP  ','              ',
     &      '              ','ISAM CONC     ','ISAM WETDEP   ',
     &      'ISAM DRY DEP  ','ISAM COMP CONC','ISAM COMP DEP ',
     &      'ISAM DERIVED  ','ISAM DRVD DDEP','ISAM DRVD WDEP',
     &      '              ','              ','DDM CONC      ',
     &      'DDM WET DEP   ','DDM DRY DEP   ','DDM COMP CONC ',
     &      'DDM COMP DEP  ','DDM DERIVED   ','DDM DRVD DDEP ',
     &      'DDM DRVD WDEP ','              ','AERO PROPERTY ',
     &      'CHEMISTRY     ','METEOROLOGY   ','PHOT          '/) 

      ! Define Indices for All Variables on the ELMO List
      ! The idea here is that these variables will be organized on
      ! the ELMO_TABLE in the order of this integer list, not the 
      ! ELMO_LIST. ELMO_LIST is searchable via the Parameter names 
      ! but they link to this list of integers so that each parameter 
      ! can be easily referenced during the run without having to do 
      ! string lookups. Please see the table defining the ELMO_LIST 
      ! below for a description of each of these indices.
      INTEGER, PARAMETER :: ID_STDEV =        1
      INTEGER, PARAMETER :: ID_DRY_DG =       2
      INTEGER, PARAMETER :: ID_WET_DG =       3 
      INTEGER, PARAMETER :: ID_WET_M2 =       4
      INTEGER, PARAMETER :: ID_DRY_M3 =       5
      INTEGER, PARAMETER :: ID_WET_M3 =       6
      INTEGER, PARAMETER :: ID_PM_NUM =       7
      INTEGER, PARAMETER :: ID_PMU_NUM =      8
      INTEGER, PARAMETER :: ID_PMF_NUM =      9
      INTEGER, PARAMETER :: ID_PMC_NUM =     10
      INTEGER, PARAMETER :: ID_N10 =         11
      INTEGER, PARAMETER :: ID_N20 =         12
      INTEGER, PARAMETER :: ID_N40 =         13
      INTEGER, PARAMETER :: ID_N100 =        14
      INTEGER, PARAMETER :: ID_PM_SRF =      15
      INTEGER, PARAMETER :: ID_PMU_SRF =     16
      INTEGER, PARAMETER :: ID_PMF_SRF =     17
      INTEGER, PARAMETER :: ID_PMC_SRF =     18
      INTEGER, PARAMETER :: ID_PM =          19
      INTEGER, PARAMETER :: ID_PMU =         20
      INTEGER, PARAMETER :: ID_PMF =         21
      INTEGER, PARAMETER :: ID_PMC =         22
      INTEGER, PARAMETER :: ID_PMNUC =       23
      INTEGER, PARAMETER :: ID_PMAIT =       24
      INTEGER, PARAMETER :: ID_PMACC =       25
      INTEGER, PARAMETER :: ID_DRY_DENS =    26
      INTEGER, PARAMETER :: ID_WET_DENS =    27
      INTEGER, PARAMETER :: ID_FPM01 =       28
      INTEGER, PARAMETER :: ID_FPM1 =        29
      INTEGER, PARAMETER :: ID_FPM25 =       30
      INTEGER, PARAMETER :: ID_FPM10 =       31
      INTEGER, PARAMETER :: ID_FPM25to10 =   32
      INTEGER, PARAMETER :: ID_FAMS =        33
      INTEGER, PARAMETER :: ID_PM01 =        34
      INTEGER, PARAMETER :: ID_PM1 =         35
      INTEGER, PARAMETER :: ID_PM25 =        36
      INTEGER, PARAMETER :: ID_PM10 =        37
      INTEGER, PARAMETER :: ID_PM25to10 =    38
      INTEGER, PARAMETER :: ID_PMAMS =       39
      INTEGER, PARAMETER :: ID_PMF_OC =      40
      INTEGER, PARAMETER :: ID_PMF_OA =      41
      INTEGER, PARAMETER :: ID_PMF_HPM =     42               
      INTEGER, PARAMETER :: ID_PMF_PH =      43               
      INTEGER, PARAMETER :: ID_PMF_POC =     44               
      INTEGER, PARAMETER :: ID_PMF_SOC =     45
      INTEGER, PARAMETER :: ID_PMF_POA =     46
      INTEGER, PARAMETER :: ID_PMF_SOA =     47
      INTEGER, PARAMETER :: ID_PMF_OMOC =    48
      INTEGER, PARAMETER :: ID_PMF_OtoC =    49
      INTEGER, PARAMETER :: ID_PMF_ASOA =    50
      INTEGER, PARAMETER :: ID_PMF_BSOA =    51
      INTEGER, PARAMETER :: ID_AMS_OA =      52
      INTEGER, PARAMETER :: ID_AMS_OTOC =    53
      INTEGER, PARAMETER :: ID_PM1_OC =      54
      INTEGER, PARAMETER :: ID_PM1_OA =      55
      INTEGER, PARAMETER :: ID_PM25_OC =     56
      INTEGER, PARAMETER :: ID_PM25_OA =     57
      INTEGER, PARAMETER :: ID_BENAPY_FAERO= 58
      INTEGER, PARAMETER :: ID_PM25_FRM =    59                   
      INTEGER, PARAMETER :: ID_PMF_FRM =     60                   
      INTEGER, PARAMETER :: ID_RH =          61
      INTEGER, PARAMETER :: ID_TEMP =        62
      INTEGER, PARAMETER :: ID_SRFTEMP =     63
      INTEGER, PARAMETER :: ID_PRES =        64
      INTEGER, PARAMETER :: ID_GN2O5F =      65
      INTEGER, PARAMETER :: ID_GN2O5C =      66
      INTEGER, PARAMETER :: ID_YCLNO2F =     67
      INTEGER, PARAMETER :: ID_YCLNO2C =     68
      INTEGER, PARAMETER :: ID_GIEPOX =      69
      INTEGER, PARAMETER :: ID_KIEPOX =      70
      INTEGER, PARAMETER :: ID_GIMAE =       71
      INTEGER, PARAMETER :: ID_VOC_NOX =     72
      INTEGER, PARAMETER :: ID_EF_HNO3 =     73
      INTEGER, PARAMETER :: ID_DZ    =       74
      INTEGER, PARAMETER :: ID_ZH    =       75
      INTEGER, PARAMETER :: ID_CFRAC =       76
      INTEGER, PARAMETER :: ID_PV    =       77
      INTEGER, PARAMETER :: ID_DENS  =       78
      INTEGER, PARAMETER :: ID_RHOJ  =       79
      INTEGER, PARAMETER :: ID_NO2COL  =     80
      INTEGER, PARAMETER :: ID_SO2COL  =     81
      INTEGER, PARAMETER :: ID_HCHOCOL  =    82
      INTEGER, PARAMETER :: ID_COCOL  =      83
      INTEGER, PARAMETER :: ID_KZ     =      84
      INTEGER, PARAMETER :: ID_KZMIN  =      85
      INTEGER, PARAMETER :: ID_AOD550 =      86
      INTEGER, PARAMETER :: ID_PMEXT550 =    87
      INTEGER, PARAMETER :: ID_PBL =         88
      INTEGER, PARAMETER :: ID_RGRND =       89
      INTEGER, PARAMETER :: ID_PRECIP =      90
      INTEGER, PARAMETER :: ID_WSPD10 =      91
      INTEGER, PARAMETER :: ID_WDIR10 =      92
      INTEGER, PARAMETER :: ID_WVEL   =      93
      INTEGER, PARAMETER :: ID_DD_OA =       94
      INTEGER, PARAMETER :: ID_WD_OA =       95
      INTEGER, PARAMETER :: ID_DD_OC =       96
      INTEGER, PARAMETER :: ID_WD_OC =       97
      INTEGER, PARAMETER :: ID_DD_POA =      98
      INTEGER, PARAMETER :: ID_WD_POA =      99
      INTEGER, PARAMETER :: ID_DD_POC =     100
      INTEGER, PARAMETER :: ID_WD_POC =     101
      INTEGER, PARAMETER :: ID_DD_SOA =     102
      INTEGER, PARAMETER :: ID_WD_SOA =     103
      INTEGER, PARAMETER :: ID_DD_SOC =     104
      INTEGER, PARAMETER :: ID_WD_SOC =     105

      TYPE ELDTP
        CHARACTER (LEN = 16) :: NAME      !Parameter Name
        INTEGER              :: ID        !ID Number in Case Statement
        INTEGER              :: VAR_TYPE  !ELMO Variable Type (e.g. ET_CONC, ET_WDEP, etc.)
        LOGICAL              :: L_MODE    !Is this property mode-expansive?
        CHARACTER (LEN = 16) :: UNIT      !Units for Parameter
        CHARACTER (LEN = 80) :: DESC      !Description
      END TYPE ELDTP

      INTEGER, PARAMETER :: N_ELMO_LIST = 105

      TYPE (ELDTP), PARAMETER :: ELMO_LIST( N_ELMO_LIST ) = (/

      ! Aerosol Parameters and Diagnostics
     &ELDTP( 'STDEV        ',ID_STDEV,      ET_AEROPROP, T, '      ', 'Mode standard deviation (dry and wet)' ),
     &ELDTP( 'DRY_DG       ',ID_DRY_DG,     ET_AEROPROP, T, 'um    ', 'Mode mean diameter (dry)' ),
     &ELDTP( 'WET_DG       ',ID_WET_DG,     ET_AEROPROP, T, 'um    ', 'Mode mean diameter (wet)' ),
     &ELDTP( 'WET_M2       ',ID_WET_M2,     ET_AEROPROP, T, 'm2 m-3', 'Mode 2nd moment (wet)' ),
     &ELDTP( 'DRY_M3       ',ID_DRY_M3,     ET_AEROPROP, T, 'm3 m-3', 'Mode 3rd moment (dry)' ),
     &ELDTP( 'WET_M3       ',ID_WET_M3,     ET_AEROPROP, T, 'm3 m-3', 'Mode 3rd moment (wet)' ),
     &ELDTP( 'TSP_NUM      ',ID_PM_NUM,     ET_AEROPROP, F, 'N m-3' , 'Total Particle Number' ),
     &ELDTP( 'PMU_NUM      ',ID_PMU_NUM,    ET_AEROPROP, F, 'N m-3' , 'Ultrafine-Mode Particle Number' ),
     &ELDTP( 'PMF_NUM      ',ID_PMF_NUM,    ET_AEROPROP, F, 'N m-3' , 'Fine-Mode Particle Number' ),
     &ELDTP( 'PMC_NUM      ',ID_PMC_NUM,    ET_AEROPROP, F, 'N m-3' , 'Coarse-Mode Particle Number' ),
     &ELDTP( 'N10          ',ID_N10,        ET_AEROPROP, F, 'N m-3' , 'Total Particle Number greater than 10 nm' ),
     &ELDTP( 'N20          ',ID_N20,        ET_AEROPROP, F, 'N m-3' , 'Total Particle Number greater than 20 nm' ),
     &ELDTP( 'N40          ',ID_N40,        ET_AEROPROP, F, 'N m-3' , 'Total Particle Number greater than 40 nm' ),
     &ELDTP( 'N100         ',ID_N100,       ET_AEROPROP, F, 'N m-3' , 'Total Particle Number greater than 100 nm' ),
     &ELDTP( 'TSP_SRF      ',ID_PM_SRF,     ET_AEROPROP, F, 'm2 m-3', 'Particle Surface Area' ),
     &ELDTP( 'PMU_SRF      ',ID_PMU_SRF,    ET_AEROPROP, F, 'm2 m-3', 'Ultrafine-Mode Surface Area' ),
     &ELDTP( 'PMF_SRF      ',ID_PMF_SRF,    ET_AEROPROP, F, 'm2 m-3', 'Fine-Mode Surface Area' ),
     &ELDTP( 'PMC_SRF      ',ID_PMC_SRF,    ET_AEROPROP, F, 'm2 m-3', 'Coarse-Mode Surface Area' ),
     &ELDTP( 'TSP          ',ID_PM,         ET_DRVD,     F, 'ug m-3', 'Total Particle Mass' ),
     &ELDTP( 'PMU          ',ID_PMU,        ET_DRVD,     F, 'ug m-3', 'Ultrafine Particle Mass' ),
     &ELDTP( 'PMF          ',ID_PMF,        ET_DRVD,     F, 'ug m-3', 'Fine Particle Mass' ),
     &ELDTP( 'PMC          ',ID_PMC,        ET_DRVD,     F, 'ug m-3', 'Coarse Particle Mass' ),
     &ELDTP( 'PMNUC        ',ID_PMNUC,      ET_DRVD,     F, 'ug m-3', 'Nucleation Mode Total PM' ),
     &ELDTP( 'PMAIT        ',ID_PMAIT,      ET_DRVD,     F, 'ug m-3', 'Aitken Mode Total PM' ),
     &ELDTP( 'PMACC        ',ID_PMACC,      ET_DRVD,     F, 'ug m-3', 'Accumulation Mode Total PM' ),
     &ELDTP( 'DRY_DENS     ',ID_DRY_DENS,   ET_AEROPROP, T, 'kg m-3', 'Bulk Density of Particles excluding particle water' ),
     &ELDTP( 'WET_DENS     ',ID_WET_DENS,   ET_AEROPROP, T, 'kg m-3', 'Bulk Density of Particles including particle water' ),
     &ELDTP( 'FPM01        ',ID_FPM01,      ET_AEROPROP, T, '1     ', 'PM01 fraction of mode' ),
     &ELDTP( 'FPM1         ',ID_FPM1,       ET_AEROPROP, T, '1     ', 'PM1 fraction of mode' ),
     &ELDTP( 'FPM25        ',ID_FPM25,      ET_AEROPROP, T, '1     ', 'PM2.5 fraction of mode' ),
     &ELDTP( 'FPM10        ',ID_FPM10,      ET_AEROPROP, T, '1     ', 'PM10 fraction of mode' ),
     &ELDTP( 'FPM25TO10    ',ID_FPM25to10,  ET_AEROPROP, T, '1     ', 'PM2.5-10 fraction of mode' ),
     &ELDTP( 'FAMS         ',ID_FAMS,       ET_AEROPROP, T, '1     ', 'AMS Transmission Fraction of mode' ),
     &ELDTP( 'PM01         ',ID_PM01,       ET_DRVD,     F, 'ug m-3', 'Bulk PM0.1 Concentration' ), 
     &ELDTP( 'PM1          ',ID_PM1,        ET_DRVD,     F, 'ug m-3', 'Bulk PM1.0 Concentration' ), 
     &ELDTP( 'PM25         ',ID_PM25,       ET_DRVD,     F, 'ug m-3', 'Bulk PM2.5 Concentration' ), 
     &ELDTP( 'PM10         ',ID_PM10,       ET_DRVD,     F, 'ug m-3', 'Bulk PM10.0 Concentration' ),
     &ELDTP( 'PM25TO10     ',ID_PM25to10,   ET_DRVD,     F, 'ug m-3', 'Coarse-Mode Total' ),
     &ELDTP( 'PMAMS        ',ID_PMAMS,      ET_DRVD,     F, 'ug m-3', 'Bulk PM Concentration in AMS' ),
                                                          
      ! Fine-Mode PM Species                         
     &ELDTP( 'PMF_OA       ',ID_PMF_OA,     ET_DRVD,     F, 'ug m-3', 'Fine-Mode Organic Aerosol' ),
     &ELDTP( 'PMF_OC       ',ID_PMF_OC,     ET_DRVD,     F, 'ug m-3', 'Fine-Mode Organic Carbon (excludes non-carbon atoms)' ),
     &ELDTP( 'PMF_HPMOLAL  ',ID_PMF_HPM,    ET_AEROPROP, F, 'mol kg-1','Fine-Mode Concentration of H+ in Particle Water' ),
     &ELDTP( 'PMF_PH       ',ID_PMF_PH,     ET_AEROPROP, F, '1'     , 'Fine-Mode pH' ),
                                                  
      ! Organic Aerosol Relevant Aggregates          
     &ELDTP( 'PMF_POA      ',ID_PMF_POA,    ET_DRVD,     F, 'ug m-3', 'Fine-Mode Primary Organic Aerosol' ),
     &ELDTP( 'PMF_POC      ',ID_PMF_POC,    ET_DRVD,     F, 'ug m-3', 'Fine-Mode Primary Organic Carbon (excluding non-carbon)' ),
     &ELDTP( 'PMF_SOA      ',ID_PMF_SOA,    ET_DRVD,     F, 'ug m-3', 'Fine-Mode Secondary Organic Aerosol' ),
     &ELDTP( 'PMF_SOC      ',ID_PMF_SOC,    ET_DRVD,     F, 'ug m-3', 'Fine-Mode Secondary Organic Carbon (excluding non-carbon)' ),
     &ELDTP( 'PMF_ASOA     ',ID_PMF_ASOA,   ET_DRVD,     F, 'ug m-3', 'Fine-Mode Anthropogenic-VOC Derived OA' ),
     &ELDTP( 'PMF_BSOA     ',ID_PMF_BSOA,   ET_DRVD,     F, 'ug m-3', 'Fine-Mode Biogenic-VOC Derived OA' ),
     &ELDTP( 'PMF_OMOC     ',ID_PMF_OMOC,   ET_AEROPROP, F, '1     ', 'Fine-Mode OM/OC' ),
     &ELDTP( 'PMF_OTOC     ',ID_PMF_OtoC,   ET_AEROPROP, F, '1     ', 'Fine-Mode O:C' ),
                                                  
      ! AMS Speciation                            
     &ELDTP( 'PMAMS_OA     ',ID_AMS_OA,     ET_DRVD,     F, 'ug m-3', 'Organic Aerosol Collected by the AMS' ),
     &ELDTP( 'PMAMS_OTOC   ',ID_AMS_OTOC,   ET_AEROPROP, F, 'ug ug-1','O:C Collected and Measured by the AMS' ),
                                                  
      ! PM1 Speciation                            
     &ELDTP( 'PM1_OA       ',ID_PM1_OA,     ET_DRVD,     F, 'ug m-3', 'PM1 Organic Aerosol' ),
     &ELDTP( 'PM1_OC       ',ID_PM1_OC,     ET_DRVD,     F, 'ug m-3', 'PM1 Organic Carbon (excluding non-carbon atoms)' ),
                                                  
      ! PM2.5 Speciation                          
     &ELDTP( 'PM25_OA      ',ID_PM25_OA,    ET_DRVD,     F, 'ug m-3', 'PM2.5 Organic Aerosol' ),
     &ELDTP( 'PM25_OC      ',ID_PM25_OC,    ET_DRVD,     F, 'ug m-3', 'PM2.5 Organic Carbon (excluding non-carbon atoms)' ),
     &ELDTP( 'BENAPY_FAERO ',ID_BENAPY_FAERO,ET_AEROPROP,F, '',       'Benzo-a-Pyrene Aerosol-Phase Fraction' ),
     
      ! Deposition of Organic Classes
     &ELDTP( 'DD_PMF_OA    ',ID_DD_OA,      ET_DRVD_DDEP,F, 'kg ha-1','Dry Deposition of Organic Aerosol' ),
     &ELDTP( 'WD_PMF_OA    ',ID_WD_OA,      ET_DRVD_WDEP,F, 'kg ha-1','Wet Deposition of Organic Aerosol' ),
     &ELDTP( 'DD_PMF_OC    ',ID_DD_OC,      ET_DRVD_DDEP,F, 'kg ha-1','Dry Deposition of Organic Carbon' ),
     &ELDTP( 'WD_PMF_OC    ',ID_WD_OC,      ET_DRVD_WDEP,F, 'kg ha-1','Wet Deposition of Organic Carbon' ),
     &ELDTP( 'DD_PMF_POA   ',ID_DD_POA,     ET_DRVD_DDEP,F, 'kg ha-1','Dry Deposition of Primary Organic Aerosol' ),
     &ELDTP( 'WD_PMF_POA   ',ID_WD_POA,     ET_DRVD_WDEP,F, 'kg ha-1','Wet Deposition of Primary Organic Aerosol' ),
     &ELDTP( 'DD_PMF_POC   ',ID_DD_POC,     ET_DRVD_DDEP,F, 'kg ha-1','Dry Deposition of Primary Organic Carbon' ),
     &ELDTP( 'WD_PMF_POC   ',ID_WD_POC,     ET_DRVD_WDEP,F, 'kg ha-1','Wet Deposition of Primary Organic Carbon' ),
     &ELDTP( 'DD_PMF_SOA   ',ID_DD_SOA,     ET_DRVD_DDEP,F, 'kg ha-1','Dry Deposition of Secondary Organic Aerosol' ),
     &ELDTP( 'WD_PMF_SOA   ',ID_WD_SOA,     ET_DRVD_WDEP,F, 'kg ha-1','Wet Deposition of Secondary Organic Aerosol' ),
     &ELDTP( 'DD_PMF_SOC   ',ID_DD_SOC,     ET_DRVD_DDEP,F, 'kg ha-1','Dry Deposition of Secondary Organic Carbon' ),
     &ELDTP( 'WD_PMF_SOC   ',ID_WD_SOC,     ET_DRVD_WDEP,F, 'kg ha-1','Wet Deposition of Secondary Organic Carbon' ),

      ! Spectral Variables and diagnostics
     &ELDTP( 'AOD_550      ',ID_AOD550,     ET_PHOT,     F, '1     ', 'Aerosol Optical Depth at 550 nm -Angstrom interp' ),
     &ELDTP( 'PM_EXT_550   ',ID_PMEXT550,   ET_PHOT,     F, 'Mm-1  ', 'Aerosol Extinction at 550 nm -Angstrom interp' ),
     &ELDTP( 'NO2_COLUMN   ',ID_NO2COL,     ET_PHOT,     F, 'Pmolec cm-2', 'NO2 column density' ),
     &ELDTP( 'SO2_COLUMN   ',ID_SO2COL,     ET_PHOT,     F, 'Pmolec cm-2', 'SO2 column density' ),
     &ELDTP( 'HCHO_COLUMN  ',ID_HCHOCOL,    ET_PHOT,     F, 'Pmolec cm-2', 'HCHO column density' ),
     &ELDTP( 'CO_COLUMN    ',ID_COCOL,      ET_PHOT,     F, 'Pmolec cm-2', 'CO column density' ),

      ! Other Special Variables and Properties            
     &ELDTP( 'PM25_FRM     ',ID_PM25_FRM,   ET_DRVD,     F, 'ug m-3', 'Federal Reference Method PM2.5' ),
     &ELDTP( 'PMF_FRM      ',ID_PMF_FRM,    ET_DRVD,     F, 'ug m-3', 'Federal Reference Method PMIJ' ),
     &ELDTP( 'RH           ',ID_RH,         ET_MET,      F, '1     ', 'Relative humidity' ),
     &ELDTP( 'TA           ',ID_TEMP,       ET_MET,      F, 'K     ', 'Temperature' ),
     &ELDTP( 'SFC_TMP      ',ID_SRFTEMP,    ET_MET,      F, 'K     ', '2-meter Surface Temperature' ),
     &ELDTP( 'PRES         ',ID_PRES,       ET_MET,      F, 'Pa    ', 'Pressure' ),
     &ELDTP( 'PBL          ',ID_PBL,        ET_MET,      F, 'm     ', 'Planetry Boundary Layer (PBL) Height' ),
     &ELDTP( 'RGRND        ',ID_RGRND,      ET_MET,      F, 'W m-2 ', 'Solar Radiation at Ground' ),
     &ELDTP( 'PRECIP       ',ID_PRECIP,     ET_MET,      F, 'cm    ', 'Precipitation' ),
     &ELDTP( 'WSPD10       ',ID_WSPD10,     ET_MET,      F, 'm s-1 ', 'Wind Speed at 10 m' ),
     &ELDTP( 'WDIR10       ',ID_WDIR10,     ET_MET,      F, 'deg   ', 'Wind Direction at 10 m' ),
     &ELDTP( 'WVEL         ',ID_WVEL,       ET_MET,      F, 'm s-1 ', 'Vertical Wind Velocity' ),
     &ELDTP( 'GAMMA_N2O5   ',ID_GN2O5F,     ET_CHEM,     F, '1     ', 'Fine Mode N2O5 Heterogeneous rxn probability' ),
     &ELDTP( 'GAMMA_N2O5K  ',ID_GN2O5C,     ET_CHEM,     F, '1     ', 'Coarse Mode N2O5 Heterogeneous rxn probability' ),
     &ELDTP( 'YIELD_CLNO2  ',ID_YCLNO2F,    ET_CHEM,     F, '1     ', 'Fine Mode CLNO2 Heterogeneous reaction yield' ),
     &ELDTP( 'YIELD_CLNO2K ',ID_YCLNO2C,    ET_CHEM,     F, '1     ', 'Coarse Mode CLNO2 Heterogeneous reaction yield' ),
     &ELDTP( 'GAMMA_IEPOX  ',ID_GIEPOX,     ET_CHEM,     F, '1     ', 'IEPOX heterogeneous uptake coefficient' ),
     &ELDTP( 'K_IEPOX      ',ID_KIEPOX,     ET_CHEM,     F, 's-1   ', 'IEPOX 1st order particle phase reaction rate const' ),
     &ELDTP( 'GAMMA_IMAE   ',ID_GIMAE,      ET_CHEM,     F, '1     ', 'IMAE+HMML heterogeneous uptake coefficient' ),
     &ELDTP( 'VOC_NOX      ',ID_VOC_NOX,    ET_CHEM,     F, '      ', 'VOC-limiting (>0.35) or NOx-limiting (<0.35) O3 formation' ),
     &ELDTP( 'EF_HNO3      ',ID_EF_HNO3,    ET_CHEM,     F, '1     ', 'Enhancement factor for HNO3 photolysis' ),
     &ELDTP( 'DZ           ',ID_DZ,         ET_MET,      F, 'm     ', 'Height of each grid cell computed from top layer height' ),
     &ELDTP( 'ZH           ',ID_ZH,         ET_MET,      F, 'm     ', 'Height of grid cell midpoints (mass-weighted)' ),
     &ELDTP( 'CFRAC        ',ID_CFRAC,      ET_MET,      F, '      ', 'Cloud Fraction' ),
     &ELDTP( 'PV           ',ID_PV,         ET_MET,      F, '      ', 'Potential Vorticity' ),
     &ELDTP( 'DENS         ',ID_DENS,       ET_MET,      F, 'kg m-3', 'Advected Density' ),
     &ELDTP( 'KZ           ',ID_KZ,         ET_MET,      F, 'm2 s-1', 'Vertical diffusivity coefficients' ),
     &ELDTP( 'KZMIN        ',ID_KZMIN,      ET_MET,      F, 'm2 s-1', 'Vertical diffusivity coefficients' ),
     &ELDTP( 'RHOJ         ',ID_RHOJ,       ET_MET,      F, 'kg m-3', 'Advected Density x Jacobian/MSFX^2' ) /)
     

      TYPE ELMO_KEY_TYPE
          CHARACTER( 32 )             :: WORD    ! Keyword for selecting multiple ELMO parameters
          INTEGER                     :: N       ! Number of Parameters associated with the Keyword
          CHARACTER( 32 ),ALLOCATABLE :: LIST(:) ! List of parameters
      END TYPE ELMO_KEY_TYPE

      TYPE (ELMO_KEY_TYPE), ALLOCATABLE, SAVE :: ELMO_KEY( : )
      INTEGER, SAVE :: N_KEY

      INTEGER, SAVE :: N_ELMO_FILES

      TYPE ELMO_FILE_TYPE
          CHARACTER(300)              :: FILENAME     ! Filename with full path
          CHARACTER(50)               :: SHORTNAME    ! ELMO[N]_[Flabel]
          INTEGER                     :: STRUCT       ! 0 = gridded, 1 = unstructured
          CHARACTER( 16)              :: FLOGICAL     ! I/O Logical Name for each output file
          INTEGER                     :: TSTEP        ! Output Time Step (HHMMSS)
          INTEGER                     :: TMODE        ! 0 = Instantaneous, 
                                                      ! 1 = Aggregate (Average/Cumulative)
          INTEGER                     :: LAY_BOT      ! Bottom Layer
          INTEGER                     :: LAY_TOP      ! Top Layer
          INTEGER                     :: NLAYS        ! Number of Layers
          INTEGER                     :: N_VARS_IN    ! Number of variables for this file 
                                                      !   specified through the interface
          CHARACTER(32), ALLOCATABLE  :: VARS_IN(:)   ! Variables on each output file
          INTEGER                     :: N_VARS       ! Number of Variables after processing
          INTEGER, ALLOCATABLE        :: TABID(:)     ! ID of Variable on ELMO_TABLE
          INTEGER, ALLOCATABLE        :: USEDID(:)    ! ID of Variable in ELMO_USED array
      END TYPE ELMO_FILE_TYPE

      TYPE ( ELMO_FILE_TYPE ), ALLOCATABLE, SAVE :: ELMO_FILE( : )

      LOGICAL, ALLOCATABLE, SAVE :: L_ELMO_TABLE_OUT(:), L_ELMO_TABLE_USED(:)

      TYPE ELMO_OUT_TYPE
          REAL, ALLOCATABLE           :: DATA(:,:,:,:)
      END TYPE ELMO_OUT_TYPE

      TYPE ( ELMO_OUT_TYPE ), ALLOCATABLE, SAVE :: ELMO_OUT( : )

      INTEGER, ALLOCATABLE, SAVE :: MAP_USEDtoTABLE( : ), MAP_TABLEtoUSED( : )
      LOGICAL, ALLOCATABLE, SAVE :: L_CALCULATED( : )
      INTEGER, ALLOCATABLE, SAVE :: ELMO_USED_UNIT( : )
      INTEGER, SAVE :: N_ELMO_USED
      INTEGER, SAVE :: LAY_TOP_MAX, LAY_BOT_MIN

      ! Variables for Storing Values of ELMO Output      
      REAL, ALLOCATABLE, SAVE :: ELMO_USED( : )
      LOGICAL, ALLOCATABLE, SAVE :: L_CALC( : )
      REAL, ALLOCATABLE, SAVE :: CONC_VEC( : )
      REAL, ALLOCATABLE, SAVE :: WDEP_VEC( : )
      REAL, ALLOCATABLE, SAVE :: DDEP_VEC( : )
#ifdef isam
      REAL, ALLOCATABLE, SAVE :: ISAM_VEC( :,: )
      REAL, ALLOCATABLE, SAVE :: ISAM_WDEP_VEC( :,: )
      REAL, ALLOCATABLE, SAVE :: ISAM_DDEP_VEC( :,: )
#endif
#ifdef sens
      REAL, ALLOCATABLE, SAVE :: DDM_VEC( :,: )
      REAL, ALLOCATABLE, SAVE :: DDM_WDEP_VEC( :,: )
      REAL, ALLOCATABLE, SAVE :: DDM_DDEP_VEC( :,: )
#endif
      
      
      INTEGER, SAVE           :: C1, R1, L1

      ! Variables for Aerosol Chemistry
      REAL, SAVE, ALLOCATABLE :: GAMMA_N2O5IJ( :,:,:  ) ! Fine modes N2O5 rx effic.
      REAL, SAVE, ALLOCATABLE :: GAMMA_N2O5K(  :,:,:  ) ! Coarse mode N2O5 rx effic.
      REAL, SAVE, ALLOCATABLE :: KPARTIEPOX (  :,:,:  ) ! IEPOX particle-phase reaction rate constant (sec-1)
      REAL, SAVE, ALLOCATABLE :: GAMMA_IEPOX(  :,:,:  ) ! IEPOX uptake coeff
      REAL, SAVE, ALLOCATABLE :: GAMMA_IMAE (  :,:,:  ) ! IMAE uptake coeff
      REAL, SAVE, ALLOCATABLE :: YCLNO2IJ   (  :,:,:  ) ! Yield CLNO2 in Fine modes 
      REAL, SAVE, ALLOCATABLE :: YCLNO2K    (  :,:,:  ) ! Yield CLNO2 in coarse mode 
      REAL, SAVE, ALLOCATABLE :: EF_HNO3    (  :,:,:  ) ! Enhancement factor for calculating J_ANO3
  
      LOGICAL, SAVE           :: AERO_CHEM_SET = .FALSE.
 
      ! Define type for storing names, properties, and components of
      ! all ELMO variables 
      TYPE ELMO_TABLE_TYPE
          CHARACTER (LEN = 16) :: NAME        ! Variable Name ( as it appears on the ELMO_LIST, in CGRID, etc.)
          CHARACTER (LEN = 16) :: OUT_NAME    ! Variable Name that will be written to the output file
          INTEGER              :: OUT_TYPE    ! IOAPI Variable Type
          INTEGER              :: ID          ! ID Number mapping to specific calculation, CGRID index, AERO 
                                              !   index, Family, etc.
          INTEGER              :: VAR_TYPE    ! ELMO Variable Type (Derived, Composite, CGRID, WDEP, DDEP)
          INTEGER              :: IM          ! Mode of interest if mode-dependent
          INTEGER              :: SOURCE      ! Source Number for ISAM or DDM3D Variables
          CHARACTER (LEN = 16) :: CUNIT       ! Units in Character Format
          INTEGER              :: IUNIT       ! Units in Integer Format:
                                              !   0 = unspecified;  1 = 'ug m-3';  2 = 'ppmV' 
          CHARACTER (LEN = 80) :: DESC        ! Description for Output File
                                              
          REAL                 :: MW          ! Species molecular weight, if defined
          LOGICAL              :: L_COMP      ! Is this variable a linear combination of other variables
          INTEGER              :: DRVD_TAB    ! Index in the table of Derived Variable Parameters
      END TYPE ELMO_TABLE_TYPE

      TYPE ELMO_COMP_TYPE
          INTEGER              :: N_VARS      ! Some variables are linear combinations of other variables.
                                              !   N_VARS is the number of components within such a combination. 
                                              !   This should not account for modes. For example, 
                                              !   to sum ASO4IJK, N_VARS would be 1 since only 
                                              !   ASO4 is being used.
          INTEGER, ALLOCATABLE :: TABID(:)    ! ID Number in ELMO_TABLE
          INTEGER, ALLOCATABLE :: ID(:)       ! Index in native array (e.g. CGRID, AEROSPC, SA_ACONC, etc.)
          INTEGER, ALLOCATABLE :: VAR_TYPE(:) ! ELMO Variable Type (Derived, CGRID, COMPOSITE, WDEP, DDEP)
          REAL,    ALLOCATABLE :: MW(:)       ! Species molecular weight, if defined
          INTEGER, ALLOCATABLE :: SPC_UNIT(:) ! Indicates whether this variables units are mass-based(1), 
                                              !   mole-based(2), or neither(0).
          REAL,    ALLOCATABLE :: COEFF(:)    ! Coefficient for each variable
          LOGICAL, ALLOCATABLE :: L_MODE(:)   ! For which modes does this aggregate apply
          LOGICAL, ALLOCATABLE :: L_SOURCE(:) ! For Which sources does this component apply
          INTEGER              :: INLET       ! Integer pointing to the corresponding inlet of choice 
                                              !   in the ELMO_INLET structure
          INTEGER              :: WETNESS     ! Integer prescribing wet or dry aerosol (1 = wet, 2 = dry)
                                              !   Default is wet
      END TYPE ELMO_COMP_TYPE

      TYPE (ELMO_TABLE_TYPE), ALLOCATABLE, SAVE :: ELMO_TABLE( : )
      TYPE (ELMO_COMP_TYPE ), ALLOCATABLE, SAVE :: ELMO_COMP( : ) 

      INTEGER, SAVE :: N_ELMO_TABLE



      ! Define List of Possible Units for Output
      INTEGER, PARAMETER :: N_UNIT = 39
      CHARACTER( 11 ), PARAMETER :: CUNIT( N_UNIT ) = 
     &    (/'UG M-3     ','PPMV       ','UGC M-3    ','PPMC       ',
     &      'UGN M-3    ','PPMN       ','M-3        ','M2 M-3     ',
     &      'HA-1       ','M2 HA-1    ','KG HA-1    ','1          ',
     &      '           ','UM         ','M3 M-3     ','N M-3      ',
     &      'KG M-3     ','UMOL M-3   ','MOL L-1    ','UG UG-1    ',
     &      'K          ','PA         ','S-1        ','M          ',
     &      'MM-1       ','M2 S-1     ','PMOLEC CM-2','NG M-3     ',
     &      'PPBV       ','MOL KG-1   ','KGN HA-1   ','KGS HA-1   ',
     &      'MEQN M-2   ','MEQS M-2   ','W M-2      ','CM         ',
     &      'M S-1      ','DEG        ','PPBC       '/)
      
      INTEGER, PARAMETER :: IU_UNSP       = 0   ! Unspecified Units
      INTEGER, PARAMETER :: IU_UGpM3      = 1   ! ug m-3
      INTEGER, PARAMETER :: IU_PPMV       = 2   ! ppmV
      INTEGER, PARAMETER :: IU_UGCpM3     = 3   ! ugC m-3
      INTEGER, PARAMETER :: IU_PPMC       = 4   ! ppmC
      INTEGER, PARAMETER :: IU_UGNpM3     = 5   ! ugN m-3
      INTEGER, PARAMETER :: IU_PPMN       = 6   ! ppmN
      INTEGER, PARAMETER :: IU_pM3        = 7   ! m-3
      INTEGER, PARAMETER :: IU_M2pM3      = 8   ! m2 m-3
      INTEGER, PARAMETER :: IU_pHA        = 9   ! ha-1
      INTEGER, PARAMETER :: IU_M2pHA      = 10  ! m2 ha-1
      INTEGER, PARAMETER :: IU_KGpHA      = 11  ! kg ha-1
      INTEGER, PARAMETER :: IU_1          = 12  ! 1
      INTEGER, PARAMETER :: IU_NONE       = 13  ! dimensionless
      INTEGER, PARAMETER :: IU_UM         = 14  ! um
      INTEGER, PARAMETER :: IU_M3pM3      = 15  ! m3 m-3
      INTEGER, PARAMETER :: IU_NpM3       = 16  ! N m-3
      INTEGER, PARAMETER :: IU_KGpM3      = 17  ! kg m-3
      INTEGER, PARAMETER :: IU_UMOLpM3    = 18  ! umol m-3
      INTEGER, PARAMETER :: IU_MOLpL      = 19  ! mol L-1
      INTEGER, PARAMETER :: IU_UGpUG      = 20  ! ug ug-1
      INTEGER, PARAMETER :: IU_K          = 21  ! K
      INTEGER, PARAMETER :: IU_PA         = 22  ! Pa
      INTEGER, PARAMETER :: IU_pS         = 23  ! s-1
      INTEGER, PARAMETER :: IU_M          = 24  ! m
      INTEGER, PARAMETER :: IU_pMM        = 25  ! Mm-1
      INTEGER, PARAMETER :: IU_M2pS       = 26  ! m2 s-1
      INTEGER, PARAMETER :: IU_PMOLECpCM2 = 27  ! Petamolecule cm-2
      INTEGER, PARAMETER :: IU_NGpM3      = 28  ! ng m-3
      INTEGER, PARAMETER :: IU_PPBV       = 29  ! ppbV
      INTEGER, PARAMETER :: IU_MOLpKG     = 30  ! mol kg-1
      INTEGER, PARAMETER :: IU_KGNpHA     = 31  ! kgN ha-1
      INTEGER, PARAMETER :: IU_KGSpHA     = 32  ! kgS ha-1
      INTEGER, PARAMETER :: IU_MEQNpM2    = 33  ! meqN m-2
      INTEGER, PARAMETER :: IU_MEQSpM2    = 34  ! meqS m-2
      INTEGER, PARAMETER :: IU_WpM2       = 35  ! W m-2
      INTEGER, PARAMETER :: IU_CM         = 36  ! CM
      INTEGER, PARAMETER :: IU_MpS        = 37  ! m s-1
      INTEGER, PARAMETER :: IU_DEG        = 38  ! deg
      INTEGER, PARAMETER :: IU_PPBC       = 39  ! ppbC


      ! Define Logical flags for turning on output categories
      LOGICAL, SAVE :: L_ELMO_WDEP, L_ELMO_DDEP

      TYPE ELMO_INLET_TYPE
          CHARACTER( 16 ) CINLET            ! Inlet Name
          REAL            DIAM_LO           ! Lower Diameter of this Inlet [um]
          REAL            DIAM_HI           ! Upper Diameter of this inlet [um]
      END TYPE ELMO_INLET_TYPE

      INTEGER, PARAMETER :: N_INLET = 10
      INTEGER, PARAMETER :: IPM01 = 1, IPM1 = 2, IPM25 = 3, IPM10 = 4, 
     &                      IPM25to10 = 5, IPMAMS = 6, INUM10 = 7,
     &                      INUM20 = 8, INUM40 = 9, INUM100 = 10
      INTEGER, PARAMETER :: IWET = 1, IDRY = 2

      TYPE (ELMO_INLET_TYPE), PARAMETER :: ELMO_INLET( N_INLET ) = (/
       !                       Inlet Name D_Lo  D_Hi
       !                       ---------- ----  ----
     &        ELMO_INLET_TYPE( 'PM01    ', 0.0,  0.1 ),
     &        ELMO_INLET_TYPE( 'PM1     ', 0.0,  1.0 ),
     &        ELMO_INLET_TYPE( 'PM25    ', 0.0,  2.5 ),
     &        ELMO_INLET_TYPE( 'PM10    ', 0.0, 10.0 ),
     &        ELMO_INLET_TYPE( 'PM25TO10', 2.5, 10.0 ),
     &        ELMO_INLET_TYPE( 'AMS     ', 0.0,  0.0 ),
     &        ELMO_INLET_TYPE( 'INUM10  ', 0.01,1.0e3),
     &        ELMO_INLET_TYPE( 'INUM20  ', 0.02,1.0e3),
     &        ELMO_INLET_TYPE( 'INUM40  ', 0.04,1.0e3),
     &        ELMO_INLET_TYPE( 'INUM100 ', 0.1, 1.0e3) /)

      REAL, SAVE :: INLET_FRAC( N_INLET,N_MODE,2 )  ! Fraction of each aerosol mode 
                                                    !   collected by each inlet
                                                    !   Third dimension is 1
                                                    !     for wet and 2 for dry

      ! Integers for specifying the kind of organic aerosol metric
      ! requested from the function SUM_ORGS in ELMO_CALC_DERIVED.F
      INTEGER, PARAMETER :: OT_OA = 1, OT_OC = 2, OT_OMOC = 3, 
     &                      OT_OTOC = 4, OT_DD_OA = 5, OT_WD_OA = 6,
     &                      OT_DD_OC = 7, OT_WD_OC = 8

      
      ! Make table of Parameters for documenting and calculating derived
      ! concentration and deposition variables
      TYPE ELMO_DRVD_TYPE
          INTEGER  ID        ! Variable ID from ELMO_LIST
          INTEGER  MODE      ! Index of Mode Mask
          INTEGER  INLET     ! Index of Mode Mask
          INTEGER  ORG_OP    ! Operation used for calculation
          INTEGER  ORG_MASK  ! Which organic categories to consider
      END TYPE ELMO_DRVD_TYPE

      INTEGER, PARAMETER :: N_ELMO_DRVD = 41
      TYPE (ELMO_DRVD_TYPE), PARAMETER :: ELMO_DRVD_TAB( N_ELMO_DRVD ) = (/
       !                 ID           Mode    Inlet   ORG_OP    ORG_MASK
       !                ------------- -----   -----   ------    --------
       !                                   
        ! Mass Variables
     &  ELMO_DRVD_TYPE( ID_PM,        IM_ALL, 0,        0,        0       ),
     &  ELMO_DRVD_TYPE( ID_PMU,       IM_UF,  0,        0,        0       ),
     &  ELMO_DRVD_TYPE( ID_PMF,       IM_F,   0,        0,        0       ),
     &  ELMO_DRVD_TYPE( ID_PMC,       IM_C,   0,        0,        0       ),
     &  ELMO_DRVD_TYPE( ID_PMNUC,     IM_NUC, 0,        0,        0       ),
     &  ELMO_DRVD_TYPE( ID_PMAIT,     IM_AIT, 0,        0,        0       ),
     &  ELMO_DRVD_TYPE( ID_PMACC,     IM_ACC, 0,        0,        0       ),
     &  ELMO_DRVD_TYPE( ID_PM01,      IM_ALL, IPM01,    0,        0       ),
     &  ELMO_DRVD_TYPE( ID_PM1,       IM_ALL, IPM1,     0,        0       ),
     &  ELMO_DRVD_TYPE( ID_PM25,      IM_ALL, IPM25,    0,        0       ),
     &  ELMO_DRVD_TYPE( ID_PM10,      IM_ALL, IPM10,    0,        0       ),
     &  ELMO_DRVD_TYPE( ID_PM25to10,  IM_ALL, IPM25to10,0,        0       ),
     &  ELMO_DRVD_TYPE( ID_PMAMS,     IM_ALL, IPMAMS,   0,        0       ),

        ! Organics
     &  ELMO_DRVD_TYPE( ID_PMF_OA,    IM_F,   0,        OT_OA,    0       ),                                          
     &  ELMO_DRVD_TYPE( ID_PMF_OC,    IM_F,   0,        OT_OC,    0       ),                                          
     &  ELMO_DRVD_TYPE( ID_PMF_POA,   IM_F,   0,        OT_OA,    IOA_PRI ),                                          
     &  ELMO_DRVD_TYPE( ID_PMF_POC,   IM_F,   0,        OT_OC,    IOA_PRI ),                                          
     &  ELMO_DRVD_TYPE( ID_PMF_SOA,   IM_F,   0,        OT_OA,    IOA_SEC ),                                          
     &  ELMO_DRVD_TYPE( ID_PMF_SOC,   IM_F,   0,        OT_OC,    IOA_SEC ),                                          
     &  ELMO_DRVD_TYPE( ID_PMF_ASOA,  IM_F,   0,        OT_OA, IOA_ANTSEC ),  
     &  ELMO_DRVD_TYPE( ID_PMF_BSOA,  IM_F,   0,        OT_OA, IOA_BIOSEC ),                                          
     &  ELMO_DRVD_TYPE( ID_PMF_OMOC,  IM_F,   0,        OT_OMOC,  0       ),                                          
     &  ELMO_DRVD_TYPE( ID_PMF_OTOC,  IM_F,   0,        OT_OTOC,  0       ),                                          
     &  ELMO_DRVD_TYPE( ID_AMS_OA,    IM_ALL, IPMAMS,   OT_OA,    0       ),                                          
     &  ELMO_DRVD_TYPE( ID_AMS_OTOC,  IM_ALL, IPMAMS,   OT_OTOC,  0       ),                                          
     &  ELMO_DRVD_TYPE( ID_PM1_OA,    IM_ALL, IPM1,     OT_OA,    0       ),                                          
     &  ELMO_DRVD_TYPE( ID_PM1_OC,    IM_ALL, IPM1,     OT_OC,    0       ),                                          
     &  ELMO_DRVD_TYPE( ID_PM25_OA,   IM_ALL, IPM25,    OT_OA,    0       ),                                          
     &  ELMO_DRVD_TYPE( ID_PM25_OC,   IM_ALL, IPM25,    OT_OC,    0       ),                                          
       
        ! Organic Deposition
     &  ELMO_DRVD_TYPE( ID_DD_OA,     IM_F,   0,        OT_DD_OA, 0       ),                                          
     &  ELMO_DRVD_TYPE( ID_WD_OA,     IM_F,   0,        OT_WD_OA, 0       ),                                          
     &  ELMO_DRVD_TYPE( ID_DD_OC,     IM_F,   0,        OT_DD_OC, 0       ),                                          
     &  ELMO_DRVD_TYPE( ID_WD_OC,     IM_F,   0,        OT_WD_OC, 0       ),                                          
     &  ELMO_DRVD_TYPE( ID_DD_POA,    IM_F,   0,        OT_DD_OA, IOA_PRI ),                                          
     &  ELMO_DRVD_TYPE( ID_WD_POA,    IM_F,   0,        OT_WD_OA, IOA_PRI ),                                          
     &  ELMO_DRVD_TYPE( ID_DD_POC,    IM_F,   0,        OT_DD_OC, IOA_PRI ),                                          
     &  ELMO_DRVD_TYPE( ID_WD_POC,    IM_F,   0,        OT_WD_OC, IOA_PRI ),                                          
     &  ELMO_DRVD_TYPE( ID_DD_SOA,    IM_F,   0,        OT_DD_OA, IOA_SEC ),                                          
     &  ELMO_DRVD_TYPE( ID_WD_SOA,    IM_F,   0,        OT_WD_OA, IOA_SEC ),                                          
     &  ELMO_DRVD_TYPE( ID_DD_SOC,    IM_F,   0,        OT_DD_OC, IOA_SEC ),                                    
     &  ELMO_DRVD_TYPE( ID_WD_SOC,    IM_F,   0,        OT_WD_OC, IOA_SEC )

     &  /)
                                                                                                                   
                                                                                                                                  
                                                                                                                                  
      ! Define global arrays for capturing ELMO variable data from other                                                          
      ! CMAQ modules                                                                                                              
        REAL, ALLOCATABLE, SAVE, PUBLIC ::                                                                                        
     &                             ELMO_AOD_550(:,:),  ! total aerosol optical depth at                                           
                                                       !   550 nm (Angstrom Interpolation)                                        
     &                             ELMO_EXT_550(:,:,:),! aerosol extinction at 550 nm for layer [1/m]                             
     &                             ELMO_NO2COL(:,:),   ! NO2 Column                                                                 
     &                             ELMO_SO2COL(:,:),   ! SO2 Column                                                                 
     &                             ELMO_HCHOCOL(:,:),  ! Formaldehyde Column                                                        
     &                             ELMO_COCOL(:,:),    ! CO Column                                                                  
     &                             ELMO_VOC_NOX(:,:,:) ! VOC or NOx limited ozone formation                                                                  
                                                                                                                                  
        REAL, ALLOCATABLE, SAVE :: ELMO_WDEP(:,:,:) ! wet deposition accumulated per time step                                    
        REAL, ALLOCATABLE, SAVE :: ELMO_DDEP(:,:,:) ! dry deposition accumulated per time step                                    
        REAL, ALLOCATABLE, SAVE :: ELMO_ISAM_WDEP(:,:,:,:) ! wet deposition accumulated per time step                             
        REAL, ALLOCATABLE, SAVE :: ELMO_ISAM_DDEP(:,:,:,:) ! dry deposition accumulated per time step                             
        REAL, ALLOCATABLE, SAVE :: ELMO_DDM_WDEP(:,:,:,:) ! wet deposition accumulated per time step                              
        REAL, ALLOCATABLE, SAVE :: ELMO_DDM_DDEP(:,:,:,:) ! dry deposition accumulated per time step                              
                                                                                                                                  
        REAL, SAVE :: WDIR10, PV                                                                                                                
                                                                                                                                                
        LOGICAL, SAVE :: INIT_TIME

        ! Set Value for Missing Variables
#ifdef mpas
        REAL, PARAMETER :: ELMO_BLANK = -9.000E36
#else
        REAL, PARAMETER :: ELMO_BLANK = AMISS3
#endif       
        CONTAINS

!------------------------------------------------------------------------------------------            
        subroutine elmo_init_shared
!------------------------------------------------------------------------------------------            
        use runtime_vars, only : voc_nox_trans
        use grid_conf,  only : ncols, nrows, nlays
        use cgrid_spcs, only : n_cgrid_spc
#ifdef isam
        use sa_defn,    only : nspc_sa, ntag_sa
#endif
#ifdef sens
        use ddm3d_defn, only : npmax
#endif

        implicit none

        integer ios
        character(32) :: pname = 'ELMO_INIT_SHARED'
        logical, save :: firstime = .True.

        if (firstime ) then
            firstime = .false.
            
            allocate( elmo_aod_550(ncols, nrows ), stat=ios)
            call checkmem( ios, 'ELMO_AOD_550',pname )
            elmo_aod_550 = 0.0
     
            allocate( elmo_ext_550(ncols, nrows, nlays ), stat=ios)
            call checkmem( ios, 'ELMO_EXT_550',pname )
            elmo_ext_550 = 0.0
     
            allocate( elmo_no2col(ncols, nrows ), stat=ios)
            call checkmem( ios, 'ELMO_NO2COL',pname )
            elmo_no2col = 0.0
     
            allocate( elmo_so2col(ncols, nrows ), stat=ios)
            call checkmem( ios, 'ELMO_SO2COL',pname )
            elmo_so2col = 0.0
     
            allocate( elmo_hchocol(ncols, nrows ), stat=ios)
            call checkmem( ios, 'ELMO_HCHOCOL',pname )
            elmo_hchocol = 0.0
     
            allocate( elmo_cocol(ncols, nrows ), stat=ios)
            call checkmem( ios, 'ELMO_COCOL',pname )
            elmo_cocol = 0.0
     
            allocate( elmo_wdep(ncols, nrows, n_cgrid_spc ), stat=ios)
            call checkmem( ios, 'ELMO_WDEP',pname )
            elmo_wdep = 0.0
     
            allocate( elmo_ddep(ncols, nrows, n_cgrid_spc ), stat=ios)
            call checkmem( ios, 'ELMO_DDEP',pname )
            elmo_ddep = 0.0

#ifdef isam
            allocate( elmo_voc_nox(ncols, nrows, nlays ), stat=ios)
            call checkmem( ios, 'ELMO_VOC_NOX',pname )
            elmo_voc_nox = voc_nox_trans
         
            allocate( elmo_isam_wdep(ncols, nrows, nspc_sa, ntag_sa ), stat=ios)
            call checkmem( ios, 'ELMO_ISAM_WDEP',pname )
            elmo_isam_wdep = 0.0
         
            allocate( elmo_isam_ddep(ncols, nrows, nspc_sa, ntag_sa ), stat=ios)
            call checkmem( ios, 'ELMO_ISAM_DDEP',pname )
            elmo_isam_ddep = 0.0
#endif
#ifdef sens
            allocate( elmo_ddm_wdep(ncols, nrows, n_cgrid_spc, npmax ), stat=ios)
            call checkmem( ios, 'ELMO_DDM_WDEP',pname )
            elmo_ddm_wdep = 0.0
            
            allocate( elmo_ddm_ddep(ncols, nrows, n_cgrid_spc, npmax ), stat=ios)
            call checkmem( ios, 'ELMO_DDM_DDEP',pname )
            elmo_ddm_ddep = 0.0
#endif
        end if


        end subroutine elmo_init_shared


!------------------------------------------------------------------------------------------            
      SUBROUTINE MAP_ELMO_NML( IFL, L_ELMO_TABLE, IERROR )
!     This subroutine maps user input from the CMAQ_Control namelist to
!     the ELMO_TABLE to define which variables should be output. It
!     expands Keywords, maps to concentrations, deposition fluxes, and
!     families, with expansion for families if requested (using a * in
!     front of the family name).
!-------------------------------------------------------------------------

      USE util_Family_module
      USE RUNTIME_VARS, ONLY : MECH

      IMPLICIT NONE

      INTEGER, INTENT( IN )  :: IFL
      LOGICAL, INTENT( OUT ) :: L_ELMO_TABLE( N_ELMO_TABLE )
      INTEGER, INTENT( OUT ) :: IERROR
      CHARACTER(32), ALLOCATABLE :: VARS_NML( : )

      INTEGER N_END, N_NML
      LOGICAL EXPAND_NML
      INTEGER I1, I2, I3, IT, IVAR, J, IV
      
      
      CHARACTER( 400 ) :: XMSG
      CHARACTER(32), PARAMETER :: PNAME = 'MAP_ELMO_NML'

      
      IERROR = 0

      ! Diagnose VARS_NML_IN Size
      N_NML = ELMO_FILE( IFL )%N_VARS_IN
      N_END = INDEX1( '', N_NML, ELMO_FILE( IFL )%VARS_IN(:) ) - 1
      IF ( N_END .LE. 0 ) THEN
          WRITE( XMSG, '(A)' ),'ERROR: Unreadable variables requested for ELMO output'
          CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
      END IF
      
      ! Initialize VARS_NML
      ALLOCATE( VARS_NML( 1000 ) )
      VARS_NML = ''
      VARS_NML( 1:N_END ) = ELMO_FILE( IFL )%VARS_IN( 1:N_END )

      ! Loop through VARS_NML and Activate ELMO Variables
      DO WHILE ( N_END .GT. 0 )
         CALL UPCASE( VARS_NML( 1 ) )
         EXPAND_NML = .FALSE.
         IF ( VARS_NML(1)(1:1) .EQ. '*' ) THEN
            EXPAND_NML = .TRUE.
            VARS_NML(1)(1:32) = VARS_NML( 1 )(2:32)//' '
         END IF

         ! Detect location of VARS_NML( INUM ) in the ELMO_TABLE
         ! or Keyword List
         I1 = INDEX1( VARS_NML(1), N_KEY, ELMO_KEY%WORD )
         I2 = INDEX1( VARS_NML(1), N_ELMO_TABLE, ELMO_TABLE%NAME )
         I3 = INDEX1( VARS_NML(1), N_ELMO_TABLE, ELMO_TABLE%OUT_NAME )

         IF ( VARS_NML(1) .EQ. 'ALL' ) THEN
            ! Add All ELMO_TABLE variables to the File
            L_ELMO_TABLE( : ) = .TRUE.

         ELSE IF ( VARS_NML(1) .EQ. 'ALL_CONC' ) THEN
            ! Activate all CGRID species
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_CONC )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_DEP' ) THEN
            ! Activate all Deposition Fluxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_WDEP .OR.
     &               ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_DDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_WDEP' ) THEN
            ! Activate all Wet Deposition Fluxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_WDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_DDEP' ) THEN
            ! Activate all Dry Deposition FLuxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_DDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_ISAM_CONC' ) THEN
            ! Activate all ISAM Species
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_ISAM_CONC )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_ISAM_WDEP' ) THEN
            ! Activate all ISAM Wet Deposition Fluxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_ISAM_WDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_ISAM_DDEP' ) THEN
            ! Activate all ISAM Dry Deposition Fluxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_ISAM_DDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_DDM_CONC' ) THEN
            ! Activate all DDM3D Species
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_DDM_CONC )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_DDM_WDEP' ) THEN
            ! Activate all DDM Wet Deposition Fluxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_DDM_WDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_DDM_DDEP' ) THEN
            ! Activate all DDM Dry Deposition Fluxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_DDM_DDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
         
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_AEROPROP' ) THEN
            ! Activate all Aerosol Properties
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_AEROPROP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
         
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_CHEM' ) THEN
            ! Activate all Available Chemistry Parameters
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_CHEM )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
         
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_MET' ) THEN
            ! Activate all Meteorological Variables
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_MET )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
         
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_PHOT' ) THEN
            ! Activate all Photolysis Variables
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_PHOT )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
 
         ELSE IF ( VARS_NML(1) .EQ. 'DEFAULT' ) THEN
            ! Replace generic DEFAULT keyword with mechanism-specific
            ! keyword
            IF ( MECH .EQ. 'BLANK' ) THEN
               XMSG = 'You have requested the DEFAULT keyword on an ELMO ' // 
     &                'output file, and the variable MECH is needed in ' //
     &                'order to know which DEFAULT keyword to map to. However, ' //
     &                'MECH has not been defined as an environment variable. ' //
     &                'For example, if your run script is in tcsh, make sure to ' //
     &                'assign $MECH with setenv.'
               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
            ELSE    
               VARS_NML( N_END+1 ) = 'DEFAULT_' // TRIM( MECH ) 
               N_END = N_END + 1
            END IF
         
         ELSE IF ( VARS_NML(1) .EQ. 'DEFAULT_DEP' ) THEN
            ! Replace generic DEFAULT keyword with mechanism-specific
            ! keyword
            IF ( MECH .EQ. 'BLANK' ) THEN
               XMSG = 'You have requested the DEFAULT_DEP keyword on an ELMO ' // 
     &                'output file, and the variable MECH is needed in ' //
     &                'order to know which DEFAULT_DEP keyword to map to. However, ' //
     &                'MECH has not been defined as an environment variable. ' //
     &                'For example, if your run script is in tcsh, make sure to ' //
     &                'assign $MECH with setenv.'
               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
            ELSE
               VARS_NML( N_END+1 ) = 'DEFAULT_DEP_' // TRIM( MECH )
               N_END = N_END + 1
            END IF
         
         ELSEIF ( I1 .GT. 0 ) THEN
            ! Expand Matched Keyword
            VARS_NML( N_END+1:N_END+ELMO_KEY( I1 )%N ) =
     &                              ELMO_KEY( I1 )%LIST
            N_END = N_END + ELMO_KEY( I1 )%N

         ELSE IF ( I2 .GT. 0 ) THEN
            ! First check for COMPOSITES
            IF ( (ELMO_TABLE( I2 )%VAR_TYPE .EQ. ET_CMPST .OR.
     &            ELMO_TABLE( I2 )%VAR_TYPE .EQ. ET_CMPST_DEP .OR.
     &            ELMO_TABLE( I2 )%VAR_TYPE .EQ. ET_ISAM_CMPST .OR.
     &            ELMO_TABLE( I2 )%VAR_TYPE .EQ. ET_ISAM_CMPST_DEP .OR.
     &            ELMO_TABLE( I2 )%VAR_TYPE .EQ. ET_DDM_CMPST .OR.
     &            ELMO_TABLE( I2 )%VAR_TYPE .EQ. ET_DDM_CMPST_DEP ) 
     &          .AND. EXPAND_NML ) THEN

                 ! Expand the COMPOSITE if the * was used
                 DO IVAR = 1,ELMO_COMP( I2 )%N_VARS
                   J = ELMO_COMP( I2 )%TABID( IVAR )
                   VARS_NML( N_END+IVAR ) = ELMO_TABLE( J )%NAME 
                 END DO
                 N_END = N_END + ELMO_COMP( I2 )%N_VARS
                
            ELSE 
              ! Entry Matches the NAME of a variable on the ELMO Table.
              ! But this could be a mode-dependent or source-dependent 
              ! variable so all of the ELMO Table entries that match 
              ! this name but correspond to different modes or sources 
              ! should be activated.
              DO IT = 1,N_ELMO_TABLE
                  IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ELMO_TABLE( I2 )%VAR_TYPE .AND.
     &                 ELMO_TABLE( IT )%NAME .EQ. ELMO_TABLE( I2 )%NAME ) THEN
                     L_ELMO_TABLE( IT ) = .TRUE.
                  END IF
              END DO

            END IF

         ELSE IF ( I3 .GT. 0 ) THEN
            ! Entry Matches the OUT_NAME of a variable on the ELMO
            ! Table.    
            ! First check for COMPOSITES
            IF ( (ELMO_TABLE( I3 )%VAR_TYPE .EQ. ET_CMPST .OR.
     &            ELMO_TABLE( I3 )%VAR_TYPE .EQ. ET_CMPST_DEP .OR.
     &            ELMO_TABLE( I3 )%VAR_TYPE .EQ. ET_ISAM_CMPST .OR.
     &            ELMO_TABLE( I3 )%VAR_TYPE .EQ. ET_ISAM_CMPST_DEP .OR.
     &            ELMO_TABLE( I3 )%VAR_TYPE .EQ. ET_DDM_CMPST .OR.
     &            ELMO_TABLE( I3 )%VAR_TYPE .EQ. ET_DDM_CMPST_DEP ) 
     &          .AND. EXPAND_NML ) THEN

                 ! Expand the COMPOSITE if the * was used
                 DO IVAR = 1,ELMO_COMP( I3 )%N_VARS
                   J = ELMO_COMP( I3 )%TABID( IVAR )
                   VARS_NML( N_END+IVAR ) = ELMO_TABLE( J )%OUT_NAME 
                 END DO
                 N_END = N_END + ELMO_COMP( I3 )%N_VARS
                
            ELSE 
                 ! This is a very specific variable. Mode and source
                 ! dependence are fixed.
                 L_ELMO_TABLE( I3 ) = .TRUE.
            END IF
          
         ELSE
            ! Entry Doesn't Match any variable on the ELMO table. Crash the
            ! model.
            XMSG = 'An ELMO variable ' // TRIM(VARS_NML(1)) //
     &             ' has been requested that does not exist. It is on ' //
     &             ' file: ' // TRIM( ELMO_FILE( IFL )%FLOGICAL )
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF

         ! Remove Keyword after processing
         VARS_NML( 1:N_END-1 ) = VARS_NML( 2:N_END )
         VARS_NML( N_END ) = ''
         N_END = N_END - 1
      END DO

      ! Check if any dry deposition variable has been requested
      DO IVAR = 1,N_ELMO_TABLE
         IV = ELMO_TABLE( IVAR )%VAR_TYPE
         IF ( IV .EQ. ET_DDEP .OR. IV .EQ. ET_ISAM_DDEP .OR.
     &        IV .EQ. ET_DDM_DDEP .OR. IV .EQ. ET_CMPST_DEP .OR.
     &        IV .EQ. ET_ISAM_CMPST_DEP .OR. IV .EQ. ET_DDM_CMPST_DEP .OR.
     &        IV .EQ. ET_DRVD_DDEP .OR. IV .EQ. ET_ISAM_DRVD_DDEP .OR.
     &        IV .EQ. ET_DDM_DRVD_DDEP ) THEN
         
              L_ELMO_DDEP = .TRUE.
              EXIT
         END IF
      END DO

      ! Check if any wet deposition variable has been requested
      DO IVAR = 1,N_ELMO_TABLE
         IV = ELMO_TABLE( IVAR )%VAR_TYPE
         IF ( IV .EQ. ET_WDEP .OR. IV .EQ. ET_ISAM_WDEP .OR.
     &        IV .EQ. ET_DDM_WDEP .OR. IV .EQ. ET_CMPST_DEP .OR.
     &        IV .EQ. ET_ISAM_CMPST_DEP .OR. IV .EQ. ET_DDM_CMPST_DEP .OR.
     &        IV .EQ. ET_DRVD_WDEP .OR. IV .EQ. ET_ISAM_DRVD_WDEP .OR.
     &        IV .EQ. ET_DDM_DRVD_WDEP ) THEN
         
              L_ELMO_WDEP = .TRUE.
              EXIT
         END IF
      END DO

      END SUBROUTINE MAP_ELMO_NML    

!------------------------------------------------------------------------------------------            
      SUBROUTINE ELMO_CHECK_LAYERS( LAYER_TOP, LAYER_BOT, IFILE )
!     This subroutine checks that the layers specified for an ELMO file
!     are allowed.
!-------------------------------------------------------------------------

      USE GRID_CONF, ONLY : NLAYS

      IMPLICIT NONE

      INTEGER, INTENT( INOUT )    :: LAYER_TOP
      INTEGER, INTENT( IN )       :: LAYER_BOT
      INTEGER, INTENT( IN )       :: IFILE
      CHARACTER(250)              :: XMSG
      CHARACTER(32), PARAMETER    :: PNAME = 'ELMO_CHECK_LAYERS'
      CHARACTER( 13 )             :: FNAME

      WRITE( FNAME, '("ELMO FILE ",I3.3)' ) IFILE

      IF ( LAYER_TOP .EQ. -1 ) LAYER_TOP = NLAYS
      IF ( LAYER_TOP .LE. 0 .OR. LAYER_TOP .GT. NLAYS ) THEN
           XMSG = TRIM( FNAME ) // '_LAY_TOP must be prescribed between '//
     &            '1 and NLAYS, or set to -1 to automatically choose NLAYS.'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      IF ( LAYER_BOT .LE. 0 .OR. LAYER_BOT .GT. NLAYS ) THEN
           XMSG = TRIM(FNAME) // '_LAY_BOT must be prescribed between '// 
     &            '1 and NLAYS.'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      IF ( LAYER_BOT .GT. LAYER_TOP ) THEN
           XMSG = TRIM(FNAME) // ' LAY_BOT must be less than or equal '// 
     &            'to ' // TRIM(FNAME) // ' LAY_TOP.'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      END SUBROUTINE ELMO_CHECK_LAYERS

!-------------------------------------------------------------------------
      SUBROUTINE MAP_ELMO_UNITS( UNIT_TMP, IUNIT, VARNAME, VARTYPE )
!     This subroutine maps units in character strings to a standard
!     vector of ELMO units that are available for output. This integer
!     vector distinguishes mass-based units from mole-based units so
!     that density conversions may be applied online.
!-------------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER( * ), INTENT( IN ) :: UNIT_TMP 
      INTEGER, INTENT( OUT )       :: IUNIT
      CHARACTER( 16 ), INTENT( IN ):: VARNAME
      INTEGER, INTENT( IN )        :: VARTYPE
      CHARACTER(  16 ) UNITTYPE
      CHARACTER( 200 ) XMSG
      INTEGER IT
 

      ! Assign Unit to Aggregate Variable
      UNITTYPE = UNIT_TMP
      CALL UPCASE( UNITTYPE )
      
      IUNIT = 0
      DO IT = 1,N_UNIT
         IF ( UNITTYPE .EQ. CUNIT( IT ) ) THEN
            IUNIT = IT
         END IF
      END DO

      IF ( IUNIT .EQ. 0 ) THEN
         IUNIT = 0
         XMSG = 'A unit (' // trim(UNIT_TMP) // ') has been given ' //
     &        'for the mapping of a variable of type ' // 
     &        TRIM(CETYPE(VARTYPE)) // ' that is unknown to ELMO. ' //
     &        'Confirm that this is correct for variable ' // trim(VARNAME) //
     &        '.  ' // TRIM( UNITTYPE)
         CALL M3WARN( 'MAP_ELMO_UNITS', 0, 0, XMSG )
      END IF
 
      END SUBROUTINE MAP_ELMO_UNITS
 
!------------------------------------------------------------------------------------------            
      REAL FUNCTION ELMO_UNIT_CONV( J, OUT_UNIT, D )
!     This subroutine calculates the conversion factor from a component
!     variable to an output variable.
!-------------------------------------------------------------------------
 
      USE AEROMET_DATA, ONLY : AIRDENS
      USE CONST, ONLY : MWAIR

      IMPLICIT NONE
 
      INTEGER, INTENT( IN )  :: J         ! Table ID of Data
      INTEGER, INTENT( IN )  :: OUT_UNIT  ! Output unit
      REAL,    INTENT( IN )  :: D         ! Data to be converted 

      INTEGER :: SPC_UNIT
      CHARACTER(16) :: PNAME = 'ELMO_UNIT_CONV'
      CHARACTER(200) :: XMSG

      ! Determine Unit Conversion Factors
      SPC_UNIT = ELMO_TABLE( J )%IUNIT
      
      IF ( SPC_UNIT .EQ. OUT_UNIT .OR. OUT_UNIT .EQ. 0 ) THEN
          ELMO_UNIT_CONV = D
          RETURN
      END IF

      ! Check if this variable has a non-zero MW, which is needed
      ! for any unit conversions.
      IF ( ELMO_TABLE( J )%MW .EQ. 0.0 ) THEN
        XMSG = 'ELMO is attempting to convert variable ' // 
     &       TRIM( ELMO_TABLE( J )%NAME ) // ' from '//
     &       'units but the MW is equal to 0.0'
        CALL M3WARN( PNAME, 0, 0, XMSG )
      END IF

      ! Calculate Unit Conversions
      SELECT CASE ( OUT_UNIT )
        CASE ( IU_UGpM3 ) ! ug m-3 out
          SELECT CASE ( SPC_UNIT )
            CASE ( IU_UGpM3 ) ! mass-based
              ELMO_UNIT_CONV = D
            CASE ( IU_PPMV ) ! mole-based
              ELMO_UNIT_CONV = D * ELMO_TABLE( J )%MW
     &            / MWAIR * AIRDENS * 1000.0 ! to mass concentration
            CASE DEFAULT
              XMSG = 'ELMO is attempting to convert variable ' // 
     &           TRIM( ELMO_TABLE( J )%NAME ) // ' from '// 
     &           TRIM(CUNIT( SPC_UNIT )) // ' to ' // TRIM(CUNIT( OUT_UNIT )) //
     &           ' but it does not know to do it. No conversion is applied.'
              CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
          END SELECT 


        CASE ( IU_PPMV ) ! ppmv out
          SELECT CASE ( SPC_UNIT )
            CASE ( IU_UGpM3 ) ! mass-based
              ELMO_UNIT_CONV = D / ELMO_TABLE( J )%MW 
     &             * MWAIR / AIRDENS / 1000.0 ! to mole mixing ratio
            CASE ( IU_PPMV ) ! mole-based
              ELMO_UNIT_CONV = D
            CASE DEFAULT
              XMSG = 'ELMO is attempting to convert variable ' // 
     &           TRIM( ELMO_TABLE( J )%NAME ) // ' from '// 
     &           TRIM(CUNIT( SPC_UNIT )) // ' to ' // TRIM(CUNIT( OUT_UNIT )) //
     &           ' but it does not know to do it. No conversion is applied.'
              CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
          END SELECT 
 
        CASE ( IU_UMOLpM3 ) ! umol m-3 out
          SELECT CASE ( SPC_UNIT )
            CASE ( IU_UGpM3 ) ! mass-based
              ELMO_UNIT_CONV = D / ELMO_TABLE( J )%MW
            CASE ( IU_PPMV ) ! mole-based
              ELMO_UNIT_CONV = D / MWAIR * AIRDENS * 1000.0 
            CASE DEFAULT
              XMSG = 'ELMO is attempting to convert variable ' // 
     &           TRIM( ELMO_TABLE( J )%NAME ) // ' from '// 
     &           TRIM(CUNIT( SPC_UNIT )) // ' to ' // TRIM(CUNIT( OUT_UNIT )) //
     &           ' but it does not know to do it. No conversion is applied.'
              CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
          END SELECT 
 
        CASE ( IU_NGpM3 ) ! ng m-3 out
          SELECT CASE ( SPC_UNIT )
            CASE ( IU_UGpM3 ) ! mass-based
              ELMO_UNIT_CONV = D * 1000.0
            CASE ( IU_PPMV ) ! mole-based
              ELMO_UNIT_CONV = D * ELMO_TABLE( J )%MW
     &            / MWAIR * AIRDENS * 1.0E6 ! to mass concentration
            CASE DEFAULT
              XMSG = 'ELMO is attempting to convert variable ' // 
     &           TRIM( ELMO_TABLE( J )%NAME ) // ' from '// 
     &           TRIM(CUNIT( SPC_UNIT )) // ' to ' // TRIM(CUNIT( OUT_UNIT )) //
     &           ' but it does not know to do it. No conversion is applied.'
              CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
          END SELECT 

        CASE ( IU_PPBV ) ! ppbV out
          SELECT CASE ( SPC_UNIT )
            CASE ( IU_UGpM3 ) ! mass-based
              ELMO_UNIT_CONV = D / ELMO_TABLE( J )%MW 
     &             * MWAIR / AIRDENS ! to mole mixing ratio
            CASE ( IU_PPMV ) ! mole-based
              ELMO_UNIT_CONV = D * 1000.0
            CASE DEFAULT
              XMSG = 'ELMO is attempting to convert variable ' // 
     &           TRIM( ELMO_TABLE( J )%NAME ) // ' from '// 
     &           TRIM(CUNIT( SPC_UNIT )) // ' to ' // TRIM(CUNIT( OUT_UNIT )) //
     &           ' but it does not know to do it. No conversion is applied.'
              CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
          END SELECT 
 
        CASE ( IU_PPMC ) ! ppmC out
          SELECT CASE ( SPC_UNIT )
            CASE ( IU_UGpM3 ) ! mass-based
              ELMO_UNIT_CONV = D / ELMO_TABLE( J )%MW 
     &             * MWAIR / AIRDENS / 1000.0 ! to mole mixing ratio
            CASE ( IU_PPMV ) ! mole-based
              ELMO_UNIT_CONV = D
            CASE DEFAULT
              XMSG = 'ELMO is attempting to convert variable ' // 
     &           TRIM( ELMO_TABLE( J )%NAME ) // ' from '// 
     &           TRIM(CUNIT( SPC_UNIT )) // ' to ' // TRIM(CUNIT( OUT_UNIT )) //
     &           ' but it does not know to do it. No conversion is applied.'
              CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
          END SELECT 
 
        CASE ( IU_KGNpHA ) ! kgN ha-1 out
          SELECT CASE ( SPC_UNIT )
            CASE ( IU_KGpHA ) ! deposition output
              ELMO_UNIT_CONV = D 
            CASE DEFAULT
              XMSG = 'ELMO is attempting to convert variable ' // 
     &           TRIM( ELMO_TABLE( J )%NAME ) // ' from '// 
     &           TRIM(CUNIT( SPC_UNIT )) // ' to ' // TRIM(CUNIT( OUT_UNIT )) //
     &           ' but it does not know to do it. No conversion is applied.'
              CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
          END SELECT 
 
        CASE ( IU_KGSpHA ) ! kgS ha-1 out
          SELECT CASE ( SPC_UNIT )
            CASE ( IU_KGpHA ) ! deposition output
              ELMO_UNIT_CONV = D 
            CASE DEFAULT
              XMSG = 'ELMO is attempting to convert variable ' // 
     &           TRIM( ELMO_TABLE( J )%NAME ) // ' from '// 
     &           TRIM(CUNIT( SPC_UNIT )) // ' to ' // TRIM(CUNIT( OUT_UNIT )) //
     &           ' but it does not know to do it. No conversion is applied.'
              CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
          END SELECT 
 

        CASE DEFAULT
              XMSG = 'ELMO is attempting to convert variable ' // 
     &           TRIM( ELMO_TABLE( J )%NAME ) // ' from '// 
     &           TRIM(CUNIT( SPC_UNIT )) // ' to ' // TRIM(CUNIT( OUT_UNIT )) //
     &           ' but cannot complete it because '//TRIM(CUNIT( OUT_UNIT ))//
     &           ' is not a supported option in ELMO. Please add it to '//
     &           'ELMO_UNIT_CONV.'
              CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
 
      END SELECT 
 
      RETURN

      END FUNCTION ELMO_UNIT_CONV

      END MODULE ELMO_DATA
