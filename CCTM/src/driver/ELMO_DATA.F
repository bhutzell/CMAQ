
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

      USE AERO_DATA, ONLY : N_MODE
      USE UTILIO_DEFN

      IMPLICIT NONE

#ifdef mpas
      INTEGER, PARAMETER :: M3REAL  =   5
#endif

      ! Define Logical values as T and F for the aerospc table
      Logical, Parameter, Private :: T = .true.
      Logical, Parameter, Private :: F = .false.

      LOGICAL, SAVE :: L_ELMO
      REAL,    SAVE :: ELMO_NSTEP = 0.
 
      ! Define ELMO Variable Types
      INTEGER, PARAMETER :: ET_CONC = 1, ET_AEROBULK = 2, ET_WDEP = 3, 
     &                      ET_DDEP = 4, ET_CMPST = 5, ET_CMPST_DEP = 6, 
     &                      ET_DRVD = 7, ET_AEROBULK_DEP = 8,
     &                      ET_ISAM_CONC = 11, ET_ISAM_AEROBULK = 12,
     &                      ET_ISAM_WDEP = 13, ET_ISAM_DDEP = 14,
     &                      ET_ISAM_CMPST= 15, ET_ISAM_CMPST_DEP = 16,
     &                      ET_ISAM_DRVD = 17, ET_ISAM_AEROBULK_DEP = 18,
     &                      ET_DDM_CONC = 21, ET_DDM_AEROBULK = 22,
     &                      ET_DDM_WDEP = 23, ET_DDM_DDEP = 24,
     &                      ET_DDM_CMPST= 25, ET_DDM_CMPST_DEP = 26,
     &                      ET_DDM_DRVD = 27, ET_DDM_AEROBULK_DEP = 28,
     &                      ET_AEROPROP = 30, ET_CHEM = 31, ET_MET = 32,
     &                      ET_PHOT = 33

      CHARACTER( 14 ), PARAMETER :: CETYPE(33) = 
     &    (/'CONC          ','BULK AERO     ','WET DEP       ',
     &      'DRY DEP       ','COMPOSITE CONC','COMPOSITE DEP ',
     &      'DERIVED CONC  ','BULK AERO DEP ','              ',
     &      '              ','ISAM CONC     ','ISAM BULK AERO',
     &      'ISAM WETDEP   ','ISAM DRY DEP  ','ISAM COMP CONC',
     &      'ISAM COMP DEP ','ISAM DERIVED  ','ISAM AERO DEP ',
     &      '              ','              ','DDM CONC      ',
     &      'DDM BULK AERO ','DDM WET DEP   ','DDM DRY DEP   ',
     &      'DDM COMP CONC ','DDM COMP DEP  ','DDM DERIVED   ',
     &      'DDM AERO DEP  ','              ','AERO PROPERTY ',
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
      INTEGER, PARAMETER :: ID_PM_MASS =     19
      INTEGER, PARAMETER :: ID_PMU_MASS =    20
      INTEGER, PARAMETER :: ID_PMF_MASS =    21
      INTEGER, PARAMETER :: ID_PMC_MASS =    22
      INTEGER, PARAMETER :: ID_PMNUC_MASS =  23
      INTEGER, PARAMETER :: ID_PMAIT_MASS =  24
      INTEGER, PARAMETER :: ID_PMACC_MASS =  25
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
      INTEGER, PARAMETER :: ID_AOD550 =      61                   
      INTEGER, PARAMETER :: ID_PMEXT550 =    62
      INTEGER, PARAMETER :: ID_RH =          63
      INTEGER, PARAMETER :: ID_TEMP =        64
      INTEGER, PARAMETER :: ID_SRFTEMP =     65
      INTEGER, PARAMETER :: ID_PRES =        66
      INTEGER, PARAMETER :: ID_GN2O5F =      67
      INTEGER, PARAMETER :: ID_GN2O5C =      68
      INTEGER, PARAMETER :: ID_YCLNO2F =     69
      INTEGER, PARAMETER :: ID_YCLNO2C =     70
      INTEGER, PARAMETER :: ID_GIEPOX =      71
      INTEGER, PARAMETER :: ID_KIEPOX =      72
      INTEGER, PARAMETER :: ID_GIMAE =       73
      INTEGER, PARAMETER :: ID_DZ    =       74
      INTEGER, PARAMETER :: ID_ZH    =       75
      INTEGER, PARAMETER :: ID_CFRAC =       76
      INTEGER, PARAMETER :: ID_PV    =       77
      INTEGER, PARAMETER :: ID_DENS  =       78
      INTEGER, PARAMETER :: ID_RHOJ  =       79
      
      ! Define Key IDs for special composites that are dependencies for
      ! existing derived variables.
      INTEGER, SAVE :: ID_PMF_SO4 = 0
      INTEGER, SAVE :: ID_PMF_NO3 = 0
      INTEGER, SAVE :: ID_PMF_NH4 = 0
      INTEGER, SAVE :: ID_PM25_SO4 = 0
      INTEGER, SAVE :: ID_PM25_NO3 = 0
      INTEGER, SAVE :: ID_PM25_NH4 = 0
      INTEGER, SAVE :: ID_PMF_HP = 0
      INTEGER, SAVE :: ID_PMF_H2O = 0
      INTEGER, SAVE :: ID_PMF_BENAPY = 0
      INTEGER, SAVE :: ID_GAS_BENAPY = 0

      TYPE ELDTP
        CHARACTER (LEN = 16) :: NAME      !Parameter Name
        INTEGER              :: ID        !ID Number in Case Statement
        INTEGER              :: VAR_TYPE  !ELMO Variable Type (e.g. ET_CONC, ET_WDEP, etc.)
        LOGICAL              :: L_MODE    !Is this property mode-expansive?
        CHARACTER (LEN = 16) :: UNIT      !Units for Parameter
        CHARACTER (LEN = 80) :: DESC      !Description
      END TYPE ELDTP

      INTEGER, PARAMETER :: N_ELMO_LIST = 79

      TYPE (ELDTP), PARAMETER :: ELMO_LIST( N_ELMO_LIST ) = (/

      ! Aerosol Parameters and Diagnostics
     &ELDTP( 'STDEV        ',ID_STDEV,      ET_AEROPROP, T, '      ', 'Mode standard deviation (dry and wet)' ),
     &ELDTP( 'DRY_DG       ',ID_DRY_DG,     ET_AEROPROP, T, 'um    ', 'Mode mean diameter (dry)' ),
     &ELDTP( 'WET_DG       ',ID_WET_DG,     ET_AEROPROP, T, 'um    ', 'Mode mean diameter (wet)' ),
     &ELDTP( 'WET_M2       ',ID_WET_M2,     ET_AEROPROP, T, 'm2 m-3', 'Mode 2nd moment (wet)' ),
     &ELDTP( 'DRY_M3       ',ID_DRY_M3,     ET_AEROPROP, T, 'm3 m-3', 'Mode 3rd moment (dry)' ),
     &ELDTP( 'WET_M3       ',ID_WET_M3,     ET_AEROPROP, T, 'm3 m-3', 'Mode 3rd moment (wet)' ),
     &ELDTP( 'PM_NUM       ',ID_PM_NUM,     ET_AEROPROP, F, 'N m-3' , 'Total Particle Number' ),
     &ELDTP( 'PMU_NUM      ',ID_PMU_NUM,    ET_AEROPROP, F, 'N m-3' , 'Ultrafine-Mode Particle Number' ),
     &ELDTP( 'PMF_NUM      ',ID_PMF_NUM,    ET_AEROPROP, F, 'N m-3' , 'Fine-Mode Particle Number' ),
     &ELDTP( 'PMC_NUM      ',ID_PMC_NUM,    ET_AEROPROP, F, 'N m-3' , 'Coarse-Mode Particle Number' ),
     &ELDTP( 'N10          ',ID_N10,        ET_AEROPROP, F, 'N m-3' , 'Total Particle Number greater than 10 nm' ),
     &ELDTP( 'N20          ',ID_N20,        ET_AEROPROP, F, 'N m-3' , 'Total Particle Number greater than 20 nm' ),
     &ELDTP( 'N40          ',ID_N40,        ET_AEROPROP, F, 'N m-3' , 'Total Particle Number greater than 40 nm' ),
     &ELDTP( 'N100         ',ID_N100,       ET_AEROPROP, F, 'N m-3' , 'Total Particle Number greater than 100 nm' ),
     &ELDTP( 'PM_SRF       ',ID_PM_SRF,     ET_AEROPROP, F, 'm2 m-3', 'Particle Surface Area' ),
     &ELDTP( 'PMU_SRF      ',ID_PMU_SRF,    ET_AEROPROP, F, 'm2 m-3', 'Ultrafine-Mode Surface Area' ),
     &ELDTP( 'PMF_SRF      ',ID_PMF_SRF,    ET_AEROPROP, F, 'm2 m-3', 'Fine-Mode Surface Area' ),
     &ELDTP( 'PMC_SRF      ',ID_PMC_SRF,    ET_AEROPROP, F, 'm2 m-3', 'Coarse-Mode Surface Area' ),
     &ELDTP( 'PM_MASS      ',ID_PM_MASS,    ET_DRVD,     F, 'ug m-3', 'Total Particle Mass' ),
     &ELDTP( 'PMU_MASS     ',ID_PMU_MASS,   ET_DRVD,     F, 'ug m-3', 'Ultrafine Particle Mass' ),
     &ELDTP( 'PMF_MASS     ',ID_PMF_MASS,   ET_DRVD,     F, 'ug m-3', 'Fine Particle Mass' ),
     &ELDTP( 'PMC_MASS     ',ID_PMC_MASS,   ET_DRVD,     F, 'ug m-3', 'Coarse Particle Mass' ),
     &ELDTP( 'PMNUC_MASS   ',ID_PMNUC_MASS, ET_DRVD,     F, 'ug m-3', 'Nucleation Mode Total PM' ),
     &ELDTP( 'PMAIT_MASS   ',ID_PMAIT_MASS, ET_DRVD,     F, 'ug m-3', 'Aitken Mode Total PM' ),
     &ELDTP( 'PMACC_MASS   ',ID_PMACC_MASS, ET_DRVD,     F, 'ug m-3', 'Accumulation Mode Total PM' ),
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
     &ELDTP( 'PMF_OC       ',ID_PMF_OC,     ET_DRVD,     F, 'ugC m-3','Fine-Mode Organic Carbon' ),
     &ELDTP( 'PMF_OA       ',ID_PMF_OA,     ET_DRVD,     F, 'ug m-3', 'Fine-Mode Organic Aerosol' ),
     &ELDTP( 'PMF_HPMOLAL  ',ID_PMF_HPM,    ET_DRVD,     F, 'mol L-1','Fine-Mode Concentration of H+ in Particle Water' ),
     &ELDTP( 'PMF_PH       ',ID_PMF_PH,     ET_AEROPROP, F, '1'     , 'Fine-Mode pH' ),
                                                  
      ! Organic Aerosol Relevant Aggregates          
     &ELDTP( 'PMF_POC      ',ID_PMF_POC,    ET_DRVD,     F, 'ugC m-3','Fine-Mode Primary Organic Carbon' ),
     &ELDTP( 'PMF_SOC      ',ID_PMF_SOC,    ET_DRVD,     F, 'ugC m-3','Fine-Mode Secondary Organic Carbon' ),
     &ELDTP( 'PMF_POA      ',ID_PMF_POA,    ET_DRVD,     F, 'ug m-3', 'Fine-Mode Primary Organic Aerosol' ),
     &ELDTP( 'PMF_SOA      ',ID_PMF_SOA,    ET_DRVD,     F, 'ug m-3', 'Fine-Mode Secondary Organic Aerosol' ),
     &ELDTP( 'PMF_OMOC     ',ID_PMF_OMOC,   ET_AEROPROP, F, '1     ','Fine-Mode OM/OC' ),
     &ELDTP( 'PMF_OTOC     ',ID_PMF_OtoC,   ET_AEROPROP, F, '1     ','Fine-Mode O:C' ),
     &ELDTP( 'PMF_ASOA     ',ID_PMF_ASOA,   ET_DRVD,     F, 'ug m-3', 'Fine-Mode Anthropogenic-VOC Derived OA' ),
     &ELDTP( 'PMF_BSOA     ',ID_PMF_BSOA,   ET_DRVD,     F, 'ug m-3', 'Fine-Mode Biogenic-VOC Derived OA' ),
                                                  
      ! AMS Speciation                            
     &ELDTP( 'PMAMS_OA     ',ID_AMS_OA,     ET_DRVD,     F, 'ug m-3', 'Organic Aerosol Collected by the AMS' ),
     &ELDTP( 'PMAMS_OTOC   ',ID_AMS_OTOC,   ET_DRVD,     F, 'ug ug-1','O:C Collected and Measured by the AMS' ),
                                                  
      ! PM1 Speciation                            
     &ELDTP( 'PM1_OC       ',ID_PM1_OC,     ET_DRVD,     F, 'ugC m-3','PM1 Organic Carbon' ),
     &ELDTP( 'PM1_OA       ',ID_PM1_OA,     ET_DRVD,     F, 'ug m-3', 'PM1 Organic Aerosol' ),
                                                  
      ! PM2.5 Speciation                          
     &ELDTP( 'PM25_OC      ',ID_PM25_OC,    ET_DRVD,     F, 'ugC m-3','PM2.5 Organic Carbon' ),
     &ELDTP( 'PM25_OA      ',ID_PM25_OA,    ET_DRVD,     F, 'ug m-3', 'PM2.5 Organic Aerosol' ),
     &ELDTP( 'BENAPY_FAERO ',ID_BENAPY_FAERO,ET_AEROPROP,F, '',       'Benzo-a-Pyrene Aerosol-Phase Fraction' ),
                                                          
      ! Other Special Variables and Properties            
     &ELDTP( 'PM25_FRM     ',ID_PM25_FRM,   ET_DRVD,     F, 'ug m-3', 'Federal Reference Method PM2.5' ),
     &ELDTP( 'PMF_FRM      ',ID_PMF_FRM,    ET_DRVD,     F, 'ug m-3', 'Federal Reference Method PMIJ' ),
     &ELDTP( 'AOD_550      ',ID_AOD550,     ET_PHOT,     F, '1     ', 'Aerosol Optical Depth at 550 nm -Angstrom interp' ),
     &ELDTP( 'PM_EXT_550   ',ID_PMEXT550,   ET_PHOT,     F, 'Mm-1  ', 'Aerosol Extinction at 550 nm -Angstrom interp' ),
     &ELDTP( 'RH           ',ID_RH,         ET_MET,      F, '1     ', 'Relative humidity' ),
     &ELDTP( 'TA           ',ID_TEMP,       ET_MET,      F, 'K     ', 'Temperature' ),
     &ELDTP( 'TSURF        ',ID_SRFTEMP,    ET_MET,      F, 'K     ', '2-meter Surface Temperature' ),
     &ELDTP( 'PRES         ',ID_PRES,       ET_MET,      F, 'Pa    ', 'Pressure' ),
     &ELDTP( 'GAMMA_N2O5   ',ID_GN2O5F,     ET_CHEM,     F, '1     ', 'Fine Mode N2O5 Heterogeneous rxn probability' ),
     &ELDTP( 'GAMMA_N2O5K  ',ID_GN2O5C,     ET_CHEM,     F, '1     ', 'Coarse Mode N2O5 Heterogeneous rxn probability' ),
     &ELDTP( 'YIELD_CLNO2  ',ID_YCLNO2F,    ET_CHEM,     F, '1     ', 'Fine Mode CLNO2 Heterogeneous reaction yield' ),
     &ELDTP( 'YIELD_CLNO2K ',ID_YCLNO2C,    ET_CHEM,     F, '1     ', 'Coarse Mode CLNO2 Heterogeneous reaction yield' ),
     &ELDTP( 'GAMMA_IEPOX  ',ID_GIEPOX,     ET_CHEM,     F, '1     ', 'IEPOX heterogeneous uptake coefficient' ),
     &ELDTP( 'K_IEPOX      ',ID_KIEPOX,     ET_CHEM,     F, 's-1   ', 'IEPOX 1st order particle phase reaction rate const' ),
     &ELDTP( 'GAMMA_IMAE   ',ID_GIMAE,      ET_CHEM,     F, '1     ', 'IMAE+HMML heterogeneous uptake coefficient' ),
     &ELDTP( 'DZ           ',ID_DZ,         ET_MET,      F, 'm     ', 'Height of each grid cell computed from top layer height' ),
     &ELDTP( 'ZH           ',ID_ZH,         ET_MET,      F, 'm     ', 'Height of grid cell midpoints (mass-weighted)' ),
     &ELDTP( 'CFRAC        ',ID_CFRAC,      ET_MET,      F, '      ', 'Cloud Fraction' ),
     &ELDTP( 'PV           ',ID_PV,         ET_MET,      F, '      ', 'Potential Vorticity' ),
     &ELDTP( 'DENS         ',ID_DENS,       ET_MET,      F, 'kg m-3', 'Advected Density' ),
     &ELDTP( 'RHOJ         ',ID_RHOJ,       ET_MET,      F, 'kg m-3', 'Advected Density x Jacobian/MSFX^2' ) /)
     

      TYPE ELMO_KEY_TYPE
          CHARACTER( 24 )             :: WORD    ! Keyword for selecting mulitple ELMO parameters
          INTEGER                     :: N       ! Number of Parameters associated with the Keyword
          CHARACTER( 16 ),ALLOCATABLE :: LIST(:) ! List of parameters
      END TYPE ELMO_KEY_TYPE

      TYPE (ELMO_KEY_TYPE), ALLOCATABLE, SAVE :: ELMO_KEY( : )
      INTEGER, SAVE :: N_KEY

      CHARACTER(32), ALLOCATABLE, SAVE :: INST_VARS_NML( : )
      CHARACTER(32), ALLOCATABLE, SAVE :: AVRG_VARS_NML( : )
      INTEGER, SAVE :: N_ELMO_INST, N_ELMO_AVRG

      LOGICAL, SAVE :: INST_ACTIVE, AVRG_ACTIVE
      INTEGER, SAVE :: INST_LAYER_TOP, INST_LAYER_BOT,
     &                 AVRG_LAYER_TOP, AVRG_LAYER_BOT
      
      INTEGER, SAVE :: NLAY_ELMO_INST
      INTEGER, SAVE :: NLAY_ELMO_AVRG

      LOGICAL, ALLOCATABLE, SAVE :: L_ELMO_TABLE_INST(:), L_ELMO_TABLE_AVRG(:),
     &                              L_ELMO_TABLE_USED(:)

      INTEGER, ALLOCATABLE, SAVE :: MAP_USEDtoTABLE( : ), MAP_TABLEtoUSED( : )
      INTEGER, ALLOCATABLE, SAVE :: MAP_AVRGtoUSED( : ), MAP_INSTtoUSED( : )
      INTEGER, ALLOCATABLE, SAVE :: MAP_TABLEtoAVRG( : ), MAP_TABLEtoINST( : )
      INTEGER, ALLOCATABLE, SAVE :: MAP_AVRGtoTABLE( : ), MAP_INSTtoTABLE( : )
      LOGICAL, ALLOCATABLE, SAVE :: L_CALCULATED( : )
      INTEGER, SAVE :: N_ELMO_USED

      INTEGER, ALLOCATABLE, SAVE :: ELMO_USED_IV( : )
      INTEGER, ALLOCATABLE, SAVE :: ELMO_USED_ID( : )
      INTEGER, ALLOCATABLE, SAVE :: ELMO_USED_IM( : )
      INTEGER, ALLOCATABLE, SAVE :: ELMO_USED_ISRC( : )

      ! Variables for Storing Values of ELMO Output      
      REAL, ALLOCATABLE, SAVE :: ELMO_INST( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: ELMO_AVRG( :,:,:,: )
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

      ! Arrays for Mapping Output variables to ELMO Table IDs
      TYPE ELMO_MAP_1D
          INTEGER, ALLOCATABLE       :: IND( : )
      END TYPE ELMO_MAP_1D
      TYPE ELMO_MAP_2D
          INTEGER, ALLOCATABLE       :: IND( :,: )
      END TYPE ELMO_MAP_2D

      TYPE( ELMO_MAP_1D ), ALLOCATABLE, SAVE :: 
     &      MAP_ELMO_IDtoTABLE_MODE(:), MAP_ELMO_IDtoTABLE_SOURCE(:)
      TYPE( ELMO_MAP_2D ), ALLOCATABLE, SAVE :: 
     &      MAP_ELMO_IDtoTABLE_MODE_SOURCE(:)

      INTEGER, ALLOCATABLE, SAVE :: N_ELMOTABLE_MODE(:,:),
     &                              N_ELMOTABLE_SOURCE(:,:)
     &                              
      INTEGER, ALLOCATABLE, SAVE :: MAP_ELMOTABLE(:,:)

      ! Variables for Aerosol Chemistry
      REAL, SAVE, ALLOCATABLE :: GAMMA_N2O5IJ( :,:,:  ) ! Fine modes N2O5 rx effic.
      REAL, SAVE, ALLOCATABLE :: GAMMA_N2O5K(  :,:,:  ) ! Coarse mode N2O5 rx effic.
      REAL, SAVE, ALLOCATABLE :: KPARTIEPOX (  :,:,:  ) ! IEPOX particle-phase reaction rate constant (sec-1)
      REAL, SAVE, ALLOCATABLE :: GAMMA_IEPOX(  :,:,:  ) ! IEPOX uptake coeff
      REAL, SAVE, ALLOCATABLE :: GAMMA_IMAE (  :,:,:  ) ! IMAE uptake coeff
      REAL, SAVE, ALLOCATABLE :: YCLNO2IJ   (  :,:,:  ) ! Yield CLNO2 in Fine modes 
      REAL, SAVE, ALLOCATABLE :: YCLNO2K    (  :,:,:  ) ! Yield CLNO2 in coarse mode 
  
      LOGICAL, SAVE           :: AERO_CHEM_SET = .FALSE.
 
      ! Define type for storing names, properties, and components of
      ! all ELMO variables 
      TYPE ELMO_TABLE_TYPE
          CHARACTER (LEN = 16) :: NAME        ! Variable Name ( as it appears on the ELMO_LIST, in CGRID, etc.)
          CHARACTER (LEN = 16) :: OUT_NAME    ! Variable Name that will be written to the output file
          INTEGER              :: OUT_TYPE    ! IOAPI Variable Type
          INTEGER              :: ID          ! ID Number mapping to specific calculation, CGRID index, AERO 
                                              !   index, Family, etc.
          INTEGER              :: VAR_TYPE    ! ELMO Variable Type (ELMO, CGRID, AEROBULK, FAM, WDEP, DDEP)
          INTEGER              :: IM          ! Mode of interest if mode-dependent
          INTEGER              :: SOURCE      ! Source Number for ISAM or DDM3D Variables
          CHARACTER (LEN = 16) :: CUNIT       ! Units in Character Format
          INTEGER              :: IUNIT       ! Units in Integer Format:
                                              !   0 = unspecified;  1 = 'ug m-3';  2 = 'ppmV' 
          CHARACTER (LEN = 80) :: DESC        ! Description for Output File
                                              
          REAL                 :: MW          ! Species molecular weight, if defined
          LOGICAL              :: L_COMP      ! Is this variable a linear combination of other variables
      END TYPE ELMO_TABLE_TYPE

      TYPE ELMO_COMP_TYPE
          INTEGER              :: N_VARS      ! Some variables are linear combinations of other variables.
                                              !   N_VARS is the number of components within such a combination. 
                                              !   This should not account for modes. For example, 
                                              !   to sum ASO4IJK, N_VARS would be 1 since only 
                                              !   ASO4 is being used.
          INTEGER, ALLOCATABLE :: TABID(:)    ! ID Number in ELMO_TABLE
          INTEGER, ALLOCATABLE :: ID(:)       ! Index in native array (e.g. CGRID, AEROSPC, SA_ACONC, etc.)
          INTEGER, ALLOCATABLE :: VAR_TYPE(:) ! ELMO Variable Type (ELMO, CGRID, AEROBULK, FAM, WDEP, DDEP)
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

      TYPE (ELMO_TABLE_TYPE), ALLOCATABLE, SAVE :: ELMO_TABLE( : ),
     &                          ELMO_INST_PARS(:), ELMO_AVRG_PARS(:)
      TYPE (ELMO_COMP_TYPE ), ALLOCATABLE, SAVE :: ELMO_COMP( : ) 

      INTEGER, SAVE :: N_ELMO_TABLE



      ! Define List of Possible Units for Output
      INTEGER, PARAMETER :: N_UNIT = 25
      CHARACTER( 10 ), PARAMETER :: CUNIT( N_UNIT ) = 
     &    (/'UG M-3    ','PPMV      ','UGC M-3   ','PPMC      ',
     &      'UGN M-3   ','PPMN      ','M-3       ','M2 M-3    ',
     &      'HA-1      ','M2 HA-1   ','KG HA-1   ','1         ',
     &      '          ','UM        ','M3 M-3    ','N M-3     ',
     &      'KG M-3    ','UMOL M-3  ','MOL L-1   ','UG UG-1   ',
     &      'K         ','PA        ','S-1       ','M         ',
     &      'MM-1      ' /)
      
      INTEGER, PARAMETER :: IU_UNSP    = 0   ! Unspecified Units
      INTEGER, PARAMETER :: IU_UGpM3   = 1   ! ug m-3
      INTEGER, PARAMETER :: IU_PPMV    = 2   ! ppmV
      INTEGER, PARAMETER :: IU_UGCpM3  = 3   ! ugC m-3
      INTEGER, PARAMETER :: IU_PPMC    = 4   ! ppmC
      INTEGER, PARAMETER :: IU_UGNpM3  = 5   ! ugN m-3
      INTEGER, PARAMETER :: IU_PPMN    = 6   ! ppmN
      INTEGER, PARAMETER :: IU_pM3     = 7   ! m-3
      INTEGER, PARAMETER :: IU_M2pM3   = 8   ! m2 m-3
      INTEGER, PARAMETER :: IU_pHA     = 9   ! ha-1
      INTEGER, PARAMETER :: IU_M2pHA   = 10  ! m2 ha-1
      INTEGER, PARAMETER :: IU_KGpHA   = 11  ! kg ha-1
      INTEGER, PARAMETER :: IU_1       = 12  ! 1
      INTEGER, PARAMETER :: IU_NONE    = 13  ! dimensionless
      INTEGER, PARAMETER :: IU_UM      = 14  ! um
      INTEGER, PARAMETER :: IU_M3pM3   = 15  ! m3 m-3
      INTEGER, PARAMETER :: IU_NpM3    = 16  ! N m-3
      INTEGER, PARAMETER :: IU_KGpM3   = 17  ! kg m-3
      INTEGER, PARAMETER :: IU_UMOLpM3 = 18  ! umol m-3
      INTEGER, PARAMETER :: IU_MOLpL   = 19  ! mol L-1
      INTEGER, PARAMETER :: IU_UGpUG   = 20  ! ug ug-1
      INTEGER, PARAMETER :: IU_K       = 21  ! K
      INTEGER, PARAMETER :: IU_PA      = 22  ! Pa
      INTEGER, PARAMETER :: IU_pS      = 23  ! s-1
      INTEGER, PARAMETER :: IU_M       = 24  ! m
      INTEGER, PARAMETER :: IU_pMM     = 25  ! Mm-1

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
       !                          Inlet Name D_Lo  D_Hi
       !                          ---------- ----  ----
     &        ELMO_INLET_TYPE( 'PM01    ', 0.0,  0.1 ),
     &        ELMO_INLET_TYPE( 'PM1     ', 0.0,  1.0 ),
     &        ELMO_INLET_TYPE( 'PM25    ', 0.0,  2.5 ),
     &        ELMO_INLET_TYPE( 'PM10    ', 0.0, 10.0 ),
     &        ELMO_INLET_TYPE( 'PM25TO10', 2.5, 10.0 ),
     &        ELMO_INLET_TYPE( 'PMAMS   ', 0.0,  0.0 ),
     &        ELMO_INLET_TYPE( 'INUM10  ', 0.01,1.0e3),
     &        ELMO_INLET_TYPE( 'INUM20  ', 0.02,1.0e3),
     &        ELMO_INLET_TYPE( 'INUM40  ', 0.04,1.0e3),
     &        ELMO_INLET_TYPE( 'INUM100 ', 0.1, 1.0e3) /)

        REAL, SAVE :: INLET_FRAC( N_INLET,N_MODE,2 )  ! Fraction of each aerosol mode 
                                                      !   collected by each inlet
                                                      !   Third dimension is 1
                                                      !     for wet and 2 for dry

        REAL, ALLOCATABLE, SAVE :: ELMO_AOD_550(:,:), ! total aerosol optical depth at 
                                                      !   550 nm (Angstrom Interpolation) 
     &                             ELMO_EXT_550(:,:,:)! aerosol extinction at 550 nm for layer [1/m]
        REAL, ALLOCATABLE, SAVE :: ELMO_WDEP(:,:,:) ! wet deposition accumulated per time step
        REAL, ALLOCATABLE, SAVE :: ELMO_DDEP(:,:,:) ! dry deposition accumulated per time step
        REAL, ALLOCATABLE, SAVE :: ELMO_ISAM_WDEP(:,:,:,:) ! wet deposition accumulated per time step
        REAL, ALLOCATABLE, SAVE :: ELMO_ISAM_DDEP(:,:,:,:) ! dry deposition accumulated per time step
        REAL, ALLOCATABLE, SAVE :: ELMO_DDM_WDEP(:,:,:,:) ! wet deposition accumulated per time step
        REAL, ALLOCATABLE, SAVE :: ELMO_DDM_DDEP(:,:,:,:) ! dry deposition accumulated per time step

        CONTAINS

!------------------------------------------------------------------------------------------            
        subroutine elmo_init_shared
!------------------------------------------------------------------------------------------            
        use grid_conf,  only : ncols, nrows, nlays
        use cgrid_spcs, only : N_SPC_WDEP, N_SPC_DEPV
#ifdef isam
        use sa_defn,    only : nspc_sa, ntag_sa
#endif
#ifdef sens
        use ddm3d_defn, only : npmax
#endif

        implicit none

        integer ios
        character(32) :: pname = 'ELMO_INIT_SHARED'

        allocate( elmo_aod_550(ncols, nrows ), stat=ios)
        call checkmem( ios, 'ELMO_AOD_550',pname )
        elmo_aod_550 = 0.0

        allocate( elmo_ext_550(ncols, nrows, nlays ), stat=ios)
        call checkmem( ios, 'ELMO_EXT_550',pname )
        elmo_ext_550 = 0.0

        allocate( elmo_wdep(ncols, nrows, n_spc_wdep+1 ), stat=ios)
        call checkmem( ios, 'ELMO_WDEP',pname )
        elmo_wdep = 0.0

        allocate( elmo_ddep(ncols, nrows, n_spc_depv ), stat=ios)
        call checkmem( ios, 'ELMO_DDEP',pname )
        elmo_ddep = 0.0

#ifdef isam
        allocate( elmo_isam_wdep(ncols, nrows, n_spc_wdep, ntag_sa ), stat=ios)
        call checkmem( ios, 'ELMO_ISAM_WDEP',pname )
        elmo_isam_wdep = 0.0

        allocate( elmo_isam_ddep(ncols, nrows, n_spc_depv, ntag_sa ), stat=ios)
        call checkmem( ios, 'ELMO_ISAM_DDEP',pname )
        elmo_isam_ddep = 0.0
#endif
#ifdef sens
        allocate( elmo_ddm_wdep(ncols, nrows, n_spc_wdep+1, npmax ), stat=ios)
        call checkmem( ios, 'ELMO_DDM_WDEP',pname )
        elmo_ddm_wdep = 0.0

        allocate( elmo_ddm_ddep(ncols, nrows, n_spc_depv, npmax ), stat=ios)
        call checkmem( ios, 'ELMO_DDM_DDEP',pname )
        elmo_ddm_ddep = 0.0
#endif

        end subroutine elmo_init_shared


!------------------------------------------------------------------------------------------            
      SUBROUTINE MAP_ELMO_NML( VARS_NML_IN, L_ELMO_TABLE, CFILE, IERROR )
!     This subroutine maps user input from the CMAQ_Control namelist to
!     the ELMO_TABLE to define which variables should be output. It
!     expands Keywords, maps to concentrations, deposition fluxes, and
!     families, with expansion for families if reuqested (using a * in
!     front of the family name).
!-------------------------------------------------------------------------

      USE util_Family_module

      IMPLICIT NONE

      CHARACTER(32), INTENT( IN ) :: VARS_NML_IN( : )
      LOGICAL, INTENT( OUT ) :: L_ELMO_TABLE( N_ELMO_TABLE )
      CHARACTER(*), INTENT(IN) :: CFILE
      INTEGER, INTENT( OUT ) :: IERROR
      CHARACTER(32), ALLOCATABLE :: VARS_NML( : )

      INTEGER N_END, N_NML
      LOGICAL EXPAND_NML
      INTEGER I1, I2, I3, IT, IFAM
      
      
      CHARACTER( 250 ) :: XMSG
      CHARACTER, PARAMETER :: PNAME = 'MAP_ELMO_NML'

      
      IERROR = 0

      ! Diagnose VARS_NML_IN Size
      N_NML = SIZE( VARS_NML_IN )
      N_END = INDEX1( '', N_NML, VARS_NML_IN ) - 1
      IF ( N_END .LE. 0 ) THEN
          WRITE( XMSG, '(A)' ),'ERROR: Undefined variables requested for ELMO output'
          CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
      END IF
      
      ! Initialize VARS_NML
      ALLOCATE( VARS_NML( 1000 ) )
      VARS_NML = ''
      VARS_NML( 1:N_END ) = VARS_NML_IN( 1:N_END )

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
            ! Add All ELMO_TABLE variables to the Instaneous File
            L_ELMO_TABLE( : ) = .TRUE.

         ELSE IF ( VARS_NML(1) .EQ. 'ALL_CONC' ) THEN
            ! Activate all CGRID species
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_CONC )
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
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_ISAM' ) THEN
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
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_DDM' ) THEN
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
            ! Activate all DDM Dry Depostion Fluxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ET_DDM_DDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
         
         ELSEIF ( I1 .GT. 0 ) THEN
            ! Expand Matched Keyword
            VARS_NML( N_END+1:N_END+ELMO_KEY( I1 )%N ) =
     &                              ELMO_KEY( I1 )%LIST
            N_END = N_END + ELMO_KEY( I1 )%N

         ELSE IF ( I2 .GT. 0 ) THEN
            ! First check for chemical families
            IF ( ELMO_TABLE( I2 )%VAR_TYPE .EQ. ET_CMPST ) THEN
              IF ( EXPAND_NML ) THEN
                 ! Expand the chemical family if the * was used
                 IFAM = INDEX1( VARS_NML(1), N_Chem_Fams, ChemFamilyName )
                 VARS_NML( N_END+1:N_END+ChemFamilyNum( IFAM ) ) =
     &                                   ChemFamilyMembers( IFAM,: )
                 N_END = N_END + ChemFamilyNum( IFAM )
              ELSE
                 ! Activate this family if no * was used
                 L_ELMO_TABLE( I2 ) = .TRUE.
              END IF
                
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
            ! Table. The OUT_NAMEs are very specific. They will always
            ! equal the unique identifier for an ELMO Variable, a
            ! raw concentration, deposition flux, mode-dependent
            ! variable (STDEV_AIT) or source-dependent variable 
            ! (e.g. PM25_ONROAD).
            L_ELMO_TABLE( I3 ) = .TRUE.
          
         ELSE
            ! Entry Doesn't Match any variable on the ELMO table. Crash the
            ! model.
            XMSG = 'An '//TRIM(CFILE)// ' ELMO variable ' // TRIM(VARS_NML(1)) //
     &             ' has been requested that does not exist.'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF

         ! Remove Family name after expansion
         VARS_NML( 1:N_END-1 ) = VARS_NML( 2:N_END )
         VARS_NML( N_END ) = ''
         N_END = N_END - 1
      END DO

      END SUBROUTINE MAP_ELMO_NML    

!------------------------------------------------------------------------------------------            
      SUBROUTINE ELMO_CHECK_LAYERS( LAYER_TOP, LAYER_BOT, FNAME )
!     This subroutine checks that the layers specified for an ELMO file
!     are allowed.
!-------------------------------------------------------------------------

      USE GRID_CONF, ONLY : NLAYS

      IMPLICIT NONE

      INTEGER, INTENT( INOUT )    :: LAYER_TOP
      INTEGER, INTENT( IN )       :: LAYER_BOT
      CHARACTER(4), INTENT( IN )  :: FNAME
      CHARACTER(250)              :: XMSG
      CHARACTER, PARAMETER        :: PNAME = 'ELMO_CHECK_LAYERS'

      IF ( LAYER_TOP .EQ. -1 ) LAYER_TOP = NLAYS
      IF ( LAYER_TOP .LE. 0 .OR. LAYER_TOP .GT. NLAYS ) THEN
           XMSG = TRIM( FNAME ) // '_LAYER_TOP must be prescribed between '//
     &            '1 and NLAYS, or set to -1 to automatically choose NLAYS.'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      IF ( LAYER_BOT .LE. 0 .OR. LAYER_BOT .GT. NLAYS ) THEN
           XMSG = TRIM(FNAME) // '_LAYER_BOT must be prescribed between '// 
     &            '1 and NLAYS.'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      IF ( LAYER_BOT .GT. LAYER_TOP ) THEN
           XMSG = TRIM(FNAME) // ' LAYER_BOT must be less than or equal '// 
     &            'to ' // TRIM(FNAME) // ' LAYER_TOP.'
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
      SUBROUTINE ELMO_UNIT_CONVERSION( J, OUT_UNIT, FAC )
!     This subroutine calculates the conversion factor from a component
!     variable to an output variable.
!-------------------------------------------------------------------------
 
      USE AEROMET_DATA, ONLY : MWAIR, AIRDENS

      IMPLICIT NONE
 
      INTEGER, INTENT( IN )  :: J
      INTEGER, INTENT( IN )  :: OUT_UNIT
      REAL,    INTENT( OUT ) :: FAC 

      INTEGER :: SPC_UNIT
      CHARACTER(200) :: XMSG

      ! Determine Unit Conversion Factors
      SPC_UNIT = ELMO_TABLE( J )%IUNIT
      
      IF ( SPC_UNIT .EQ. OUT_UNIT .OR. OUT_UNIT .EQ. 0 ) THEN
          FAC = 1.0
          RETURN
      END IF

      ! Check if this variable has a non-zero MW, which is needed
      ! for any unit conversions.
      IF ( ELMO_TABLE( J )%MW .EQ. 0.0 ) THEN
        XMSG = 'ELMO is attempting to convert variable ' // 
     &       TRIM( ELMO_TABLE( J )%NAME ) // ' to mole- or mass-based '//
     &       'units but the MW is equal to 0.0'
        CALL M3WARN( 'COMP_UNIT_CONVERSION', 0, 0, XMSG )
      END IF

      ! Calculate Unit Conversions
      SELECT CASE ( SPC_UNIT )
        CASE ( 1 ) ! mass-based
          FAC = FAC / ELMO_TABLE( J )%MW 
     &          * MWAIR / AIRDENS / 1000.0 ! to mole mixing ratio
        CASE ( 2 ) ! mole-based
          FAC = FAC * ELMO_TABLE( J )%MW
     &          / MWAIR * AIRDENS * 1000.0 ! to mass concentration
      END SELECT 
 
      END SUBROUTINE ELMO_UNIT_CONVERSION

!----------------------------------------------------------------------------      
      INTEGER FUNCTION FIND_ELMO_TABLE( IDG, IM, IV, ISRC )
!----------------------------------------------------------------------------      
!     Look up the index in the ELMO Table given the calculation ID,
!     variable type, mode number, and source number.
!----------------------------------------------------------------------------      

      USE RUNTIME_VARS, ONLY : LOGDEV, LOG_MESSAGE

      IMPLICIT NONE

      INTEGER :: IDG, IM, IV, ISRC
      INTEGER :: I, J, NM, NS

      CHARACTER( 200 ) :: XMSG

      IF ( IDG.EQ.0 .OR. IV.EQ.0 ) THEN
             WRITE( XMSG, '(A,A,I4,A,I2,A,A)' )
     &          'ELMO is searching for a variable on the ELMO Table with ', 
     &          'calculation ID ', IDG, ' and variable type ', IV, '. ',
     &          'Neither of these indices are allowed to be zero.'
         CALL LOG_MESSAGE( LOGDEV, XMSG )
         CALL M3EXIT( 'FIND_ELMO_TABLE', 0, 0, '', XSTAT1 )
      END IF

      NM = N_ELMOTABLE_MODE( IV,IDG )
      NS = N_ELMOTABLE_SOURCE( IV,IDG )

      ! Check if this combination of IDG and IV have multiple modes
      ! and/or sources. If so, look up Table ID on alternate maps.
      IF ( NM .GT. 0 .AND. NS .GT. 0 ) THEN
          ! Look up Table ID on multiple mode and source map
          IF ( IDG.EQ.0 .OR. IV.EQ.0 ) THEN
             WRITE( XMSG, '(A,A,I2,A,I2,A,A)' )
     &              'ELMO is searching for a variable on the ELMO Table with ',
     &              'mode number ', IM, ' and source number ', ISRC, '. ',
     &              'Neither of these indices are allowed to be zero.'
             CALL LOG_MESSAGE( LOGDEV, XMSG )
             CALL M3EXIT( 'FIND_ELMO_TABLE', 0, 0, '', XSTAT1 )
          END IF
          I = MAP_ELMOTABLE( IV,IDG )
          J = MAP_ELMO_IDtoTABLE_MODE_SOURCE( I )%IND( IM,ISRC )

      ELSE IF ( NM .GT. 0 ) THEN
          ! Look up Table ID on multiple mode map
          IF ( IDG.EQ.0 .OR. IV.EQ.0 ) THEN
             WRITE( XMSG, '(A,A,I2,A)' )
     &              'ELMO is searching for a variable on the ELMO Table with ',
     &              'mode number ', IM, '. This index is not allowed to be zero.'
             CALL LOG_MESSAGE( LOGDEV, XMSG )
             CALL M3EXIT( 'FIND_ELMO_TABLE', 0, 0, '', XSTAT1 )
          END IF
          I = MAP_ELMOTABLE( IV,IDG )
          J = MAP_ELMO_IDtoTABLE_MODE( I )%IND( IM )

      ELSE IF ( NS .GT. 0 ) THEN
          ! Look up Table ID on multiple source map
          IF ( IDG.EQ.0 .OR. IV.EQ.0 ) THEN
             WRITE( XMSG, '(A,A,I2,A)' )
     &              'ELMO is searching for a variable on the ELMO Table with ',
     &              'source number ', ISRC, ' This index is not allowed to be zero.'
             CALL LOG_MESSAGE( LOGDEV, XMSG )
             CALL M3EXIT( 'FIND_ELMO_TABLE', 0, 0, '', XSTAT1 )
          END IF
          I = MAP_ELMOTABLE( IV,IDG )
          J = MAP_ELMO_IDtoTABLE_SOURCE( I )%IND( ISRC )

      ELSE
          ! Look up Table ID on default map
          J = MAP_ELMOTABLE( IV,IDG )

      END IF

      FIND_ELMO_TABLE = J
      
      END FUNCTION FIND_ELMO_TABLE

      END MODULE ELMO_DATA
