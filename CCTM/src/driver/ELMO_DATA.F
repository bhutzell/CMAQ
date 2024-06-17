
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

      ! Define Indices for All ELMO Parameters
      ! The idea here is that parameters will be organized and
      ! calculated in the order of this list, not the ELMO_LIST. The
      ! ELMO_LIST is searchable via the Parameter names but they
      ! link to this list of integers so that each parameter can be
      ! easily referenced during the run without having to do string
      ! lookups. Please see the table defining the ELMO_LIST below 
      ! for a description of each of these indices.
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
      INTEGER, PARAMETER :: ID_PMC_SO4 =     40
      INTEGER, PARAMETER :: ID_PMC_NO3 =     41
      INTEGER, PARAMETER :: ID_PMC_NH4 =     42
      INTEGER, PARAMETER :: ID_PMC_CL =      43
      INTEGER, PARAMETER :: ID_PMC_NA =      44
      INTEGER, PARAMETER :: ID_PMC_MG =      45
      INTEGER, PARAMETER :: ID_PMC_K =       46
      INTEGER, PARAMETER :: ID_PMC_CA =      47
      INTEGER, PARAMETER :: ID_PMF_SO4 =     48
      INTEGER, PARAMETER :: ID_PMF_NO3 =     49
      INTEGER, PARAMETER :: ID_PMF_NH4 =     50
      INTEGER, PARAMETER :: ID_PMF_CL =      51
      INTEGER, PARAMETER :: ID_PMF_NA =      52
      INTEGER, PARAMETER :: ID_PMF_EC =      53
      INTEGER, PARAMETER :: ID_PMF_OC =      54
      INTEGER, PARAMETER :: ID_PMF_OA =      55
      INTEGER, PARAMETER :: ID_PMF_FE =      56
      INTEGER, PARAMETER :: ID_PMF_AL =      57
      INTEGER, PARAMETER :: ID_PMF_SI =      58
      INTEGER, PARAMETER :: ID_PMF_TI =      59
      INTEGER, PARAMETER :: ID_PMF_CA =      60
      INTEGER, PARAMETER :: ID_PMF_MG =      61
      INTEGER, PARAMETER :: ID_PMF_K  =      62
      INTEGER, PARAMETER :: ID_PMF_MN =      63
      INTEGER, PARAMETER :: ID_PMF_H2O =     64                
      INTEGER, PARAMETER :: ID_SOIL_IMPV =   65                
      INTEGER, PARAMETER :: ID_PMF_UNSP1 =   66                
      INTEGER, PARAMETER :: ID_PMF_UNSP2 =   67                
      INTEGER, PARAMETER :: ID_PMF_HP =      68                
      INTEGER, PARAMETER :: ID_PMF_HPM =     69                
      INTEGER, PARAMETER :: ID_PMF_PH =      70                
      INTEGER, PARAMETER :: ID_PMF_POC =     71                
      INTEGER, PARAMETER :: ID_PMF_SOC =     72
      INTEGER, PARAMETER :: ID_PMF_POA =     73
      INTEGER, PARAMETER :: ID_PMF_SOA =     74
      INTEGER, PARAMETER :: ID_PMF_NCOM =    75
      INTEGER, PARAMETER :: ID_PMF_OMOC =    76
      INTEGER, PARAMETER :: ID_PMF_OtoC =    77
      INTEGER, PARAMETER :: ID_PMF_ASOA =    78
      INTEGER, PARAMETER :: ID_PMF_BSOA =    79
      INTEGER, PARAMETER :: ID_PMF_CLDGLY =  80
      INTEGER, PARAMETER :: ID_PMF_ISOP =    81
      INTEGER, PARAMETER :: ID_PMF_IEPOX =   82
      INTEGER, PARAMETER :: ID_PMF_MTNSOA =  83
      INTEGER, PARAMETER :: ID_PMF_MTSOA =   84
      INTEGER, PARAMETER :: ID_AMS_SO4 =     85
      INTEGER, PARAMETER :: ID_AMS_NO3 =     86
      INTEGER, PARAMETER :: ID_AMS_NH4 =     87
      INTEGER, PARAMETER :: ID_AMS_CL =      88
      INTEGER, PARAMETER :: ID_AMS_OA =      89
      INTEGER, PARAMETER :: ID_AMS_OTOC =    90
      INTEGER, PARAMETER :: ID_PM1_SO4 =     91
      INTEGER, PARAMETER :: ID_PM1_NO3 =     92
      INTEGER, PARAMETER :: ID_PM1_NH4 =     93
      INTEGER, PARAMETER :: ID_PM1_CL =      94
      INTEGER, PARAMETER :: ID_PM1_NA =      95
      INTEGER, PARAMETER :: ID_PM1_EC =      96
      INTEGER, PARAMETER :: ID_PM1_OC =      97
      INTEGER, PARAMETER :: ID_PM1_OA =      98
      INTEGER, PARAMETER :: ID_PM1_MG =      99
      INTEGER, PARAMETER :: ID_PM1_K =      100
      INTEGER, PARAMETER :: ID_PM1_CA =     101
      INTEGER, PARAMETER :: ID_PM1_OT =     102
      INTEGER, PARAMETER :: ID_PM1_FE =     103
      INTEGER, PARAMETER :: ID_PM1_SI =     104
      INTEGER, PARAMETER :: ID_PM1_TI =     105
      INTEGER, PARAMETER :: ID_PM1_MN =     106
      INTEGER, PARAMETER :: ID_PM1_AL =     107
      INTEGER, PARAMETER :: ID_PM1_SOIL =   108
      INTEGER, PARAMETER :: ID_PM1_UNSP1 =  109
      INTEGER, PARAMETER :: ID_PM1_UN =     110
      INTEGER, PARAMETER :: ID_PM1_HP =     111
      INTEGER, PARAMETER :: ID_PM25_SO4 =   112
      INTEGER, PARAMETER :: ID_PM25_NO3 =   113
      INTEGER, PARAMETER :: ID_PM25_NH4 =   114
      INTEGER, PARAMETER :: ID_PM25_CL =    115
      INTEGER, PARAMETER :: ID_PM25_NA =    116
      INTEGER, PARAMETER :: ID_PM25_EC =    117
      INTEGER, PARAMETER :: ID_PM25_OC =    118
      INTEGER, PARAMETER :: ID_PM25_OA =    119
      INTEGER, PARAMETER :: ID_PM25_MG =    120
      INTEGER, PARAMETER :: ID_PM25_K =     121
      INTEGER, PARAMETER :: ID_PM25_CA =    122
      INTEGER, PARAMETER :: ID_PM25_OT =    123
      INTEGER, PARAMETER :: ID_PM25_FE =    124
      INTEGER, PARAMETER :: ID_PM25_SI =    125
      INTEGER, PARAMETER :: ID_PM25_TI =    126
      INTEGER, PARAMETER :: ID_PM25_MN =    127
      INTEGER, PARAMETER :: ID_PM25_AL =    128
      INTEGER, PARAMETER :: ID_PM25_SOIL =  129
      INTEGER, PARAMETER :: ID_PM25_UNSP1 = 130
      INTEGER, PARAMETER :: ID_PM25_UN =    131
      INTEGER, PARAMETER :: ID_PM25_HP =    132
      INTEGER, PARAMETER :: ID_PM25to10_SO4=133
      INTEGER, PARAMETER :: ID_PM25to10_NO3=134
      INTEGER, PARAMETER :: ID_PM25to10_NH4=135
      INTEGER, PARAMETER :: ID_PM25to10_CL =136
      INTEGER, PARAMETER :: ID_PM25to10_NA =137
      INTEGER, PARAMETER :: ID_PM25_HDSL   =138
      INTEGER, PARAMETER :: ID_PM25_HBE    =139
      INTEGER, PARAMETER :: ID_PM25_HCD    =140
      INTEGER, PARAMETER :: ID_PM25_HCR3   =141
      INTEGER, PARAMETER :: ID_PM25_HCR6   =142
      INTEGER, PARAMETER :: ID_PM25_HCR    =143
      INTEGER, PARAMETER :: ID_PM25_HPB    =144
      INTEGER, PARAMETER :: ID_PM25_HMN    =145
      INTEGER, PARAMETER :: ID_PM25_HNI    =146
      INTEGER, PARAMETER :: ID_PM25_HAS    =147
      INTEGER, PARAMETER :: ID_PM25_HG     =148
      INTEGER, PARAMETER :: ID_PM10_HDSL   =149
      INTEGER, PARAMETER :: ID_PM10_HBE    =150
      INTEGER, PARAMETER :: ID_PM10_HCD    =151
      INTEGER, PARAMETER :: ID_PM10_HCR3   =152
      INTEGER, PARAMETER :: ID_PM10_HCR6   =153
      INTEGER, PARAMETER :: ID_PM10_HCR    =154
      INTEGER, PARAMETER :: ID_PM10_HPB    =155
      INTEGER, PARAMETER :: ID_PM10_HMN    =156
      INTEGER, PARAMETER :: ID_PM10_HNI    =157
      INTEGER, PARAMETER :: ID_PM10_HAS    =158
      INTEGER, PARAMETER :: ID_PM10_HG     =159
      INTEGER, PARAMETER :: ID_PM25_BENAPY =160
      INTEGER, PARAMETER :: ID_PM10_BENAPY =161
      INTEGER, PARAMETER :: ID_PMF_BENAPY  =162
      INTEGER, PARAMETER :: ID_GAS_BENAPY  =163
      INTEGER, PARAMETER :: ID_BENAPY_FAERO=164
      INTEGER, PARAMETER :: ID_TNO3 =       165                     
      INTEGER, PARAMETER :: ID_TNO3_MOL =   166                     
      INTEGER, PARAMETER :: ID_TNO3TOT =    167                     
      INTEGER, PARAMETER :: ID_PM25_FRM =   168                     
      INTEGER, PARAMETER :: ID_PMF_FRM =    169                     
      INTEGER, PARAMETER :: ID_AOD550 =     160                     
      INTEGER, PARAMETER :: ID_PMEXT550 =   171
      INTEGER, PARAMETER :: ID_RH =         172
      INTEGER, PARAMETER :: ID_TEMP =       173
      INTEGER, PARAMETER :: ID_SRFTEMP =    174
      INTEGER, PARAMETER :: ID_PRES =       175
      INTEGER, PARAMETER :: ID_GN2O5F =     176
      INTEGER, PARAMETER :: ID_GN2O5C =     177
      INTEGER, PARAMETER :: ID_YCLNO2F =    178
      INTEGER, PARAMETER :: ID_YCLNO2C =    179
      INTEGER, PARAMETER :: ID_GIEPOX =     170
      INTEGER, PARAMETER :: ID_KIEPOX =     181
      INTEGER, PARAMETER :: ID_GIMAE =      182
      INTEGER, PARAMETER :: ID_DZ    =      183
      INTEGER, PARAMETER :: ID_ZH    =      184
      INTEGER, PARAMETER :: ID_CFRAC =      185
      INTEGER, PARAMETER :: ID_PV    =      186
      INTEGER, PARAMETER :: ID_DENS  =      187
      INTEGER, PARAMETER :: ID_RHOJ  =      188
      INTEGER, PARAMETER :: ID_GAS_HNO3 =   189
      INTEGER, PARAMETER :: ID_GAS_NH3  =   180
      INTEGER, PARAMETER :: ID_DD_OXN_NOX = 191
      INTEGER, PARAMETER :: ID_WD_OXN_NOX = 192
      INTEGER, PARAMETER :: ID_DD_OXN_TNO3= 193
      INTEGER, PARAMETER :: ID_WD_OXN_TNO3= 194
      INTEGER, PARAMETER :: ID_DD_OXN_PANT= 195
      INTEGER, PARAMETER :: ID_WD_OXN_PANT= 196
      INTEGER, PARAMETER :: ID_DD_OXN_ORGN= 197
      INTEGER, PARAMETER :: ID_WD_OXN_ORGN= 198
      INTEGER, PARAMETER :: ID_DD_OXN_OTHR= 199
      INTEGER, PARAMETER :: ID_WD_OXN_OTHR= 200
      INTEGER, PARAMETER :: ID_DD_OXN_TOT = 201
      INTEGER, PARAMETER :: ID_WD_OXN_TOT = 202
      INTEGER, PARAMETER :: ID_TD_OXN_TOT = 203
      INTEGER, PARAMETER :: ID_NOX        = 204


      TYPE ELDTP
        CHARACTER (LEN = 16) :: NAME      !Parameter Name
        INTEGER              :: ID        !ID Number in Case Statement
        INTEGER              :: OUT_TYPE  !Output Variable Type (e.g. M3REAL, etc.)
        LOGICAL              :: L_MODE    !Is this property mode-dependent?
        LOGICAL              :: L_ISAM    !Is this property available for ISAM
        LOGICAL              :: L_DDM3D   !Is this property available for DDM3D
        CHARACTER (LEN = 16) :: UNIT      !Units for Parameter
        CHARACTER (LEN = 80) :: DESC      !Description
      END TYPE ELDTP

      INTEGER, PARAMETER :: N_ELMO_LIST = 204

      TYPE (ELDTP), PARAMETER :: ELMO_LIST( N_ELMO_LIST ) = (/
      ! Gas-Phase Variables
     &ELDTP( 'NOX          ',ID_NOX,        M3REAL, F,T,T, 'ppmV  ', 'NOx Concentration' ),

      ! Aerosol Parameters and Diagnostics
     &ELDTP( 'STDEV        ',ID_STDEV,      M3REAL, T,F,F, '      ', 'Mode standard deviation (dry and wet)' ),
     &ELDTP( 'DRY_DG       ',ID_DRY_DG,     M3REAL, T,F,F, 'um    ', 'Mode mean diameter (dry)' ),
     &ELDTP( 'WET_DG       ',ID_WET_DG,     M3REAL, T,F,F, 'um    ', 'Mode mean diameter (wet)' ),
     &ELDTP( 'WET_M2       ',ID_WET_M2,     M3REAL, T,F,F, 'm2 m-3', 'Mode 2nd moment (wet)' ),
     &ELDTP( 'DRY_M3       ',ID_DRY_M3,     M3REAL, T,F,F, 'm3 m-3', 'Mode 3rd moment (dry)' ),
     &ELDTP( 'WET_M3       ',ID_WET_M3,     M3REAL, T,F,F, 'm3 m-3', 'Mode 3rd moment (wet)' ),
     &ELDTP( 'PM_NUM       ',ID_PM_NUM,     M3REAL, F,F,F, 'N m-3' , 'Total Particle Number' ),
     &ELDTP( 'PMU_NUM      ',ID_PMU_NUM,    M3REAL, F,F,F, 'N m-3' , 'Ultrafine-Mode Particle Number' ),
     &ELDTP( 'PMF_NUM      ',ID_PMF_NUM,    M3REAL, F,F,F, 'N m-3' , 'Fine-Mode Particle Number' ),
     &ELDTP( 'PMC_NUM      ',ID_PMC_NUM,    M3REAL, F,F,F, 'N m-3' , 'Coarse-Mode Particle Number' ),
     &ELDTP( 'N10          ',ID_N10,        M3REAL, F,F,F, 'N m-3' , 'Total Particle Number greater than 10 nm' ),
     &ELDTP( 'N20          ',ID_N20,        M3REAL, F,F,F, 'N m-3' , 'Total Particle Number greater than 20 nm' ),
     &ELDTP( 'N40          ',ID_N40,        M3REAL, F,F,F, 'N m-3' , 'Total Particle Number greater than 40 nm' ),
     &ELDTP( 'N100         ',ID_N100,       M3REAL, F,F,F, 'N m-3' , 'Total Particle Number greater than 100 nm' ),
     &ELDTP( 'PM_SRF       ',ID_PM_SRF,     M3REAL, F,F,F, 'm2 m-3', 'Particle Surface Area' ),
     &ELDTP( 'PMU_SRF      ',ID_PMU_SRF,    M3REAL, F,F,F, 'm2 m-3', 'Ultrafine-Mode Surface Area' ),
     &ELDTP( 'PMF_SRF      ',ID_PMF_SRF,    M3REAL, F,F,F, 'm2 m-3', 'Fine-Mode Surface Area' ),
     &ELDTP( 'PMC_SRF      ',ID_PMC_SRF,    M3REAL, F,F,F, 'm2 m-3', 'Coarse-Mode Surface Area' ),
     &ELDTP( 'PM_MASS      ',ID_PM_MASS,    M3REAL, F,T,T, 'ug m-3', 'Total Particle Mass' ),
     &ELDTP( 'PMU_MASS     ',ID_PMU_MASS,   M3REAL, F,T,T, 'ug m-3', 'Ultrafine Particle Mass' ),
     &ELDTP( 'PMF_MASS     ',ID_PMF_MASS,   M3REAL, F,T,T, 'ug m-3', 'Fine Particle Mass' ),
     &ELDTP( 'PMC_MASS     ',ID_PMC_MASS,   M3REAL, F,T,T, 'ug m-3', 'Coarse Particle Mass' ),
     &ELDTP( 'PMNUC_MASS   ',ID_PMNUC_MASS, M3REAL, F,T,T, 'ug m-3', 'Nucleation Mode Total PM' ),
     &ELDTP( 'PMAIT_MASS   ',ID_PMAIT_MASS, M3REAL, F,T,T, 'ug m-3', 'Aitken Mode Total PM' ),
     &ELDTP( 'PMACC_MASS   ',ID_PMACC_MASS, M3REAL, F,T,T, 'ug m-3', 'Accumulation Mode Total PM' ),
     &ELDTP( 'DRY_DENS     ',ID_DRY_DENS,   M3REAL, T,F,F, 'kg m-3', 'Bulk Density of Particles excluding particle water' ),
     &ELDTP( 'WET_DENS     ',ID_WET_DENS,   M3REAL, T,F,F, 'kg m-3', 'Bulk Density of Particles including particle water' ),
     &ELDTP( 'FPM01        ',ID_FPM01,      M3REAL, T,F,F, '1     ', 'PM01 fraction of mode' ),
     &ELDTP( 'FPM1         ',ID_FPM1,       M3REAL, T,F,F, '1     ', 'PM1 fraction of mode' ),
     &ELDTP( 'FPM25        ',ID_FPM25,      M3REAL, T,F,F, '1     ', 'PM2.5 fraction of mode' ),
     &ELDTP( 'FPM10        ',ID_FPM10,      M3REAL, T,F,F, '1     ', 'PM10 fraction of mode' ),
     &ELDTP( 'FPM25TO10    ',ID_FPM25to10,  M3REAL, T,F,F, '1     ', 'PM2.5-10 fraction of mode' ),
     &ELDTP( 'FAMS         ',ID_FAMS,       M3REAL, T,F,F, '1     ', 'AMS Transmission Fraction of mode' ),
     &ELDTP( 'PM01         ',ID_PM01,       M3REAL, F,T,T, 'ug m-3', 'Bulk PM0.1 Concentration' ), 
     &ELDTP( 'PM1          ',ID_PM1,        M3REAL, F,T,T, 'ug m-3', 'Bulk PM1.0 Concentration' ), 
     &ELDTP( 'PM25         ',ID_PM25,       M3REAL, F,T,T, 'ug m-3', 'Bulk PM2.5 Concentration' ), 
     &ELDTP( 'PM10         ',ID_PM10,       M3REAL, F,T,T, 'ug m-3', 'Bulk PM10.0 Concentration' ),
     &ELDTP( 'PM25TO10     ',ID_PM25to10,   M3REAL, F,T,T, 'ug m-3', 'Coarse-Mode Total' ),
     &ELDTP( 'PMAMS        ',ID_PMAMS,      M3REAL, F,T,T, 'ug m-3', 'Bulk PM Concentration in AMS' ),
                                                          
      ! Coarse-Mode PM Species                                 
     &ELDTP( 'PMC_SO4      ',ID_PMC_SO4,    M3REAL, F,T,T, 'ug m-3', 'Coarse-Mode Sulfate' ),
     &ELDTP( 'PMC_NO3      ',ID_PMC_NO3,    M3REAL, F,T,T, 'ug m-3', 'Coarse-Mode Nitrate' ),
     &ELDTP( 'PMC_NH4      ',ID_PMC_NH4,    M3REAL, F,T,T, 'ug m-3', 'Coarse-Mode Ammonium' ),
     &ELDTP( 'PMC_CL       ',ID_PMC_CL,     M3REAL, F,T,T, 'ug m-3', 'Coarse-Mode Chloride' ),
     &ELDTP( 'PMC_NA       ',ID_PMC_NA,     M3REAL, F,T,T, 'ug m-3', 'Recon Crs Sodium (.8373*ASEACAT+.0626*ASOIL+.0023*ACORS)' ),
     &ELDTP( 'PMC_MG       ',ID_PMC_MG,     M3REAL, F,T,T, 'ug m-3', 'Recon Crs Magnesium (.0997*ASEACAT+.0170*ASOIL+.0032*ACORS)'),
     &ELDTP( 'PMC_K        ',ID_PMC_K,      M3REAL, F,T,T, 'ug m-3', 'Recon Crs Potasium (.0310*ASEACAT+.0242*ASOIL+.0176*ACORS)' ),
     &ELDTP( 'PMC_CA       ',ID_PMC_CA,     M3REAL, F,T,T, 'ug m-3', 'Recon Crs Calcium (.0320*ASEACAT+.0838*ASOIL+.0562*ACORS)' ),
                                                          
      ! Fine-Mode PM Species                                 
     &ELDTP( 'PMF_SO4      ',ID_PMF_SO4,    M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Sulfate' ),
     &ELDTP( 'PMF_NO3      ',ID_PMF_NO3,    M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Nitrate' ),
     &ELDTP( 'PMF_NH4      ',ID_PMF_NH4,    M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Ammonium' ),
     &ELDTP( 'PMF_CL       ',ID_PMF_CL,     M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Chloride' ),
     &ELDTP( 'PMF_NA       ',ID_PMF_NA,     M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Sodium' ),
     &ELDTP( 'PMF_EC       ',ID_PMF_EC,     M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Black Carbon' ),
     &ELDTP( 'PMF_OC       ',ID_PMF_OC,     M3REAL, F,T,T, 'ugC m-3','Fine-Mode Organic Carbon' ),
     &ELDTP( 'PMF_OA       ',ID_PMF_OA,     M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Organic Aerosol' ),
     &ELDTP( 'PMF_FE       ',ID_PMF_FE,     M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Iron' ),
     &ELDTP( 'PMF_AL       ',ID_PMF_AL,     M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Aluminum' ),
     &ELDTP( 'PMF_SI       ',ID_PMF_SI,     M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Silicon' ),
     &ELDTP( 'PMF_TI       ',ID_PMF_TI,     M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Titanium' ),
     &ELDTP( 'PMF_CA       ',ID_PMF_CA,     M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Calcium' ),
     &ELDTP( 'PMF_MG       ',ID_PMF_MG,     M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Magnesium' ),
     &ELDTP( 'PMF_K        ',ID_PMF_K,      M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Potassium' ),
     &ELDTP( 'PMF_MN       ',ID_PMF_MN,     M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Manganese' ),
     &ELDTP( 'PMF_H2O      ',ID_PMF_H2O,    M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Particle Water' ),
     &ELDTP( 'PMF_SOILIMPV ',ID_SOIL_IMPV,  M3REAL, F,T,T, 'ug m-3', 'Recon of Soil PM from correlations at IMPROVE sites' ),
     &ELDTP( 'PMF_UN_IMPV1 ',ID_PMF_UNSP1,  M3REAL, F,T,T, 'ug m-3', 'Unspeciated PM including non-carbon organic mass' ),
     &ELDTP( 'PMF_UN_IMPV2 ',ID_PMF_UNSP2,  M3REAL, F,T,T, 'ug m-3', 'Unspeciated PM excluding non-carbon organic mass' ),
     &ELDTP( 'PMF_HP       ',ID_PMF_HP,     M3REAL, F,T,T, 'umol m-3','Fine-Mode H+ Ion' ),
     &ELDTP( 'PMF_HPMOLAL  ',ID_PMF_HPM,    M3REAL, F,T,T, 'mol L-1','Fine-Mode Concentration of H+ in Particle Water' ),
     &ELDTP( 'PMF_PH       ',ID_PMF_PH,     M3REAL, F,F,F, '1'     , 'Fine-Mode pH' ),
                                                          
      ! Organic Aerosol Relevant Aggregates                  
     &ELDTP( 'PMF_POC      ',ID_PMF_POC,    M3REAL, F,T,T, 'ugC m-3','Fine-Mode Primary Organic Carbon' ),
     &ELDTP( 'PMF_SOC      ',ID_PMF_SOC,    M3REAL, F,T,T, 'ugC m-3','Fine-Mode Secondary Organic Carbon' ),
     &ELDTP( 'PMF_POA      ',ID_PMF_POA,    M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Primary Organic Aerosol' ),
     &ELDTP( 'PMF_SOA      ',ID_PMF_SOA,    M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Secondary Organic Aerosol' ),
     &ELDTP( 'PMF_NCOM     ',ID_PMF_NCOM,   M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Non-Carbon Organic Matter in POA' ),
     &ELDTP( 'PMF_OMOC     ',ID_PMF_OMOC,   M3REAL, F,F,F, '1     ','Fine-Mode OM/OC' ),
     &ELDTP( 'PMF_OTOC     ',ID_PMF_OtoC,   M3REAL, F,F,F, '1     ','Fine-Mode O:C' ),
     &ELDTP( 'PMF_ASOA     ',ID_PMF_ASOA,   M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Anthropogenic-VOC Derived OA' ),
     &ELDTP( 'PMF_BSOA     ',ID_PMF_BSOA,   M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Biogenic-VOC Derived OA' ),
     &ELDTP( 'PMF_CLDGLY   ',ID_PMF_CLDGLY, M3REAL, F,T,T, 'ug m-3', 'Fine-Mode SOA from glyoxal and methylglyoxal' ),
     &ELDTP( 'PMF_ISOPSOA  ',ID_PMF_ISOP,   M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Isoprene SOA excl IEPOX SOA' ),
     &ELDTP( 'PMF_IEPOXSOA ',ID_PMF_IEPOX,  M3REAL, F,T,T, 'ug m-3', 'Fine-Mode IEPOX SOA' ),
     &ELDTP( 'PMF_MTNSOA   ',ID_PMF_MTNSOA, M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Monoterpene Nitrate SOA' ),
     &ELDTP( 'PMF_MTSOA    ',ID_PMF_MTSOA,  M3REAL, F,T,T, 'ug m-3', 'Fine-Mode Monoterpene SOA excl Nitrates' ),
                                                          
      ! AMS Speciation                                    
     &ELDTP( 'PMAMS_SO4    ',ID_AMS_SO4,    M3REAL, F,T,T, 'ug m-3', 'Sulfate Collected by the AMS' ),
     &ELDTP( 'PMAMS_NO3    ',ID_AMS_NO3,    M3REAL, F,T,T, 'ug m-3', 'Nitrate Collected by the AMS' ),
     &ELDTP( 'PMAMS_NH4    ',ID_AMS_NH4,    M3REAL, F,T,T, 'ug m-3', 'Ammonium Collected by the AMS' ),
     &ELDTP( 'PMAMS_CL     ',ID_AMS_CL,     M3REAL, F,T,T, 'ug m-3', 'Chloride Collected by the AMS' ),
     &ELDTP( 'PMAMS_OA     ',ID_AMS_OA,     M3REAL, F,T,T, 'ug m-3', 'Organic Aerosol Collected by the AMS' ),
     &ELDTP( 'PMAMS_OTOC   ',ID_AMS_OTOC,   M3REAL, F,T,T, 'ug ug-1','O:C Collected and Measured by the AMS' ),
                                                          
      ! PM1 Speciation                                    
     &ELDTP( 'PM1_SO4      ',ID_PM1_SO4,    M3REAL, F,T,T, 'ug m-3', 'PM1 Sulfate' ),
     &ELDTP( 'PM1_NO3      ',ID_PM1_NO3,    M3REAL, F,T,T, 'ug m-3', 'PM1 Nitrate' ),
     &ELDTP( 'PM1_NH4      ',ID_PM1_NH4,    M3REAL, F,T,T, 'ug m-3', 'PM1 Ammonium' ),
     &ELDTP( 'PM1_CL       ',ID_PM1_CL,     M3REAL, F,T,T, 'ug m-3', 'PM1 Chloride' ),
     &ELDTP( 'PM1_NA       ',ID_PM1_NA,     M3REAL, F,T,T, 'ug m-3', 'PM1 Sodium' ),
     &ELDTP( 'PM1_EC       ',ID_PM1_EC,     M3REAL, F,T,T, 'ug m-3', 'PM1 Black Carbon' ),
     &ELDTP( 'PM1_OC       ',ID_PM1_OC,     M3REAL, F,T,T, 'ugC m-3','PM1 Organic Carbon' ),
     &ELDTP( 'PM1_OA       ',ID_PM1_OA,     M3REAL, F,T,T, 'ug m-3', 'PM1 Organic Aerosol' ),
     &ELDTP( 'PM1_MG       ',ID_PM1_MG,     M3REAL, F,T,T, 'ug m-3', 'PM1 Magnesium' ),
     &ELDTP( 'PM1_K        ',ID_PM1_K,      M3REAL, F,T,T, 'ug m-3', 'PM1 Potassium' ),
     &ELDTP( 'PM1_CA       ',ID_PM1_CA,     M3REAL, F,T,T, 'ug m-3', 'PM1 Calcium' ),
     &ELDTP( 'PM1_OTHER    ',ID_PM1_OT,     M3REAL, F,T,T, 'ug m-3', 'PM1 Other Mass' ),
     &ELDTP( 'PM1_FE       ',ID_PM1_FE,     M3REAL, F,T,T, 'ug m-3', 'PM1 Iron' ),
     &ELDTP( 'PM1_SI       ',ID_PM1_SI,     M3REAL, F,T,T, 'ug m-3', 'PM1 Silicon' ),
     &ELDTP( 'PM1_TI       ',ID_PM1_TI,     M3REAL, F,T,T, 'ug m-3', 'PM1 Titanium' ),
     &ELDTP( 'PM1_MN       ',ID_PM1_MN,     M3REAL, F,T,T, 'ug m-3', 'PM1 Manganese' ),
     &ELDTP( 'PM1_AL       ',ID_PM1_AL,     M3REAL, F,T,T, 'ug m-3', 'PM1 Aluminum' ),
     &ELDTP( 'PM1_SOIL     ',ID_PM1_SOIL,   M3REAL, F,T,T, 'ug m-3', 'PM1 Recon Soil (Fine) and Explicit (Coarse)' ),
     &ELDTP( 'PM1_UNSP1    ',ID_PM1_UNSP1,  M3REAL, F,T,T, 'ug m-3', 'PM1 Unsp. Coarse Mass for the IMPROVE Method' ),
     &ELDTP( 'PM1_UNSPCRS  ',ID_PM1_UN,     M3REAL, F,T,T, 'ug m-3', 'PM1 Unspeciated Coarse Mass' ),
     &ELDTP( 'PM1_HP       ',ID_PM1_HP,     M3REAL, F,T,T, 'ug m-3', 'PM1 H+ Ion' ),
                                                          
      ! PM2.5 Speciation                                  
     &ELDTP( 'PM25_SO4     ',ID_PM25_SO4,   M3REAL, F,T,T, 'ug m-3', 'PM2.5 Sulfate' ),
     &ELDTP( 'PM25_NO3     ',ID_PM25_NO3,   M3REAL, F,T,T, 'ug m-3', 'PM2.5 Nitrate' ),
     &ELDTP( 'PM25_NH4     ',ID_PM25_NH4,   M3REAL, F,T,T, 'ug m-3', 'PM2.5 Ammonium' ),
     &ELDTP( 'PM25_CL      ',ID_PM25_CL,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 Chloride' ),
     &ELDTP( 'PM25_NA      ',ID_PM25_NA,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 Sodium' ),
     &ELDTP( 'PM25_EC      ',ID_PM25_EC,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 Black Carbon' ),
     &ELDTP( 'PM25_OC      ',ID_PM25_OC,    M3REAL, F,T,T, 'ugC m-3','PM2.5 Organic Carbon' ),
     &ELDTP( 'PM25_OA      ',ID_PM25_OA,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 Organic Aerosol' ),
     &ELDTP( 'PM25_MG      ',ID_PM25_MG,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 Magnesium' ),
     &ELDTP( 'PM25_K       ',ID_PM25_K,     M3REAL, F,T,T, 'ug m-3', 'PM2.5 Potassium' ),
     &ELDTP( 'PM25_CA      ',ID_PM25_CA,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 Calcium' ),
     &ELDTP( 'PM25_OTHER   ',ID_PM25_OT,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 Other Mass' ),
     &ELDTP( 'PM25_FE      ',ID_PM25_FE,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 Iron' ),
     &ELDTP( 'PM25_SI      ',ID_PM25_SI,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 Silicon' ),
     &ELDTP( 'PM25_TI      ',ID_PM25_TI,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 Titanium' ),
     &ELDTP( 'PM25_MN      ',ID_PM25_MN,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 Manganese' ),
     &ELDTP( 'PM25_AL      ',ID_PM25_AL,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 Aluminum' ),
     &ELDTP( 'PM25_SOIL    ',ID_PM25_SOIL,  M3REAL, F,T,T, 'ug m-3', 'PM2.5 Recon Soil (Fine) and Explicit (Coarse)' ),
     &ELDTP( 'PM25_UNSP1   ',ID_PM25_UNSP1, M3REAL, F,T,T, 'ug m-3', 'PM2.5 Unsp. Coarse Mass for the IMPROVE method' ),
     &ELDTP( 'PM25_UNSPCRS ',ID_PM25_UN,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 Unspeciated Coarse Mass' ),
     &ELDTP( 'PM25_HP      ',ID_PM25_HP,    M3REAL, F,T,T, 'ug m-3', 'PM2.5 H+ Ion' ),
                                                          
      ! PM2.5-10.0 Speciation                             
     &ELDTP( 'PM25TO10_SO4 ',ID_PM25to10_SO4,M3REAL,F,T,T, 'ug m-3', 'Coarse-Mode Sulfate' ),
     &ELDTP( 'PM25TO10_NO3 ',ID_PM25to10_NO3,M3REAL,F,T,T, 'ug m-3', 'Coarse-Mode Nitrate' ),
     &ELDTP( 'PM25TO10_NH4 ',ID_PM25to10_NH4,M3REAL,F,T,T, 'ug m-3', 'Coarse-Mode Ammonium' ),
     &ELDTP( 'PM25TO10_CL  ',ID_PM25to10_CL, M3REAL,F,T,T, 'ug m-3', 'Coarse-Mode Chloride' ),
     &ELDTP( 'PM25TO10_NA  ',ID_PM25to10_NA, M3REAL,F,T,T, 'ug m-3', 'Coarse-Mode Sodium' ),
                                                          
      ! PM Toxics that are part of the                    
      !   Multipollutant (mp) Mechanism                   
     &ELDTP( 'PM25_HDIESEL ',ID_PM25_HDSL,   M3REAL,F,T,T, 'ug m-3', 'PM2.5 Diesel Total from HAP inventory' ),
     &ELDTP( 'PM25_HBE     ',ID_PM25_HBE,    M3REAL,F,T,T, 'ug m-3', 'PM2.5 Beryllium from HAP inventory' ),
     &ELDTP( 'PM25_HCD     ',ID_PM25_HCD,    M3REAL,F,T,T, 'ug m-3', 'PM2.5 Cadmium from HAP inventory' ),
     &ELDTP( 'PM25_HCR3    ',ID_PM25_HCR3,   M3REAL,F,T,T, 'ug m-3', 'PM2.5 Chromium-III from HAP inventory' ),
     &ELDTP( 'PM25_HCR6    ',ID_PM25_HCR6,   M3REAL,F,T,T, 'ug m-3', 'PM2.5 Chromium-VI from HAP inventory' ),
     &ELDTP( 'PM25_HCR     ',ID_PM25_HCR,    M3REAL,F,T,T, 'ug m-3', 'PM2.5 Chromium Total from HAP inventory' ),
     &ELDTP( 'PM25_HPB     ',ID_PM25_HPB,    M3REAL,F,T,T, 'ug m-3', 'PM2.5 Lead from HAP inventory' ),
     &ELDTP( 'PM25_HMN     ',ID_PM25_HMN,    M3REAL,F,T,T, 'ug m-3', 'PM2.5 Manganese from HAP inventory' ),
     &ELDTP( 'PM25_HNI     ',ID_PM25_HNI,    M3REAL,F,T,T, 'ug m-3', 'PM2.5 Nickel from HAP inventory' ),
     &ELDTP( 'PM25_HAS     ',ID_PM25_HAS,    M3REAL,F,T,T, 'ug m-3', 'PM2.5 Arsenic from HAP inventory' ),
     &ELDTP( 'PM25_HG      ',ID_PM25_HG,     M3REAL,F,T,T, 'ug m-3', 'PM2.5 Mercury from HAP inventory' ),
     &ELDTP( 'PM10_HDIESEL ',ID_PM10_HDSL,   M3REAL,F,T,T, 'ug m-3', 'PM10.0 Diesel Total from HAP inventory' ),
     &ELDTP( 'PM10_HBE     ',ID_PM10_HBE,    M3REAL,F,T,T, 'ug m-3', 'PM10.0 Beryllium from HAP inventory' ),
     &ELDTP( 'PM10_HCD     ',ID_PM10_HCD,    M3REAL,F,T,T, 'ug m-3', 'PM10.0 Cadmium from HAP inventory' ),
     &ELDTP( 'PM10_HCR3    ',ID_PM10_HCR3,   M3REAL,F,T,T, 'ug m-3', 'PM10.0 Chromium-III from HAP inventory' ),
     &ELDTP( 'PM10_HCR6    ',ID_PM10_HCR6,   M3REAL,F,T,T, 'ug m-3', 'PM10.0 Chromium-VI from HAP inventory' ),
     &ELDTP( 'PM10_HCR     ',ID_PM10_HCR,    M3REAL,F,T,T, 'ug m-3', 'PM10.0 Chromium Total from HAP inventory' ),
     &ELDTP( 'PM10_HPB     ',ID_PM10_HPB,    M3REAL,F,T,T, 'ug m-3', 'PM10.0 Lead from HAP inventory' ),
     &ELDTP( 'PM10_HMN     ',ID_PM10_HMN,    M3REAL,F,T,T, 'ug m-3', 'PM10.0 Manganese from HAP inventory' ),
     &ELDTP( 'PM10_HNI     ',ID_PM10_HNI,    M3REAL,F,T,T, 'ug m-3', 'PM10.0 Nickel from HAP inventory' ),
     &ELDTP( 'PM10_HAS     ',ID_PM10_HAS,    M3REAL,F,T,T, 'ug m-3', 'PM10.0 Arsenic from HAP inventory' ),
     &ELDTP( 'PM10_HG      ',ID_PM10_HG,     M3REAL,F,T,T, 'ug m-3', 'PM10.0 Mercury from HAP inventory' ),
     &ELDTP( 'PM25_BENAPY  ',ID_PM25_BENAPY, M3REAL,F,T,T, 'ug m-3', 'PM2.5 Benzo-a-Pyrene' ),
     &ELDTP( 'PM10_BENAPY  ',ID_PM10_BENAPY, M3REAL,F,T,T, 'ug m-3', 'PM10.0 Benzo-a-Pyrene' ),
     &ELDTP( 'PMF_BENAPY   ',ID_PMF_BENAPY,  M3REAL,F,T,T, 'ug m-3', 'Fine-Mode Benzo-a-Pyrene' ),
     &ELDTP( 'GAS_BENAPY   ',ID_GAS_BENAPY,  M3REAL,F,T,T, 'ug m-3', 'Gas-Phase Benzo-a-Pyrene' ),
     &ELDTP( 'BENAPY_FAERO ',ID_BENAPY_FAERO,M3REAL,F,F,F, '',       'Benzo-a-Pyrene Aerosol-Phase Fraction' ),
     &ELDTP( 'GAS_HNO3     ',ID_GAS_HNO3,    M3REAL,F,T,T, 'ug m-3', 'Gas-Phase Nitric Acid' ),
     &ELDTP( 'GAS_NH3      ',ID_GAS_NH3,     M3REAL,F,T,T, 'ug m-3', 'Gas-Phase Ammonia' ),
                                                          
      ! Deposition Fluxes                                 
     &ELDTP( 'DD_OXN_NOX   ',ID_DD_OXN_NOX,  M3REAL,F,T,T, 'kg ha-1','Dry Deposition of Oxidized Nitrogen from NOx ' ),
     &ELDTP( 'WD_OXN_NOX   ',ID_WD_OXN_NOX,  M3REAL,F,T,T, 'kg ha-1','Wet Deposition of Oxidized Nitrogen from NOx ' ),
     &ELDTP( 'DD_OXN_TNO3  ',ID_DD_OXN_TNO3, M3REAL,F,T,T, 'kg ha-1','Dry Deposition of Oxidized Nitrogen from Total Nitrate ' ),
     &ELDTP( 'WD_OXN_TNO3  ',ID_WD_OXN_TNO3, M3REAL,F,T,T, 'kg ha-1','Wet Deposition of Oxidized Nitrogen from Total Nitrate ' ),
     &ELDTP( 'DD_OXN_PANT  ',ID_DD_OXN_PANT, M3REAL,F,T,T, 'kg ha-1','Dry Deposition of Oxidized Nitrogen from PAN ' ),
     &ELDTP( 'WD_OXN_PANT  ',ID_WD_OXN_PANT, M3REAL,F,T,T, 'kg ha-1','Wet Deposition of Oxidized Nitrogen from PAN ' ),
     &ELDTP( 'DD_OXN_ORGN  ',ID_DD_OXN_ORGN, M3REAL,F,T,T, 'kg ha-1','Dry Deposition of Oxidized Nitrogen from Organic Nitrate ' ),
     &ELDTP( 'WD_OXN_ORGN  ',ID_WD_OXN_ORGN, M3REAL,F,T,T, 'kg ha-1','Wet Deposition of Oxidized Nitrogen from Organic Nitrate ' ),
     &ELDTP( 'DD_OXN_OTHR  ',ID_DD_OXN_OTHR, M3REAL,F,T,T, 'kg ha-1','Dry Deposition of Oxidized Nitrogen from Other ' ),
     &ELDTP( 'WD_OXN_OTHR  ',ID_WD_OXN_OTHR, M3REAL,F,T,T, 'kg ha-1','Wet Deposition of Oxidized Nitrogen from Other ' ),
     &ELDTP( 'DD_OXN_TOT   ',ID_DD_OXN_TOT,  M3REAL,F,T,T, 'kg ha-1','Total Dry Deposition of Oxidized Nitrogen ' ),
     &ELDTP( 'WD_OXN_TOT   ',ID_WD_OXN_TOT,  M3REAL,F,T,T, 'kg ha-1','Total Wet Deposition of Oxidized Nitrogen ' ),
     &ELDTP( 'TD_OXN_TOT   ',ID_TD_OXN_TOT,  M3REAL,F,T,T, 'kg ha-1','Total Wet+Dry Deposition of Oxidized Nitrogen ' ),
                                                          
      ! Other Special Variables and Properties            
     &ELDTP( 'TNO3         ',ID_TNO3,       M3REAL, F,T,T, 'ug m-3', 'Total Nitrate only Including Inorganics' ),
     &ELDTP( 'TNO3_MOL     ',ID_TNO3_MOL,   M3REAL, F,T,T, 'ppmV'  , 'Total Nitrate only Including Inorganics' ),
     &ELDTP( 'TNO3TOT      ',ID_TNO3TOT,    M3REAL, F,T,T, 'ug m-3', 'Total Nitrate including Inorganics and Organics' ),
     &ELDTP( 'PM25_FRM     ',ID_PM25_FRM,   M3REAL, F,T,T, 'ug m-3', 'Federal Reference Method PM2.5' ),
     &ELDTP( 'PMF_FRM      ',ID_PMF_FRM,    M3REAL, F,T,T, 'ug m-3', 'Federal Reference Method PMIJ' ),
     &ELDTP( 'AOD_550      ',ID_AOD550,     M3REAL, F,F,F, '1     ', 'Aerosol Optical Depth at 550 nm -Angstrom interp' ),
     &ELDTP( 'PM_EXT_550   ',ID_PMEXT550,   M3REAL, F,F,F, 'Mm-1  ', 'Aerosol Extinction at 550 nm -Angstrom interp' ),
     &ELDTP( 'RH           ',ID_RH,         M3REAL, F,F,F, '1     ', 'Relative humidity' ),
     &ELDTP( 'TA           ',ID_TEMP,       M3REAL, F,F,F, 'K     ', 'Temperature' ),
     &ELDTP( 'TSURF        ',ID_SRFTEMP,    M3REAL, F,F,F, 'K     ', '2-meter Surface Temperature' ),
     &ELDTP( 'PRES         ',ID_PRES,       M3REAL, F,F,F, 'Pa    ', 'Pressure' ),
     &ELDTP( 'GAMMA_N2O5   ',ID_GN2O5F,     M3REAL, F,F,F, '1     ', 'Fine Mode N2O5 Heterogeneous rxn probability' ),
     &ELDTP( 'GAMMA_N2O5K  ',ID_GN2O5C,     M3REAL, F,F,F, '1     ', 'Coarse Mode N2O5 Heterogeneous rxn probability' ),
     &ELDTP( 'YIELD_CLNO2  ',ID_YCLNO2F,    M3REAL, F,F,F, '1     ', 'Fine Mode CLNO2 Heterogeneous reaction yield' ),
     &ELDTP( 'YIELD_CLNO2K ',ID_YCLNO2C,    M3REAL, F,F,F, '1     ', 'Coarse Mode CLNO2 Heterogeneous reaction yield' ),
     &ELDTP( 'GAMMA_IEPOX  ',ID_GIEPOX,     M3REAL, F,F,F, '1     ', 'IEPOX heterogeneous uptake coefficient' ),
     &ELDTP( 'K_IEPOX      ',ID_KIEPOX,     M3REAL, F,F,F, 's-1   ', 'IEPOX 1st order particle phase reaction rate const' ),
     &ELDTP( 'GAMMA_IMAE   ',ID_GIMAE,      M3REAL, F,F,F, '1     ', 'IMAE+HMML heterogeneous uptake coefficient' ),
     &ELDTP( 'DZ           ',ID_DZ,         M3REAL, F,F,F, 'm     ', 'Height of each grid cell computed from top layer height' ),
     &ELDTP( 'ZH           ',ID_ZH,         M3REAL, F,F,F, 'm     ', 'Height of grid cell midpoints (mass-weighted)' ),
     &ELDTP( 'CFRAC        ',ID_CFRAC,      M3REAL, F,F,F, '      ', 'Cloud Fraction' ),
     &ELDTP( 'PV           ',ID_PV,         M3REAL, F,F,F, '      ', 'Potential Vorticity' ),
     &ELDTP( 'DENS         ',ID_DENS,       M3REAL, F,F,F, 'kg m-3', 'Advected Density' ),
     &ELDTP( 'RHOJ         ',ID_RHOJ,       M3REAL, F,F,F, 'kg m-3', 'Advected Density x Jacobian/MSFX^2' ) /)
     

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
      REAL, ALLOCATABLE, SAVE :: ISAM_WDEP_VEC( : )
      REAL, ALLOCATABLE, SAVE :: ISAM_DDEP_VEC( : )
#endif
#ifdef sens
      REAL, ALLOCATABLE, SAVE :: DDM_WDEP_VEC( : )
      REAL, ALLOCATABLE, SAVE :: DDM_DDEP_VEC( : )
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
          LOGICAL              :: L_VALID     ! True (Default) if all components exist in simulation. 
                                              !   False if any components cannot be found
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

      ! Define Parameters indicating what kind of variable each ELMO
      ! output is
      INTEGER, PARAMETER :: ETYPE_ELMO = 1, ETYPE_CGRID = 2,
     &                      ETYPE_AEROBULK = 3, ETYPE_FAM = 4, 
     &                      ETYPE_WDEP = 5, ETYPE_DDEP = 6,
     &                      ETYPE_ISAM_ELMO = 7, ETYPE_ISAM = 8, 
     &                      ETYPE_ISAM_AEROBULK = 9, ETYPE_ISAM_FAM = 10,
     &                      ETYPE_ISAM_WDEP = 11, ETYPE_ISAM_DDEP = 12,
     &                      ETYPE_DDM_ELMO = 13, ETYPE_DDM = 14, 
     &                      ETYPE_DDM_AEROBULK = 15, ETYPE_DDM_FAM = 16,
     &                      ETYPE_DDM_WDEP = 17, ETYPE_DDM_DDEP = 18

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
        allocate( elmo_isam_wdep(ncols, nrows, nspc_sa, ntag_sa ), stat=ios)
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
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ETYPE_CGRID )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_WDEP' ) THEN
            ! Activate all Wet Deposition Fluxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ETYPE_WDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_DDEP' ) THEN
            ! Activate all Dry Deposition FLuxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ETYPE_DDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_ISAM' ) THEN
            ! Activate all ISAM Species
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ETYPE_ISAM )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_ISAM_WDEP' ) THEN
            ! Activate all ISAM Wet Deposition Fluxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ETYPE_ISAM_WDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_ISAM_DDEP' ) THEN
            ! Activate all ISAM Dry Deposition Fluxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ETYPE_ISAM_DDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_DDM' ) THEN
            ! Activate all DDM3D Species
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ETYPE_DDM )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_DDM_WDEP' ) THEN
            ! Activate all DDM Wet Deposition Fluxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ETYPE_DDM_WDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
          
         ELSE IF ( VARS_NML(1) .EQ. 'ALL_DDM_DDEP' ) THEN
            ! Activate all DDM Dry Depostion Fluxes
            DO IT = 1,N_ELMO_TABLE
                IF ( ELMO_TABLE( IT )%VAR_TYPE .EQ. ETYPE_DDM_DDEP )
     &                  L_ELMO_TABLE( IT ) = .TRUE.
            END DO
         
         ELSEIF ( I1 .GT. 0 ) THEN
            ! Expand Matched Keyword
            VARS_NML( N_END+1:N_END+ELMO_KEY( I1 )%N ) =
     &                              ELMO_KEY( I1 )%LIST
            N_END = N_END + ELMO_KEY( I1 )%N

         ELSE IF ( I2 .GT. 0 ) THEN
            ! First check for chemical families
            IF ( ELMO_TABLE( I2 )%VAR_TYPE .EQ. ETYPE_FAM ) THEN
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
      SUBROUTINE INIT_ELMO_COMP( J, N_VARS, AEROTYPE_TMP )
!     This subroutine allocates and initalizes all of the elements of an 
!        index IDG within the ELMO_COEFFS structure
!
!     IDG - the index of this ELMO variable in the space defined
!           by the integer index variables like ID_PMF_POC, etc.
!     N_VARS - Number of variables used to create this aggregate variable. 
!           Does not include the number of modes. For example, ASO4I+ASO4J 
!           counts as 1 variable (ASO4). It will be expanded to modes later.
!     AEROTYPE - Aerosol size range or mode to be calculated. This defines 
!           the modes considered and the inlet type if any. Options
!           include:
!             ALL, NONE, ULTRAFINE, FINE, COARSE, NUCLEATION, AITKEN,
!             ACCUMULATION, PM01, PM1, PM25, PM10, PM25to10 and PMAMS
!     
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY : N_MODE, AEROMODE

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: J 
      INTEGER, INTENT( IN ) :: N_VARS
      CHARACTER( * ), INTENT( IN ) ::  AEROTYPE_TMP
      CHARACTER( 20 ) AEROTYPE
      INTEGER VAR_TYPE, ISRC

      CHARACTER( 200 ) XMSG
      CHARACTER(16), SAVE :: PNAME = 'INIT_ELMO_COEF'
      INTEGER ALLOCSTAT

      ! Initialize All Components of ELMO_COEFFS
      ELMO_TABLE( J )%L_COMP = .TRUE.
      ELMO_COMP( J )%N_VARS = N_VARS
      ALLOCATE( ELMO_COMP( J )%L_MODE  ( N_MODE ),
     &          ELMO_COMP( J )%L_SOURCE( N_VARS ),
     &          ELMO_COMP( J )%MW      ( N_VARS ),
     &          ELMO_COMP( J )%SPC_UNIT( N_VARS ),
     &          ELMO_COMP( J )%ID      ( N_VARS ),
     &          ELMO_COMP( J )%TABID   ( N_VARS ),
     &          ELMO_COMP( J )%VAR_TYPE( N_VARS ),
     &          ELMO_COMP( J )%COEFF   ( N_VARS ),
     &          STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_COMP' 
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_COMP( J )%L_VALID        = .TRUE.
      ELMO_COMP( J )%L_MODE  ( : )  = .FALSE.
      ELMO_COMP( J )%L_SOURCE( : )  = .FALSE.
      ELMO_COMP( J )%MW      ( : )  = 0.
      ELMO_COMP( J )%SPC_UNIT( : )  = 0
      ELMO_COMP( J )%ID      ( : )  = 0
      ELMO_COMP( J )%TABID   ( : )  = 0
      ELMO_COMP( J )%VAR_TYPE( : )  = 0
      ELMO_COMP( J )%COEFF   ( : )  = 0.
      ELMO_COMP( J )%INLET          = 0
      ELMO_COMP( J )%WETNESS        = IWET

      ! Specify the Aerosol Modes Needed and Inlet Type based on User
      ! Input through the AEROTYPE variable.
      AEROTYPE = AEROTYPE_TMP
      CALL UPCASE( AEROTYPE )
      SELECT CASE ( AEROTYPE )
         CASE ( 'ALL' )
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
         CASE ('NONE' )
             ELMO_COMP( J )%L_MODE( : ) = .FALSE.
         CASE ('ULTRAFINE' )
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%ULTRAFINE_MASK
         CASE ('FINE' )
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%FINE_MASK
         CASE ('COARSE' )
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%COARSE_MASK
         CASE ('NUCLEATION' )
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%NUC_MASK
         CASE ('AITKEN' )
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%AITKEN_MASK
         CASE ('ACCUMULATION' )
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%ACCUM_MASK
         CASE ('PM01' )
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
             ELMO_COMP( J )%INLET = IPM01
             ELMO_COMP( J )%WETNESS = IWET
         CASE ('PM1' )
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
             ELMO_COMP( J )%INLET = IPM1
             ELMO_COMP( J )%WETNESS = IWET
         CASE ('PM25' )
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
             ELMO_COMP( J )%INLET = IPM25
             ELMO_COMP( J )%WETNESS = IWET
         CASE ('PM10' )
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
             ELMO_COMP( J )%INLET = IPM10
             ELMO_COMP( J )%WETNESS = IWET
         CASE ('PM25TO10' )
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
             ELMO_COMP( J )%INLET = IPM25to10
             ELMO_COMP( J )%WETNESS = IWET
         CASE ('PMAMS' )
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
             ELMO_COMP( J )%INLET = IPMAMS
             ELMO_COMP( J )%WETNESS = IWET
         CASE ('GAS')
             ! No Aerosol Parameters need to be set since
             ! this aggregate will be all gas.
         CASE DEFAULT
             XMSG = 'An aerosol descriptor (' // trim(AEROTYPE) // ') has been given ' //
     &              'for the mapping of an ELMO variable. It is not recognized.'
             CALL M3EXIT( 'INIT_ELMO_COMP', 0, 0, XMSG, XSTAT1 )
      END SELECT

      ! Determine the Variable Type
      VAR_TYPE = ELMO_TABLE( J )%VAR_TYPE
      
      ! If this variable is an ISAM or DDM variable, set the Source Mask 
      ! for this Component
      IF ( VAR_TYPE .EQ. ETYPE_ISAM_ELMO .OR. VAR_TYPE .EQ. ETYPE_ISAM 
     & .OR.VAR_TYPE .EQ. ETYPE_ISAM_WDEP .OR. VAR_TYPE .EQ. ETYPE_ISAM_DDEP
     & .OR.VAR_TYPE .EQ. ETYPE_DDM_ELMO  .OR. VAR_TYPE .EQ. ETYPE_DDM
     & .OR.VAR_TYPE .EQ. ETYPE_DDM_WDEP  .OR. VAR_TYPE .EQ. ETYPE_DDM_DDEP 
     & .OR.VAR_TYPE .EQ. ETYPE_ISAM_FAM  .OR. VAR_TYPE .EQ. ETYPE_DDM_FAM
     &        ) THEN
            
         ! The only options are one source or all sources summed
         ! together (SOURCE from the ELMO Table variable = 0). 
         ! Future versions may expand the feature to allow subsets 
         ! of sources to be summed together.
         ISRC = ELMO_TABLE( J )%SOURCE
         IF ( ISRC .EQ. 0 ) THEN
            ELMO_COMP( J )%L_SOURCE(:) = .TRUE.
         ELSE
            ELMO_COMP( J )%L_SOURCE( ISRC ) = .TRUE.
         END IF
      END IF                                                                                                
                                                                                                
      END SUBROUTINE INIT_ELMO_COMP                                                             
                                                                                                
!------------------------------------------------------------------------------------------            
      SUBROUTINE MAP_ELMO_UNITS( UNIT_TMP, IUNIT, VARNAME )
!     This subroutine maps units in character strings to a standard
!     vector of ELMO units that are available for output. This integer
!     vector distinguishes mass-based units from mole-based units so
!     that density conversions may be applied online.
!-------------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER( * ), INTENT( IN ) :: UNIT_TMP 
      INTEGER, INTENT( OUT )       :: IUNIT
      CHARACTER( 16 ), INTENT( IN ):: VARNAME
      CHARACTER(  4 ) UNITTYPE
      CHARACTER( 200 ) XMSG
 

      ! Assign Unit to Aggregate Variable
      UNITTYPE = UNIT_TMP
      CALL UPCASE( UNITTYPE )
      SELECT CASE ( UNITTYPE )
        CASE ( 'UG M-3' )
           IUNIT = 1
        CASE ( 'PPMV' )
           IUNIT = 2
        CASE ( 'UGC M-3' )
           IUNIT = 1
        CASE ( 'PPMC' )
           IUNIT = 2
        CASE ( 'UGN M-3' )
           IUNIT = 1
        CASE ( 'PPMN' )
           IUNIT = 2
        CASE ( 'M-3' )
           IUNIT = 0
        CASE ( 'M2 M-3' )
           IUNIT = 0
        CASE ( 'HA-1' )
           IUNIT = 0
        CASE ( 'M2 HA-1' )
           IUNIT = 0
        CASE ( 'KG HA-1' )
           IUNIT = 1
        CASE DEFAULT
           IUNIT = 0
           XMSG = 'A unit (' // trim(UNIT_TMP) // ') has been given ' //
     &          'for the mapping of an ELMO aggregate variable, that '//
     &          'is not consistent with a concentration or deposition '//
     &          'flux. Confirm that this is correct for variable '//
     &          trim(VARNAME) //'.'
           CALL M3WARN( 'MAP_ELMO_UNITS', 0, 0, XMSG )
      END SELECT
 
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
