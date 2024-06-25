
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

      MODULE ELMO_COMPONENTS

      USE ELMO_DATA
      USE RUNTIME_VARS, ONLY : LOGDEV

      IMPLICIT NONE

      CONTAINS

!-------------------------------------------------------------------------
      SUBROUTINE ELMO_MAP_COMPONENTS()
!     This subroutine maps the ELMO variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------
      
      USE CGRID_SPCS, ONLY : CGRID_NAME
      USE UTIL_FAMILY_MODULE, ONLY : CHEMFAMILYNUM, CHEMFAMILYMEMBERS
      USE AERO_DATA, ONLY : ascat_na_fac, asoil_na_fac, acors_na_fac,
     &                      ascat_mg_fac, asoil_mg_fac, acors_mg_fac,
     &                      ascat_k_fac,  asoil_k_fac,  acors_k_fac,
     &                      ascat_ca_fac, asoil_ca_fac, acors_ca_fac,
     &                                    asoil_fe_fac, acors_fe_fac,
     &                                    asoil_mn_fac, acors_mn_fac

      IMPLICIT NONE

      INTEGER IDG, N_VARS, IELMO, S, F, IV, J, ID, K

      WRITE( LOGDEV, '(/5x,A,A)')'The following variables are marked invalid by ', 
     &                      'ELMO because the listed component could not be found:'

      ! Loop through all available ELMO_TABLE Variables
      DO J = 1,N_ELMO_TABLE
          ID = ELMO_TABLE( J )%ID

          IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_ELMO .OR.
     &         ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_ISAM_ELMO .OR.
     &         ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_DDM_ELMO ) THEN
           SELECT CASE ( ID )
             
             !!!! Gas-Phase Variables !!!!
             ! Map NOx
             CASE ( ID_NOX ) 
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'GAS' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'NO', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'NO2', 1.0 )

             !!!! Coarse-Mode Variables !!!!  

             ! Map PMC_SO4 - Coarse-Mode Sulfate
             CASE ( ID_PMC_SO4 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'COARSE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASO4', 1.0 )
 
             ! Map PMC_NO3 - Coarse-Mode Nitrate
             CASE ( ID_PMC_NO3 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'COARSE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANO3', 1.0 )
 
             ! Map PMC_NH4 - Coarse-Mode Ammonium
             CASE ( ID_PMC_NH4 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'COARSE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANH4', 1.0 )
 
             ! Map PMC_CL - Coarse-Mode Chlorine
             CASE ( ID_PMC_CL ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'COARSE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACL', 1.0 )
 
             ! Map PMC_NA - Coarse-Mode Sodium
             CASE ( ID_PMC_NA ) 
               N_VARS = 3
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'COARSE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASEACAT', real(ascat_na_fac) )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASOIL',   real(asoil_na_fac) )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ACORS',   real(acors_na_fac) )
 
             ! Map PMC_MG - Coarse-Mode Magnesium
             CASE ( ID_PMC_MG ) 
               N_VARS = 3
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'COARSE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASEACAT', real(ascat_mg_fac) )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASOIL',   real(asoil_mg_fac) )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ACORS',   real(acors_mg_fac) )
 
             ! Map PMC_K - Coarse-Mode Potassium
             CASE ( ID_PMC_K ) 
               N_VARS = 3
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'COARSE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASEACAT', real(ascat_k_fac) )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASOIL',   real(asoil_k_fac) )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ACORS',   real(acors_k_fac) )

             ! Map PMC_CA - Coarse-Mode Calcium
             CASE ( ID_PMC_CA ) 
               N_VARS = 3
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'COARSE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASEACAT', real(ascat_ca_fac) )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASOIL',   real(asoil_ca_fac) )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ACORS',   real(acors_ca_fac) )

             
             !!!! Fine-Mode Variables !!!!  

             ! Map PMF_SO4 - Fine-Mode Sulfate
             CASE ( ID_PMF_SO4 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASO4', 1.0 )
 
             ! Map PMF_NO3 - Fine-Mode Nitrate
             CASE ( ID_PMF_NO3 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANO3', 1.0 )
 
             ! Map PMF_NH4 - Fine-Mode Ammonium
             CASE ( ID_PMF_NH4 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS , 'FINE')
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANH4', 1.0 )
 
             ! Map PMF_CL - Fine-Mode Chloride
             CASE ( ID_PMF_CL ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACL', 1.0 )
 
             ! Map PMF_NA - Fine-Mode Sodium
             CASE ( ID_PMF_NA ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANA', 1.0 )
 
             ! Map PMF_EC - Fine-Mode Elemental Carbon                                   
             CASE ( ID_PMF_EC )                                                         
               N_VARS = 1                                                               
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )                           
               CALL ELMO_SET_COMPONENTS( J, 1, 'AEC', 1.0 )                  
                                                                                        
             ! Map PMF_FE - Fine-Mode Iron
             CASE ( ID_PMF_FE ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AFE', 1.0 )
 
             ! Map PMF_AL - Fine-Mode Aluminum
             CASE ( ID_PMF_AL ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AAL', 1.0 )
 
             ! Map PMF_SI - Fine-Mode Silicon
             CASE ( ID_PMF_SI ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASI', 1.0 )
 
             ! Map PMF_TI - Fine-Mode Titanium
             CASE ( ID_PMF_TI ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ATI', 1.0 )
 
             ! Map PMF_CA - Fine-Mode Calcium
             CASE ( ID_PMF_CA ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACA', 1.0 )
 
             ! Map PMF_MG - Fine-Mode Magnesium
             CASE ( ID_PMF_MG ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AMG', 1.0 )
 
             ! Map PMF_K - Fine-Mode Potassium
             CASE ( ID_PMF_K ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AK', 1.0 )
 
             ! Map PMF_MN - Fine-Mode Manganese
             CASE ( ID_PMF_MN ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AMN', 1.0 )
 
             ! Map PMF_H2O - Fine-Mode Particle Water           
             CASE ( ID_PMF_H2O )                               
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AH2O', 1.0/19.0 )
 
             ! Map SOIL_IMPV - IMPROVE reconstruction of Soil PM
             CASE ( ID_SOIL_IMPV )
               N_VARS = 5
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ACCUMULATION' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AAL', 2.20 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASI', 2.49 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ACA', 1.63 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'AFE', 2.42 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'ATI', 1.94 )
 
             ! Map PMF_UNSP1 - Retrieve Fine-Mode Unspeciated PM based 
             !   on reconstructed SOIL PM from IMPROVE method and including 
             !   NCOM in the unspeciated portion
             CASE ( ID_PMF_UNSP1 )
               N_VARS = 9
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'PMF_MASS', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASO4', -1.0 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ANO3', -1.0 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'ANH4', -1.0 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'ACL', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 6, 'ANA', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 7, 'AEC', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 8, 'PMF_OC', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 9, 'PMF_SOILIMPV', -1.0 )          
 
             ! Map PMF_UNSP2 - Retrieve Fine-Mode Unspeciated PM based 
             !   on reconstructed SOIL PM from IMPROVE method and EXCLUDING 
             !   NCOM in the unspeciated portion
             CASE ( ID_PMF_UNSP2 )
               N_VARS = 9
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'PMF_MASS', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASO4', -1.0 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ANO3', -1.0 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'ANH4', -1.0 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'ACL', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 6, 'ANA', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 7, 'AEC', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 8, 'PMF_OA', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 9, 'PMF_SOILIMPV', -1.0 )          

             ! Map PMF_HP - Fine-Mode Hydronium Ion
             CASE ( ID_PMF_HP ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AH3OP', 1.0/19.0 )
 
             ! Map PMF_NCOM - Fine-Mode Non-Carbon Organic Mass in POA
             CASE ( ID_PMF_NCOM ) 
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'PMF_OA', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'PMF_OC',-1.0 )
 
             ! Map PMF_CLDGLY - Fine-Mode SOA from glyoxal and methylglyoxal
             CASE ( ID_PMF_CLDGLY ) 
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AORGC', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'AGLY', 1.0 )
 
             ! Map PMF_ISOP - Fine-Mode Isoprene SOA excluding IEPOX SOA
             CASE ( ID_PMF_ISOP ) 
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AISO1', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'AISO2', 1.0 )
 
             ! Map PMF_IEPOX - Fine-Mode IEPOX SOA
             CASE ( ID_PMF_IEPOX ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AISO3', 1.0 )
 
             ! Map PMF_MTNSOA - Fine-Mode Monoterpene Nitrate SOA
             CASE ( ID_PMF_MTNSOA ) 
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AMTNO3', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'AMTHYD', 1.0 )
 
             ! Map PMF_MTSOA - Fine-Mode Monoterpene SOA excluding
             ! Nitrates
             CASE ( ID_PMF_MTSOA ) 
               N_VARS = 6
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AMT1', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'AMT2', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'AMT3', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'AMT4', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'AMT5', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 6, 'AMT6', 1.0 )
 
             ! Map PMF_BENAPY - Fine-Mode Benzo-a-Pyrene
             CASE ( ID_PMF_BENAPY ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'FINE' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ABENAPY', 1.0 )
 
             ! Map GAS_BENAPY - Gas-Phase Benzo-a-Pyrene
             CASE ( ID_GAS_BENAPY ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'BENAPY',1.0 ) 
             
             ! Map GAS_NH3 
             CASE ( ID_GAS_NH3 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'NH3',1.0 ) 
 
             ! Map GAS_HNO3
             CASE ( ID_GAS_HNO3 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'HNO3',1.0 ) 
 
             
             !!!! AMS Variables !!!!  

             ! Map AMS_SO4 - AMS-Measure Sulfate
             CASE ( ID_AMS_SO4 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PMAMS' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASO4', 1.0 )
             
             ! Map AMS_NO3 - AMS-Measured Nitrate
             CASE ( ID_AMS_NO3 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PMAMS' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANO3', 1.0 )
 
             ! Map AMS_NH4 - AMS-Measured Ammonium
             CASE ( ID_AMS_NH4 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PMAMS' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANH4', 1.0 )
 
             ! Map AMS_CL - AMS-Measured Chloride
             CASE ( ID_AMS_CL ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PMAMS' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACL', 1.0 )
 
             
             !!!! PM1.0 Variables !!!!  

             ! Map PM1_SO4 - PM1 Sulfate
             CASE ( ID_PM1_SO4 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASO4', 1.0 )
 
             ! Map PM1_NO3 - PM1 Nitrate
             CASE ( ID_PM1_NO3 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANO3', 1.0 )
 
             ! Map PM1_NH4 - PM1 Ammonium
             CASE ( ID_PM1_NH4 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANH4', 1.0 )
 
             ! Map PM1_CL - PM1 Chloride
             CASE ( ID_PM1_CL ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACL', 1.0 )
 
             ! Map PM1_NA - PM1 Sodium
             CASE ( ID_PM1_NA ) 
               N_VARS = 4
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANA', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASEACAT', real(ascat_na_fac) )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ASOIL',   real(asoil_na_fac) )
               CALL ELMO_SET_COMPONENTS( J, 4, 'ACORS',   real(acors_na_fac) )
 
             ! Map PM1_EC - PM1 Elemental Carbon
             CASE ( ID_PM1_EC ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AEC', 1.0 )
 
             ! Map PM1_MG - PM1 Magnesium
             CASE ( ID_PM1_MG ) 
               N_VARS = 4
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AMG', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASEACAT', real(ascat_mg_fac) )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ASOIL',   real(asoil_mg_fac) )
               CALL ELMO_SET_COMPONENTS( J, 4, 'ACORS',   real(acors_mg_fac) )
 
             ! Map PM1_K - PM1 Potassium
             CASE ( ID_PM1_K ) 
               N_VARS = 4
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AK', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASEACAT', real(ascat_k_fac) )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ASOIL',   real(asoil_k_fac) )
               CALL ELMO_SET_COMPONENTS( J, 4, 'ACORS',   real(acors_k_fac) )
 
             ! Map PM1_CA - PM1 Calcium
             CASE ( ID_PM1_CA ) 
               N_VARS = 4
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACA', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASEACAT', real(ascat_ca_fac ))
               CALL ELMO_SET_COMPONENTS( J, 3, 'ASOIL',   real(asoil_ca_fac ))
               CALL ELMO_SET_COMPONENTS( J, 4, 'ACORS',   real(acors_ca_fac ))
 
             ! Map PM1_Other - PM1 Other 
             CASE ( ID_PM1_OT ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AOTHR', 1.0 )
 
             ! Map PM1_FE - PM1 Iron
             CASE ( ID_PM1_FE ) 
               N_VARS = 3
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AFE', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASOIL',   real(asoil_fe_fac ))
               CALL ELMO_SET_COMPONENTS( J, 3, 'ACORS',   real(acors_fe_fac ))
 
             ! Map PM1_SI - PM1 Silicon
             CASE ( ID_PM1_SI ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASI', 1.0 )
 
             ! Map PM1_TI - PM1 Titanium
             CASE ( ID_PM1_TI ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ATI', 1.0 )
 
             ! Map PM1_MN - PM1 Manganese
             CASE ( ID_PM1_MN ) 
               N_VARS = 3
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AMN', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASOIL',   real(asoil_mn_fac ))
               CALL ELMO_SET_COMPONENTS( J, 3, 'ACORS',   real(acors_mn_fac ))
 
             ! Map PM1_AL - PM1 Aluminum
             CASE ( ID_PM1_AL ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AAL', 1.0 )
  
             ! Map PM1_Soil - PM1.0 Reconstructed Soil for the fine
             ! mode + explicit soil for the coarse mode
             CASE ( ID_PM1_SOIL ) 
               N_VARS = 6
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASOIL', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'AAL', 2.20 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ASI', 2.49 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'ACA', 1.63 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'AFE', 2.42 )
               CALL ELMO_SET_COMPONENTS( J, 6, 'ATI', 1.94 )
   
             ! Map PM1.0 Unspeciated PM based on reconstructed
             ! SOIL PM from IMPROVE method and including NCOM in the
             ! unspeciated portion
             CASE ( ID_PM1_UNSP1 )
               N_VARS = 9
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'PM1', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASO4', -1.0 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ANO3', -1.0 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'ANH4', -1.0 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'ACL', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 6, 'ANA', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 7, 'AEC', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 8, 'PM1_OC', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 9, 'PM1_SOIL', -1.0 )          
  
             ! Map PM1_UN - PM1.0 Unspeciated Coarse Mass
             CASE ( ID_PM1_UN ) 
               N_VARS = 3
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASEACAT', 1.0 - real( ascat_na_fac + ascat_mg_fac +  
     &                                                          ascat_k_fac  + ascat_ca_fac  )) 
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASOIL',   1.0 - real( asoil_na_fac + asoil_mg_fac +
     &                                                          asoil_k_fac  + asoil_ca_fac +
     &                                                          asoil_mn_fac + asoil_fe_fac  ))
               CALL ELMO_SET_COMPONENTS( J, 3, 'ACORS',   1.0 - real( acors_na_fac + acors_mg_fac +
     &                                                          acors_k_fac  + acors_ca_fac +
     &                                                          acors_mn_fac + acors_fe_fac  ))
              
             ! Map PM1_HP - PM1.0 Hydronium Ion
             CASE ( ID_PM1_HP ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM1' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AH3OP', 1.0/19.0 ) 
             
             !!!! PM2.5 Variables !!!!  

             ! Map PM25_SO4 - PM2.5 Sulfate
             CASE ( ID_PM25_SO4 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASO4', 1.0 )
 
             ! Map PM25_NO3 - PM2.5 Nitrate
             CASE ( ID_PM25_NO3 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANO3', 1.0 )
 
             ! Map PM25_NH4 - PM2.5 Ammonium
             CASE ( ID_PM25_NH4 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANH4', 1.0 )
 
             ! Map PM25_CL - PM2.5 Chloride
             CASE ( ID_PM25_CL ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACL', 1.0 )
 
             ! Map PM25_NA - PM2.5 Sodium
             CASE ( ID_PM25_NA ) 
               N_VARS = 4
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANA', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASEACAT', real(ascat_na_fac ))
               CALL ELMO_SET_COMPONENTS( J, 3, 'ASOIL',   real(asoil_na_fac ))
               CALL ELMO_SET_COMPONENTS( J, 4, 'ACORS',   real(acors_na_fac ))
 
             ! Map PM25_EC - PM2.5 Elemental Carbon
             CASE ( ID_PM25_EC ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AEC', 1.0 )
 
             ! Map PM25_MG - PM2.5 Magnesium
             CASE ( ID_PM25_MG ) 
               N_VARS = 4
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AMG', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASEACAT', real(ascat_mg_fac ))
               CALL ELMO_SET_COMPONENTS( J, 3, 'ASOIL',   real(asoil_mg_fac ))
               CALL ELMO_SET_COMPONENTS( J, 4, 'ACORS',   real(acors_mg_fac ))
 
             ! Map PM25_K - PM2.5 Potassium
             CASE ( ID_PM25_K ) 
               N_VARS = 4
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AK', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASEACAT', real(ascat_k_fac ))
               CALL ELMO_SET_COMPONENTS( J, 3, 'ASOIL',   real(asoil_k_fac ))
               CALL ELMO_SET_COMPONENTS( J, 4, 'ACORS',   real(acors_k_fac ))
 
             ! Map PM25_CA - PM2.5 Calcium
             CASE ( ID_PM25_CA ) 
               N_VARS = 4
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACA', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASEACAT', real(ascat_ca_fac ))
               CALL ELMO_SET_COMPONENTS( J, 3, 'ASOIL', real(asoil_ca_fac ))
               CALL ELMO_SET_COMPONENTS( J, 4, 'ACORS', real(acors_ca_fac ))
 
             ! Map PM25_Other - PM25 Other 
             CASE ( ID_PM25_OT ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AOTHR', 1.0 )
 
             ! Map PM25_FE - PM2.5 Iron
             CASE ( ID_PM25_FE ) 
               N_VARS = 3
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AFE', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASOIL', real(asoil_fe_fac ))
               CALL ELMO_SET_COMPONENTS( J, 3, 'ACORS', real(acors_fe_fac ))
 
             ! Map PM25_SI - PM2.5 Silicon
             CASE ( ID_PM25_SI ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASI', 1.0 )
 
             ! Map PM25_TI - PM2.5 Titanium
             CASE ( ID_PM25_TI ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ATI', 1.0 )
 
             ! Map PM25_MN - PM2.5 Manganese
             CASE ( ID_PM25_MN ) 
               N_VARS = 3
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AMN', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASOIL', real(asoil_mn_fac ))
               CALL ELMO_SET_COMPONENTS( J, 3, 'ACORS', real(acors_mn_fac ))
 
             ! Map PM25_AL - PM2.5 Aluminum
             CASE ( ID_PM25_AL ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AAL', 1.0 )
 
             ! Map PM25_Soil - PM2.5 Reconstructed Soil for the fine
             ! mode + explicit soil for the coarse mode
             CASE ( ID_PM25_SOIL ) 
               N_VARS = 6
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASOIL', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'AAL', 2.20 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ASI', 2.49 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'ACA', 1.63 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'AFE', 2.42 )
               CALL ELMO_SET_COMPONENTS( J, 6, 'ATI', 1.94 )
   
             ! Map PM2.5 Unspeciated PM based on reconstructed
             ! SOIL PM from IMPROVE method and including NCOM in the
             ! unspeciated portion
             CASE ( ID_PM25_UNSP1 )
               N_VARS = 9
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'PM25', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASO4', -1.0 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ANO3', -1.0 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'ANH4', -1.0 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'ACL', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 6, 'ANA', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 7, 'AEC', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 8, 'PM25_OC', -1.0 )          
               CALL ELMO_SET_COMPONENTS( J, 9, 'PM25_SOIL', -1.0 )          
 
             ! Map PM25_UN - PM2.5 Unspeciated Coarse Mass
             CASE ( ID_PM25_UN ) 
               N_VARS = 3
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASEACAT', 1.0 - real( ascat_na_fac + ascat_mg_fac +  
     &                                                          ascat_k_fac  + ascat_ca_fac  )) 
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASOIL',   1.0 - real( asoil_na_fac + asoil_mg_fac +
     &                                                          asoil_k_fac  + asoil_ca_fac +
     &                                                          asoil_mn_fac + asoil_fe_fac  ))
               CALL ELMO_SET_COMPONENTS( J, 3, 'ACORS',   1.0 - real( acors_na_fac + acors_mg_fac +
     &                                                          acors_k_fac  + acors_ca_fac +
     &                                                          acors_mn_fac + acors_fe_fac  ))
 
             ! Map PM25_HP - PM2.5 Hydronium Ion
             CASE ( ID_PM25_HP ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AH3OP', 1.0/19.0 )
 
             
             !!!! PM2.5 - 10.0 Variables !!!!  

             ! Map PM25to10_SO4 - PM2.5-10.0 Sulfate
             CASE ( ID_PM25to10_SO4 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25TO10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ASO4', 1.0 )
 
             ! Map PM25to10_NO3 - PM2.5-10.0 Nitrate
             CASE ( ID_PM25to10_NO3 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25TO10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANO3', 1.0 )
 
             ! Map PM25to10_NH4 - PM2.5-10.0 Ammonium
             CASE ( ID_PM25to10_NH4 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25TO10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANH4', 1.0 )
 
             ! Map PM25to10_CL - PM2.5-10.0 Chloride
             CASE ( ID_PM25to10_CL ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25TO10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACL', 1.0 )
 
             ! Map PM25to10_NA - PM2.5-10.0 Sodium
             CASE ( ID_PM25to10_NA ) 
               N_VARS = 4
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25TO10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANA', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ASEACAT', real(ascat_na_fac ))
               CALL ELMO_SET_COMPONENTS( J, 3, 'ASOIL',   real(asoil_na_fac ))
               CALL ELMO_SET_COMPONENTS( J, 4, 'ACORS',   real(acors_na_fac ))
 
             !!!! Toxics !!!!  
 
             ! Map PM25_HDSL - HAP inventory PM2.5 diesel mass
             CASE ( ID_PM25_HDSL ) 
               N_VARS = 6
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ADE_OTHR', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ADE_EC', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ADE_OC', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'ADE_SO4', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'ADE_NO3', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 6, 'ADE_CORS', 1.0 )
 
             ! Map PM25_HBE - HAP inventory Beryllium
             CASE ( ID_PM25_HBE ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ABE', 1.0 )
 
             ! Map PM25_HCD - HAP inventory Cadmium
             CASE ( ID_PM25_HCD ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACD', 1.0 )
 
             ! Map PM25_HCR3 - HAP inventory Chromium III
             CASE ( ID_PM25_HCR3 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACR_III', 1.0 )
 
             ! Map PM25_HCR6 - HAP inventory Chromium VI
             CASE ( ID_PM25_HCR6 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACR_VI', 1.0 )
 
             ! Map PM25_HCR - HAP inventory Total Chromium
             CASE ( ID_PM25_HCR ) 
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'PM25_HCR3', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'PM25_HCR6', 1.0 )
 
             ! Map PM25_HPB - HAP inventory Lead
             CASE ( ID_PM25_HPB ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'APB', 1.0 )
 
             ! Map PM25_HMN - HAP inventory Manganese
             CASE ( ID_PM25_HMN ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AMN_HAPS', 1.0 )
 
             ! Map PM25_HNI - HAP inventory Nickel
             CASE ( ID_PM25_HNI ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANI', 1.0 )
 
             ! Map PM25_HAS - HAP inventory Arsenic
             CASE ( ID_PM25_HAS ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AAS', 1.0 )
 
             ! Map PM25_HG - Mercury
             CASE ( ID_PM25_HG ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'APHG', 1.0 )
 
             ! Map PM25_BENAPY - Benzo-a-Pyrene
             CASE ( ID_PM25_BENAPY ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM25' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ABENAPY', 1.0 )
 
             ! Map PM10_HDSL - HAP inventory PM2.5 diesel mass
             CASE ( ID_PM10_HDSL ) 
               N_VARS = 6
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ADE_OTHR', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'ADE_EC', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'ADE_OC', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'ADE_SO4', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'ADE_NO3', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 6, 'ADE_CORS', 1.0 )
 
             ! Map PM10_HBE - HAP inventory Beryllium
             CASE ( ID_PM10_HBE ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ABE', 1.0 )
 
             ! Map PM10_HCD - HAP inventory Cadmium
             CASE ( ID_PM10_HCD ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACD', 1.0 )
 
             ! Map PM10_HCR3 - HAP inventory Chromium III
             CASE ( ID_PM10_HCR3 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACR_III', 1.0 )
 
             ! Map PM10_HCR6 - HAP inventory Chromium VI
             CASE ( ID_PM10_HCR6 ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ACR_VI', 1.0 )
 
             ! Map PM10_HCR - HAP inventory Total Chromium
             CASE ( ID_PM10_HCR ) 
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'PM10_HCR3', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'PM10_HCR6', 1.0 )
 
             ! Map PM10_HPB - HAP inventory Lead
             CASE ( ID_PM10_HPB ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'APB', 1.0 )
 
             ! Map PM10_HMN - HAP inventory Manganese
             CASE ( ID_PM10_HMN ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AMN_HAPS', 1.0 )
 
             ! Map PM10_HNI - HAP inventory Nickel
             CASE ( ID_PM10_HNI ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANI', 1.0 )
 
             ! Map PM10_HAS - HAP inventory Arsenic
             CASE ( ID_PM10_HAS ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'AAS', 1.0 )
 
             ! Map PM10_HG - Mercury
             CASE ( ID_PM10_HG ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'APHG', 1.0 )
 
             ! Map PM10_BENAPY - Benzo-a-Pyrene
             CASE ( ID_PM10_BENAPY ) 
               N_VARS = 1
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'PM10' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ABENAPY', 1.0 )
 
             !!!! Other Variables !!!!  

             ! Map TNO3 - Total Inorganic Nitrate in mass concentration
             ! units. The units are specified in the ELMO_LIST Table in
             ! ELMO_DATA.F.
             CASE ( ID_TNO3 ) 
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANO3', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'HNO3', 1.0 ) 
 
             ! Map TNO3_MOL - Total Inorganic Nitrate in Mixing Ratio
             ! Units. The units are specified in the ELMO_LIST Table in
             ! ELMO_DATA.F.
             CASE ( ID_TNO3_MOL ) 
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANO3', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'HNO3', 1.0 ) 
 
             ! Map TNO3TOT - Total Inorganic+Organic Nitrate
             CASE ( ID_TNO3TOT ) 
               N_VARS = 4
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'ANO3', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'HNO3', 1.0 ) 
               CALL ELMO_SET_COMPONENTS( J, 3, 'AISOPNN', 1.0 ) ! Convert to just NO3
               CALL ELMO_SET_COMPONENTS( J, 4, 'AMTNO3', 1.0 )  ! Convert to just NO3
 
             ! Map Dry Deposition of Oxidized Nitrogen from NOx
             CASE ( ID_DD_OXN_NOX )
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'DD_NO2' , 0.30435 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'DD_NO'  , 0.46667 )
             
             ! Map Wet Deposition of Oxidized Nitrogen from NOx
             CASE ( ID_WD_OXN_NOX )
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'WD_NO2' , 0.30435 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'WD_NO'  , 0.46667 )
             
             ! Map Dry Deposition of Oxidized Nitrogen from NO3
             CASE ( ID_DD_OXN_TNO3 )
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'DD_HNO3', 0.22222 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'DD_ANO3', 0.22581 )
             
             ! Map Wet Deposition of Oxidized Nitrogen from NO3
             CASE ( ID_WD_OXN_TNO3 )
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'WD_HNO3', 0.22222 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'WD_ANO3', 0.22581 )
             
             ! Map Dry Deposition of Oxidized Nitrogen from PAN
             CASE ( ID_DD_OXN_PANT )
               N_VARS = 3
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'DD_PAN' , 0.11570 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'DD_PPN' , 0.11570 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'DD_MPAN', 0.11570 )
             
             ! Map Wet Deposition of Oxidized Nitrogen from PAN
             CASE ( ID_WD_OXN_PANT )
               N_VARS = 3
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'WD_PAN' , 0.11570 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'WD_PPN' , 0.11570 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'WD_MPAN', 0.11570 )
             
             ! Map Dry Deposition of Oxidized Nitrogen from ORGN
             CASE ( ID_DD_OXN_ORGN )
               N_VARS = 7
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'DD_ONIT', 0.10770 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'DD_ISON', 0.10770 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'DD_TRPN', 0.10770 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'DD_NALD', 0.10770 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'DD_IPN' , 0.10770 )
               CALL ELMO_SET_COMPONENTS( J, 6, 'DD_IPC' , 0.10770 )
               CALL ELMO_SET_COMPONENTS( J, 7, 'DD_HONIT',0.10770 )
             
             ! Map Wet Deposition of Oxidized Nitrogen from ORGN
             CASE ( ID_WD_OXN_ORGN )
               N_VARS = 7
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'WD_ONIT', 0.10770 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'WD_ISON', 0.10770 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'WD_TRPN', 0.10770 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'WD_NALD', 0.10770 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'WD_IPN' , 0.10770 )
               CALL ELMO_SET_COMPONENTS( J, 6, 'WD_IPC' , 0.10770 )
               CALL ELMO_SET_COMPONENTS( J, 7, 'WD_HONIT',0.10770 )
             
             ! Map Dry Deposition of Oxidized Nitrogen from Other
             CASE ( ID_DD_OXN_OTHR )
               N_VARS = 3
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'DD_N2O5', 0.25926 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'DD_HONO', 0.29787 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'DD_HNO4', 0.00    )
             
             ! Map Wet Deposition of Oxidized Nitrogen from Other
             CASE ( ID_WD_OXN_OTHR )
               N_VARS = 6
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'WD_N2O5', 0.25926 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'WD_HONO', 0.29787 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'WD_HNO4', 0.17772 )

             ! Map Dry Deposition of Oxidized Nitrogen
             CASE ( ID_DD_OXN_TOT )
               N_VARS = 5
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'DD_OXN_NOX' , 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'DD_OXN_TNO3', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'DD_OXN_PANT', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'DD_OXN_ORGN', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'DD_OXN_OTHR', 1.0 )

             ! Map Wet Deposition of Oxidized Nitrogen
             CASE ( ID_WD_OXN_TOT )
               N_VARS = 5
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'WD_OXN_NOX' , 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'WD_OXN_TNO3', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 3, 'WD_OXN_PANT', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 4, 'WD_OXN_ORGN', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 5, 'WD_OXN_OTHR', 1.0 )

             ! Map Total Deposition of Oxidized Nitrogen
             CASE ( ID_TD_OXN_TOT )
               N_VARS = 2
               CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
               CALL ELMO_SET_COMPONENTS( J, 1, 'DD_OXN_TOT', 1.0 )
               CALL ELMO_SET_COMPONENTS( J, 2, 'WD_OXN_TOT', 1.0 )


          END SELECT

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_FAM .OR.
     &             ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_ISAM_FAM .OR.
     &             ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_DDM_FAM ) THEN
           ! Populate the members of this Chemical Family in the
           ! ELMO_COMP array.
           N_VARS = CHEMFAMILYNUM( ID )
           CALL ELMO_INIT_COMPONENTS( J, N_VARS, 'ALL' )
           DO K = 1,N_VARS
             CALL ELMO_SET_COMPONENTS( J, K, CHEMFAMILYMEMBERS(ID,K), 1.0 )
           END DO

         END IF


      END DO

      WRITE( LOGDEV, *)

      END SUBROUTINE ELMO_MAP_COMPONENTS

!-------------------------------------------------------------------------
      SUBROUTINE ELMO_INIT_COMPONENTS( J, N_VARS, AEROTYPE_TMP )
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
#ifdef isam
      USE SA_DEFN, ONLY : NTAG_SA
#elif sens
      USE DDM3D_DEFN, ONLY : NPMAX
#endif

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: J 
      INTEGER, INTENT( IN ) :: N_VARS
      CHARACTER( * ), INTENT( IN ) :: AEROTYPE_TMP
      CHARACTER( 20 ) AEROTYPE
      INTEGER VAR_TYPE, ISRC

      CHARACTER( 200 ) XMSG
      CHARACTER(16), SAVE :: PNAME = 'INIT_ELMO_COEF'
      INTEGER ALLOCSTAT

      ! Initialize All Components of ELMO_COEFFS
      ELMO_TABLE( J )%L_COMP = .TRUE.
      ELMO_COMP( J )%N_VARS = N_VARS
      ALLOCATE( ELMO_COMP( J )%L_MODE  ( N_MODE ),
#ifdef isam    
     &          ELMO_COMP( J )%L_SOURCE( NTAG_SA+1 ),
#elif sens
     &          ELMO_COMP( J )%L_SOURCE( NPMAX ),
#else
     &          ELMO_COMP( J )%L_SOURCE( 1 ),
#endif
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
             CALL M3EXIT( 'ELMO_INIT_COMPONENTS', 0, 0, XMSG, XSTAT1 )
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
      END SUBROUTINE ELMO_INIT_COMPONENTS 

!-------------------------------------------------------------------------
      SUBROUTINE ELMO_SET_COMPONENTS( J, IVAR, CSPEC_IN, COEFF )
!     This subroutine maps the variables in CSPEC to the ELMO Table and
!     stores IDs, variable properties, and coefficients for calculation
!     of ELMO variables during runtime.
!-------------------------------------------------------------------------

      USE PRECURSOR_DATA, ONLY : N_PRECURSOR, PRECURSOR
      USE CGRID_SPCS, ONLY : N_CGRID_SPC, CGRID_NAME
      USE AERO_DATA, ONLY : N_AEROSPC, AEROSPC
      IMPLICIT NONE

      INTEGER, INTENT( IN ) ::  J, IVAR
      CHARACTER( * ), INTENT( IN ) :: CSPEC_IN
      CHARACTER( 32 ) CSPEC
      REAL, INTENT( IN ) :: COEFF
      INTEGER  ISPEC, JSPEC, IAERO, ICGRID, IELMO
      INTEGER  I1, I2, VAR_TYPE, ISRC, K
      CHARACTER( 1000 ) :: XMSG

      ! Details in the processing of the components of variable J rely
      ! on knowing what type of variable it is
      VAR_TYPE = ELMO_TABLE( J )%VAR_TYPE

      ! Find this component in the ELMO_TABLE
      CSPEC = CSPEC_IN
      I1 = INDEX1( CSPEC, N_ELMO_TABLE, ELMO_TABLE(:)%OUT_NAME )
      I2 = INDEX1( CSPEC, N_ELMO_TABLE, ELMO_TABLE(:)%NAME )

      ! For ISAM ELMO Variables, we need to make sure the components
      ! match the source number, or apply to all sources (i.e. ISAM
      ! Bulk)
      IF ( VAR_TYPE .EQ. ETYPE_ISAM_ELMO ) THEN
#ifdef isam      
         CSPEC = CSPEC // '_ISAM'
         ISRC = ELMO_TABLE( J )%SOURCE
         DO K = 1,N_ELMO_TABLE
           IF ( ELMO_TABLE( K )%NAME .EQ. CSPEC .AND.
     &          ELMO_TABLE( K )%SOURCE .EQ. ISRC     ) THEN
              ! This component matches an ELMO Table Output Name
              ELMO_COMP( J )%TABID( IVAR )    = K
              ELMO_COMP( J )%ID( IVAR )       = ELMO_TABLE( K )%ID
              ELMO_COMP( J )%VAR_TYPE( IVAR ) = VAR_TYPE
              ELMO_COMP( J )%MW( IVAR )       = ELMO_TABLE( K )%MW
              CALL MAP_ELMO_UNITS( ELMO_TABLE( K )%CUNIT,
     &                       ELMO_COMP( J )%SPC_UNIT( IVAR ),
     &                       ELMO_TABLE( K )%NAME,
     &                       ELMO_TABLE( K )%VAR_TYPE )
              ELMO_COMP( J )%COEFF( IVAR )    = COEFF
           END IF
         END DO
#endif

      ELSE IF  ( VAR_TYPE .EQ. ETYPE_DDM_ELMO ) THEN
#ifdef sens
         CSPEC = CSPEC // '_DDM'
         ISRC = ELMO_TABLE( J )%SOURCE
         DO K = 1,N_ELMO_TABLE
           IF ( ELMO_TABLE( K )%NAME .EQ. CSPEC .AND.
     &          ELMO_TABLE( K )%SOURCE .EQ. ISRC     ) THEN
              ! This component matches an ELMO Table Output Name
              ELMO_COMP( J )%TABID( IVAR )    = K
              ELMO_COMP( J )%ID( IVAR )       = ELMO_TABLE( K )%ID
              ELMO_COMP( J )%VAR_TYPE( IVAR ) = VAR_TYPE
              ELMO_COMP( J )%MW( IVAR )       = ELMO_TABLE( K )%MW
              CALL MAP_ELMO_UNITS( ELMO_TABLE( K )%CUNIT,
     &                       ELMO_COMP( J )%SPC_UNIT( IVAR ),
     &                       ELMO_TABLE( K )%NAME,
     &                       ELMO_TABLE( K )%VAR_TYPE )
              ELMO_COMP( J )%COEFF( IVAR )    = COEFF
           END IF
         END DO 
#endif

      ELSE IF ( I1 .GT. 0 ) THEN
        ! This component matches an ELMO Table Output Name
        ELMO_COMP( J )%TABID( IVAR )    = I1
        ELMO_COMP( J )%ID( IVAR )       = ELMO_TABLE( I1 )%ID
        ELMO_COMP( J )%VAR_TYPE( IVAR ) = ELMO_TABLE( I1 )%VAR_TYPE
        ELMO_COMP( J )%MW( IVAR )       = ELMO_TABLE( I1 )%MW
        CALL MAP_ELMO_UNITS( ELMO_TABLE( I1 )%CUNIT,
     &                       ELMO_COMP( J )%SPC_UNIT( IVAR ),
     &                       ELMO_TABLE( I1 )%NAME,
     &                       ELMO_TABLE( I1 )%VAR_TYPE )
        ELMO_COMP( J )%COEFF( IVAR )    = COEFF
        
      ELSE IF ( I2 .GT. 0 ) THEN
        ! This component matches an ELMO Table Name  
        ELMO_COMP( J )%TABID( IVAR )    = I2
        ELMO_COMP( J )%ID( IVAR )       = ELMO_TABLE( I2 )%ID
        ELMO_COMP( J )%VAR_TYPE( IVAR ) = ELMO_TABLE( I2 )%VAR_TYPE
        ELMO_COMP( J )%MW( IVAR )       = ELMO_TABLE( I2 )%MW
        CALL MAP_ELMO_UNITS( ELMO_TABLE( I2 )%CUNIT,
     &                       ELMO_COMP( J )%SPC_UNIT( IVAR ),
     &                       ELMO_TABLE( I2 )%NAME,
     &                       ELMO_TABLE( I2 )%VAR_TYPE )
        ELMO_COMP( J )%COEFF( IVAR )    = COEFF
        
      ELSE
        ELMO_COMP( J )%L_VALID = .FALSE. 
        IF ( ( CSPEC(1:3) .EQ. 'DD_' .AND. .NOT. L_ELMO_DDEP ) .OR.
     &       ( CSPEC(1:3) .EQ. 'WD_' .AND. .NOT. L_ELMO_WDEP ) ) THEN
           ! Do not print warning
        ELSE
           WRITE( LOGDEV, '(8x,A9,1x,A16,3x,A10,1x,A)' ) 
     &            'Variable:',ELMO_TABLE(J)%NAME,
     &            'Component:',CSPEC
        END IF
      END IF
 
      END SUBROUTINE ELMO_SET_COMPONENTS
 
      END MODULE ELMO_COMPONENTS

