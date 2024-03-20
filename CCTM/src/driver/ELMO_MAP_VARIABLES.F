
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
 
!-------------------------------------------------------------------------
      SUBROUTINE ELMO_MAP_VARIABLES()
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

      INTEGER IDG, N_VARS, IELMO, S, F, IV

      ! Loop through all available ELMO_TABLE Variables
      DO J = 1,N_ELMO_TABLE
          ID = ELMO_TABLE( J )%ID

          IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_ELMO ) THEN
           SELECT CASE ( ID )
             
             !!!! Coarse-Mode Variables !!!!  

             ! Map PMC_SO4 - Coarse-Mode Sulfate
             CASE ( ID_PMC_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'COARSE' )
               CALL SET_ELMO_COMP( J, 1, 'ASO4', 1.0 )
 
             ! Map PMC_NO3 - Coarse-Mode Nitrate
             CASE ( ID_PMC_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'COARSE' )
               CALL SET_ELMO_COMP( J, 1, 'ANO3', 1.0 )
 
             ! Map PMC_NH4 - Coarse-Mode Ammonium
             CASE ( ID_PMC_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'COARSE' )
               CALL SET_ELMO_COMP( J, 1, 'ANH4', 1.0 )
 
             ! Map PMC_CL - Coarse-Mode Chlorine
             CASE ( ID_PMC_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'COARSE' )
               CALL SET_ELMO_COMP( J, 1, 'ACL', 1.0 )
 
             ! Map PMC_NA - Coarse-Mode Sodium
             CASE ( ID_PMC_NA ) 
               N_VARS = 3
               CALL INIT_ELMO_COMP( J, N_VARS, 'COARSE' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'ASEACAT', real(ascat_na_fac) )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASOIL',   real(asoil_na_fac) )
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ACORS',   real(acors_na_fac) )
 
             ! Map PMC_MG - Coarse-Mode Magnesium
             CASE ( ID_PMC_MG ) 
               N_VARS = 3
               CALL INIT_ELMO_COMP( J, N_VARS, 'COARSE' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'ASEACAT', real(ascat_mg_fac) )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASOIL',   real(asoil_mg_fac) )
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ACORS',   real(acors_mg_fac) )
 
             ! Map PMC_K - Coarse-Mode Potassium
             CASE ( ID_PMC_K ) 
               N_VARS = 3
               CALL INIT_ELMO_COMP( J, N_VARS, 'COARSE' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'ASEACAT', real(ascat_k_fac) )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASOIL',   real(asoil_k_fac) )
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ACORS',   real(acors_k_fac) )

             ! Map PMC_CA - Coarse-Mode Calcium
             CASE ( ID_PMC_CA ) 
               N_VARS = 3
               CALL INIT_ELMO_COMP( J, N_VARS, 'COARSE' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'ASEACAT', real(ascat_ca_fac) )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASOIL',   real(asoil_ca_fac) )
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ACORS',   real(acors_ca_fac) )

             
             !!!! Fine-Mode Variables !!!!  

             ! Map PMF_SO4 - Fine-Mode Sulfate
             CASE ( ID_PMF_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'ASO4', 1.0 )
 
             ! Map PMF_NO3 - Fine-Mode Nitrate
             CASE ( ID_PMF_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'ANO3', 1.0 )
 
             ! Map PMF_NH4 - Fine-Mode Ammonium
             CASE ( ID_PMF_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS , 'FINE')
               CALL SET_ELMO_COMP( J, 1, 'ANH4', 1.0 )
 
             ! Map PMF_CL - Fine-Mode Chloride
             CASE ( ID_PMF_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'ACL', 1.0 )
 
             ! Map PMF_NA - Fine-Mode Sodium
             CASE ( ID_PMF_NA ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'ANA', 1.0 )
 
             ! Map PMF_EC - Fine-Mode Elemental Carbon                                   
             CASE ( ID_PMF_EC )                                                         
               N_VARS = 1                                                               
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )                           
               CALL SET_ELMO_COMP( J, 1, 'AEC', 1.0 )                  
                                                                                        
             ! Map PMF_FE - Fine-Mode Iron
             CASE ( ID_PMF_FE ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AFE', 1.0 )
 
             ! Map PMF_AL - Fine-Mode Aluminum
             CASE ( ID_PMF_AL ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AAL', 1.0 )
 
             ! Map PMF_SI - Fine-Mode Silicon
             CASE ( ID_PMF_SI ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'ASI', 1.0 )
 
             ! Map PMF_TI - Fine-Mode Titanium
             CASE ( ID_PMF_TI ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'ATI', 1.0 )
 
             ! Map PMF_CA - Fine-Mode Calcium
             CASE ( ID_PMF_CA ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'ACA', 1.0 )
 
             ! Map PMF_MG - Fine-Mode Magnesium
             CASE ( ID_PMF_MG ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AMG', 1.0 )
 
             ! Map PMF_K - Fine-Mode Potassium
             CASE ( ID_PMF_K ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AK', 1.0 )
 
             ! Map PMF_MN - Fine-Mode Manganese
             CASE ( ID_PMF_MN ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AMN', 1.0 )
 
             ! Map PMF_H2O - Fine-Mode Particle Water           
             CASE ( ID_PMF_H2O )                               
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AH2O', 1.0/19.0 )
 
             ! Map SOIL_IMPV - IMPROVE reconstruction of Soil PM
             CASE ( ID_SOIL_IMPV )
               N_VARS = 5
               CALL INIT_ELMO_COMP( J, N_VARS, 'ACCUMULATION' )
               CALL SET_ELMO_COMP( J, 1, 'AAL', 2.20 )
               CALL SET_ELMO_COMP( J, 2, 'ASI', 2.49 )
               CALL SET_ELMO_COMP( J, 3, 'ACA', 1.63 )
               CALL SET_ELMO_COMP( J, 4, 'AFE', 2.42 )
               CALL SET_ELMO_COMP( J, 5, 'ATI', 1.94 )
 
             ! Map PMF_UNSP1 - Retrieve Fine-Mode Unspeciated PM based 
             !   on reconstructed SOIL PM from IMPROVE method and including 
             !   NCOM in the unspeciated portion
             CASE ( ID_PMF_UNSP1 )
               N_VARS = 9
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AGG',  'PMF_MASS', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'ASO4', -1.0 )
               CALL SET_ELMO_COMP( J, 3, 'ANO3', -1.0 )
               CALL SET_ELMO_COMP( J, 4, 'ANH4', -1.0 )
               CALL SET_ELMO_COMP( J, 5, 'ACL', -1.0 )          
               CALL SET_ELMO_COMP( J, 6, 'ANA', -1.0 )          
               CALL SET_ELMO_COMP( J, 7, 'AEC', -1.0 )          
               CALL SET_ELMO_COMP( J, 8, 'PMF_OC', -1.0 )          
               CALL SET_ELMO_COMP( J, 9, 'PMF_SOILIMPV', -1.0 )          
 
             ! Map PMF_UNSP2 - Retrieve Fine-Mode Unspeciated PM based 
             !   on reconstructed SOIL PM from IMPROVE method and EXCLUDING 
             !   NCOM in the unspeciated portion
             CASE ( ID_PMF_UNSP2 )
               N_VARS = 9
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AGG',  'PMF_MASS', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'ASO4', -1.0 )
               CALL SET_ELMO_COMP( J, 3, 'ANO3', -1.0 )
               CALL SET_ELMO_COMP( J, 4, 'ANH4', -1.0 )
               CALL SET_ELMO_COMP( J, 5, 'ACL', -1.0 )          
               CALL SET_ELMO_COMP( J, 6, 'ANA', -1.0 )          
               CALL SET_ELMO_COMP( J, 7, 'AEC', -1.0 )          
               CALL SET_ELMO_COMP( J, 8, 'PMF_OA', -1.0 )          
               CALL SET_ELMO_COMP( J, 9, 'PMF_SOILIMPV', -1.0 )          

             ! Map PMF_HP - Fine-Mode Hydronium Ion
             CASE ( ID_PMF_HP ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AH3OP', 1.0/19.0 )
 
             ! Map PMF_NCOM - Fine-Mode Non-Carbon Organic Mass in POA
             CASE ( ID_PMF_NCOM ) 
               N_VARS = 2
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'PMF_OA', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'PMF_OC',-1.0 )
 
             ! Map PMF_CLDGLY - Fine-Mode SOA from glyoxal and methylglyoxal
             CASE ( ID_PMF_CLDGLY ) 
               N_VARS = 2
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AORGC', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AGLY', 1.0 )
 
             ! Map PMF_ISOP - Fine-Mode Isoprene SOA excluding IEPOX SOA
             CASE ( ID_PMF_ISOP ) 
               N_VARS = 2
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AISO1', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AISO2', 1.0 )
 
             ! Map PMF_IEPOX - Fine-Mode IEPOX SOA
             CASE ( ID_PMF_IEPOX ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AISO3', 1.0 )
 
             ! Map PMF_MTNSOA - Fine-Mode Monoterpene Nitrate SOA
             CASE ( ID_PMF_MTNSOA ) 
               N_VARS = 2
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AMTNO3', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AMTHYD', 1.0 )
 
             ! Map PMF_MTSOA - Fine-Mode Monoterpene SOA excluding
             ! Nitrates
             CASE ( ID_PMF_MTSOA ) 
               N_VARS = 6
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'AMT1', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AMT2', 1.0 )
               CALL SET_ELMO_COMP( J, 3, 'AMT3', 1.0 )
               CALL SET_ELMO_COMP( J, 4, 'AMT4', 1.0 )
               CALL SET_ELMO_COMP( J, 5, 'AMT5', 1.0 )
               CALL SET_ELMO_COMP( J, 6, 'AMT6', 1.0 )
 
             ! Map PMF_BENAPY - Fine-Mode Benzo-a-Pyrene
             CASE ( ID_PMF_BENAPY ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'FINE' )
               CALL SET_ELMO_COMP( J, 1, 'ABENAPY', 1.0 )
 
             ! Map GAS_BENAPY - Gas-Phase Benzo-a-Pyrene
             CASE ( ID_GAS_BENAPY ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'ALL' )
               CALL SET_ELMO_COMP( J, 1, 'BENAPY',1.0 ) 
             
             ! Map GAS_NH3 
             CASE ( ID_GAS_NH3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'ALL' )
               CALL SET_ELMO_COMP( J, 1, 'NH3',1.0 ) 
 
             ! Map GAS_HNO3
             CASE ( ID_GAS_HNO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'ALL' )
               CALL SET_ELMO_COMP( J, 1, 'HNO3',1.0 ) 
 
             
             !!!! AMS Variables !!!!  

             ! Map AMS_SO4 - AMS-Measure Sulfate
             CASE ( ID_AMS_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PMAMS' )
               CALL SET_ELMO_COMP( J, 1, 'ASO4', 1.0 )
             
             ! Map AMS_NO3 - AMS-Measured Nitrate
             CASE ( ID_AMS_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PMAMS' )
               CALL SET_ELMO_COMP( J, 1, 'ANO3', 1.0 )
 
             ! Map AMS_NH4 - AMS-Measured Ammonium
             CASE ( ID_AMS_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PMAMS' )
               CALL SET_ELMO_COMP( J, 1, 'ANH4', 1.0 )
 
             ! Map AMS_CL - AMS-Measured Chloride
             CASE ( ID_AMS_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PMAMS' )
               CALL SET_ELMO_COMP( J, 1, 'ACL', 1.0 )
 
             
             !!!! PM1.0 Variables !!!!  

             ! Map PM1_SO4 - PM1 Sulfate
             CASE ( ID_PM1_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'ASO4', 1.0 )
 
             ! Map PM1_NO3 - PM1 Nitrate
             CASE ( ID_PM1_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'ANO3', 1.0 )
 
             ! Map PM1_NH4 - PM1 Ammonium
             CASE ( ID_PM1_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'ANH4', 1.0 )
 
             ! Map PM1_CL - PM1 Chloride
             CASE ( ID_PM1_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'ACL', 1.0 )
 
             ! Map PM1_NA - PM1 Sodium
             CASE ( ID_PM1_NA ) 
               N_VARS = 4
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'ANA', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASEACAT', real(ascat_na_fac) )
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ASOIL',   real(asoil_na_fac) )
               CALL SET_ELMO_COMP( J, 4, 'AERO', 'ACORS',   real(acors_na_fac) )
 
             ! Map PM1_EC - PM1 Elemental Carbon
             CASE ( ID_PM1_EC ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'AEC', 1.0 )
 
             ! Map PM1_MG - PM1 Magnesium
             CASE ( ID_PM1_MG ) 
               N_VARS = 4
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'AMG', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASEACAT', real(ascat_mg_fac) )
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ASOIL',   real(asoil_mg_fac) )
               CALL SET_ELMO_COMP( J, 4, 'AERO', 'ACORS',   real(acors_mg_fac) )
 
             ! Map PM1_K - PM1 Potassium
             CASE ( ID_PM1_K ) 
               N_VARS = 4
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'AK', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASEACAT', real(ascat_k_fac) )
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ASOIL',   real(asoil_k_fac) )
               CALL SET_ELMO_COMP( J, 4, 'AERO', 'ACORS',   real(acors_k_fac) )
 
             ! Map PM1_CA - PM1 Calcium
             CASE ( ID_PM1_CA ) 
               N_VARS = 4
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'ACA', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASEACAT', real(ascat_ca_fac ))
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ASOIL',   real(asoil_ca_fac ))
               CALL SET_ELMO_COMP( J, 4, 'AERO', 'ACORS',   real(acors_ca_fac ))
 
             ! Map PM1_Other - PM1 Other 
             CASE ( ID_PM1_OT ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'AOTHR', 1.0 )
 
             ! Map PM1_FE - PM1 Iron
             CASE ( ID_PM1_FE ) 
               N_VARS = 3
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'AFE', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASOIL',   real(asoil_fe_fac ))
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ACORS',   real(acors_fe_fac ))
 
             ! Map PM1_SI - PM1 Silicon
             CASE ( ID_PM1_SI ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'ASI', 1.0 )
 
             ! Map PM1_TI - PM1 Titanium
             CASE ( ID_PM1_TI ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'ATI', 1.0 )
 
             ! Map PM1_MN - PM1 Manganese
             CASE ( ID_PM1_MN ) 
               N_VARS = 3
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'AMN', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASOIL',   real(asoil_mn_fac ))
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ACORS',   real(acors_mn_fac ))
 
             ! Map PM1_AL - PM1 Aluminum
             CASE ( ID_PM1_AL ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'AAL', 1.0 )
  
             ! Map PM1_Soil - PM1.0 Reconstructed Soil for the fine
             ! mode + explicit soil for the coarse mode
             CASE ( ID_PM1_SOIL ) 
               N_VARS = 6
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'ASOIL', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AAL', 2.20 )
               CALL SET_ELMO_COMP( J, 3, 'ASI', 2.49 )
               CALL SET_ELMO_COMP( J, 4, 'ACA', 1.63 )
               CALL SET_ELMO_COMP( J, 5, 'AFE', 2.42 )
               CALL SET_ELMO_COMP( J, 6, 'ATI', 1.94 )
   
             ! Map PM1.0 Unspeciated PM based on reconstructed
             ! SOIL PM from IMPROVE method and including NCOM in the
             ! unspeciated portion
             CASE ( ID_PM1_UNSP1 )
               N_VARS = 9
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'PM1', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'ASO4', -1.0 )
               CALL SET_ELMO_COMP( J, 3, 'ANO3', -1.0 )
               CALL SET_ELMO_COMP( J, 4, 'ANH4', -1.0 )
               CALL SET_ELMO_COMP( J, 5, 'ACL', -1.0 )          
               CALL SET_ELMO_COMP( J, 6, 'ANA', -1.0 )          
               CALL SET_ELMO_COMP( J, 7, 'AEC', -1.0 )          
               CALL SET_ELMO_COMP( J, 8, 'PM1_OC', -1.0 )          
               CALL SET_ELMO_COMP( J, 9, 'PM1_SOIL', -1.0 )          
  
             ! Map PM1_UN - PM1.0 Unspeciated Coarse Mass
             CASE ( ID_PM1_UN ) 
               N_VARS = 3
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'ASEACAT', 1.0 - real( ascat_na_fac + ascat_mg_fac +  
     &                                                                    ascat_k_fac  + ascat_ca_fac  )) 
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASOIL',   1.0 - real( asoil_na_fac + asoil_mg_fac +
     &                                                                    asoil_k_fac  + asoil_ca_fac +
     &                                                                    asoil_mn_fac + asoil_fe_fac  ))
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ACORS',   1.0 - real( acors_na_fac + acors_mg_fac +
     &                                                                    acors_k_fac  + acors_ca_fac +
     &                                                                    acors_mn_fac + acors_fe_fac  ))
              
             ! Map PM1_HP - PM1.0 Hydronium Ion
             CASE ( ID_PM1_HP ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM1' )
               CALL SET_ELMO_COMP( J, 1, 'AH3OP', 1.0/19.0 ) 
             
             !!!! PM2.5 Variables !!!!  

             ! Map PM25_SO4 - PM2.5 Sulfate
             CASE ( ID_PM25_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ASO4', 1.0 )
 
             ! Map PM25_NO3 - PM2.5 Nitrate
             CASE ( ID_PM25_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ANO3', 1.0 )
 
             ! Map PM25_NH4 - PM2.5 Ammonium
             CASE ( ID_PM25_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ANH4', 1.0 )
 
             ! Map PM25_CL - PM2.5 Chloride
             CASE ( ID_PM25_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ACL', 1.0 )
 
             ! Map PM25_NA - PM2.5 Sodium
             CASE ( ID_PM25_NA ) 
               N_VARS = 4
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'ANA', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASEACAT', real(ascat_na_fac ))
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ASOIL',   real(asoil_na_fac ))
               CALL SET_ELMO_COMP( J, 4, 'AERO', 'ACORS',   real(acors_na_fac ))
 
             ! Map PM25_EC - PM2.5 Elemental Carbon
             CASE ( ID_PM25_EC ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'AEC', 1.0 )
 
             ! Map PM25_MG - PM2.5 Magnesium
             CASE ( ID_PM25_MG ) 
               N_VARS = 4
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'AMG', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASEACAT', real(ascat_mg_fac ))
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ASOIL',   real(asoil_mg_fac ))
               CALL SET_ELMO_COMP( J, 4, 'AERO', 'ACORS',   real(acors_mg_fac ))
 
             ! Map PM25_K - PM2.5 Potassium
             CASE ( ID_PM25_K ) 
               N_VARS = 4
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'AK', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASEACAT', real(ascat_k_fac ))
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ASOIL',   real(asoil_k_fac ))
               CALL SET_ELMO_COMP( J, 4, 'AERO', 'ACORS',   real(acors_k_fac ))
 
             ! Map PM25_CA - PM2.5 Calcium
             CASE ( ID_PM25_CA ) 
               N_VARS = 4
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'ACA', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASEACAT', real(ascat_ca_fac ))
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ASOIL', real(asoil_ca_fac ))
               CALL SET_ELMO_COMP( J, 4, 'AERO', 'ACORS', real(acors_ca_fac ))
 
             ! Map PM25_Other - PM25 Other 
             CASE ( ID_PM25_OT ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'AOTHR', 1.0 )
 
             ! Map PM25_FE - PM2.5 Iron
             CASE ( ID_PM25_FE ) 
               N_VARS = 3
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'AFE', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASOIL', real(asoil_fe_fac ))
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ACORS', real(acors_fe_fac ))
 
             ! Map PM25_SI - PM2.5 Silicon
             CASE ( ID_PM25_SI ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ASI', 1.0 )
 
             ! Map PM25_TI - PM2.5 Titanium
             CASE ( ID_PM25_TI ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ATI', 1.0 )
 
             ! Map PM25_MN - PM2.5 Manganese
             CASE ( ID_PM25_MN ) 
               N_VARS = 3
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'AMN', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASOIL', real(asoil_mn_fac ))
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ACORS', real(acors_mn_fac ))
 
             ! Map PM25_AL - PM2.5 Aluminum
             CASE ( ID_PM25_AL ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'AAL', 1.0 )
 
             ! Map PM25_Soil - PM2.5 Reconstructed Soil for the fine
             ! mode + explicit soil for the coarse mode
             CASE ( ID_PM25_SOIL ) 
               N_VARS = 6
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ASOIL', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AAL', 2.20 )
               CALL SET_ELMO_COMP( J, 3, 'ASI', 2.49 )
               CALL SET_ELMO_COMP( J, 4, 'ACA', 1.63 )
               CALL SET_ELMO_COMP( J, 5, 'AFE', 2.42 )
               CALL SET_ELMO_COMP( J, 6, 'ATI', 1.94 )
   
             ! Map PM2.5 Unspeciated PM based on reconstructed
             ! SOIL PM from IMPROVE method and including NCOM in the
             ! unspeciated portion
             CASE ( ID_PM25_UNSP1 )
               N_VARS = 9
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'PM25', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'ASO4', -1.0 )
               CALL SET_ELMO_COMP( J, 3, 'ANO3', -1.0 )
               CALL SET_ELMO_COMP( J, 4, 'ANH4', -1.0 )
               CALL SET_ELMO_COMP( J, 5, 'ACL', -1.0 )          
               CALL SET_ELMO_COMP( J, 6, 'ANA', -1.0 )          
               CALL SET_ELMO_COMP( J, 7, 'AEC', -1.0 )          
               CALL SET_ELMO_COMP( J, 8, 'PM25_OC', -1.0 )          
               CALL SET_ELMO_COMP( J, 9, 'PM25_SOIL', -1.0 )          
 
             ! Map PM25_UN - PM2.5 Unspeciated Coarse Mass
             CASE ( ID_PM25_UN ) 
               N_VARS = 3
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'ASEACAT', 1.0 - real( ascat_na_fac + ascat_mg_fac +  
     &                                                                    ascat_k_fac  + ascat_ca_fac  )) 
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASOIL',   1.0 - real( asoil_na_fac + asoil_mg_fac +
     &                                                                    asoil_k_fac  + asoil_ca_fac +
     &                                                                    asoil_mn_fac + asoil_fe_fac  ))
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ACORS',   1.0 - real( acors_na_fac + acors_mg_fac +
     &                                                                    acors_k_fac  + acors_ca_fac +
     &                                                                    acors_mn_fac + acors_fe_fac  ))
 
             ! Map PM25_HP - PM2.5 Hydronium Ion
             CASE ( ID_PM25_HP ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'AH3OP', 1.0/19.0 )
 
             
             !!!! PM2.5 - 10.0 Variables !!!!  

             ! Map PM25to10_SO4 - PM2.5-10.0 Sulfate
             CASE ( ID_PM25to10_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25TO10' )
               CALL SET_ELMO_COMP( J, 1, 'ASO4', 1.0 )
 
             ! Map PM25to10_NO3 - PM2.5-10.0 Nitrate
             CASE ( ID_PM25to10_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25TO10' )
               CALL SET_ELMO_COMP( J, 1, 'ANO3', 1.0 )
 
             ! Map PM25to10_NH4 - PM2.5-10.0 Ammonium
             CASE ( ID_PM25to10_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25TO10' )
               CALL SET_ELMO_COMP( J, 1, 'ANH4', 1.0 )
 
             ! Map PM25to10_CL - PM2.5-10.0 Chloride
             CASE ( ID_PM25to10_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25TO10' )
               CALL SET_ELMO_COMP( J, 1, 'ACL', 1.0 )
 
             ! Map PM25to10_NA - PM2.5-10.0 Sodium
             CASE ( ID_PM25to10_NA ) 
               N_VARS = 4
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25TO10' )
               CALL SET_ELMO_COMP( J, 1, 'AERO', 'ANA', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'AERO', 'ASEACAT', real(ascat_na_fac ))
               CALL SET_ELMO_COMP( J, 3, 'AERO', 'ASOIL',   real(asoil_na_fac ))
               CALL SET_ELMO_COMP( J, 4, 'AERO', 'ACORS',   real(acors_na_fac ))
 
             !!!! Toxics !!!!  
 
             ! Map PM25_HDSL - HAP inventory PM2.5 diesel mass
             CASE ( ID_PM25_HDSL ) 
               N_VARS = 6
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ADE_OTHR', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'ADE_EC', 1.0 )
               CALL SET_ELMO_COMP( J, 3, 'ADE_OC', 1.0 )
               CALL SET_ELMO_COMP( J, 4, 'ADE_SO4', 1.0 )
               CALL SET_ELMO_COMP( J, 5, 'ADE_NO3', 1.0 )
               CALL SET_ELMO_COMP( J, 6, 'ADE_CORS', 1.0 )
 
             ! Map PM25_HBE - HAP inventory Beryllium
             CASE ( ID_PM25_HBE ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ABE', 1.0 )
 
             ! Map PM25_HCD - HAP inventory Cadmium
             CASE ( ID_PM25_HCD ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ACD', 1.0 )
 
             ! Map PM25_HCR3 - HAP inventory Chromium III
             CASE ( ID_PM25_HCR3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ACR_III', 1.0 )
 
             ! Map PM25_HCR6 - HAP inventory Chromium VI
             CASE ( ID_PM25_HCR6 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ACR_VI', 1.0 )
 
             ! Map PM25_HCR - HAP inventory Total Chromium
             CASE ( ID_PM25_HCR ) 
               N_VARS = 2
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'PM25_HCR3', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'PM25_HCR6', 1.0 )
 
             ! Map PM25_HPB - HAP inventory Lead
             CASE ( ID_PM25_HPB ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'APB', 1.0 )
 
             ! Map PM25_HMN - HAP inventory Manganese
             CASE ( ID_PM25_HMN ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'AMN_HAPS', 1.0 )
 
             ! Map PM25_HNI - HAP inventory Nickel
             CASE ( ID_PM25_HNI ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ANI', 1.0 )
 
             ! Map PM25_HAS - HAP inventory Arsenic
             CASE ( ID_PM25_HAS ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'AAS', 1.0 )
 
             ! Map PM25_HG - Mercury
             CASE ( ID_PM25_HG ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'APHG', 1.0 )
 
             ! Map PM25_BENAPY - Benzo-a-Pyrene
             CASE ( ID_PM25_BENAPY ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM25' )
               CALL SET_ELMO_COMP( J, 1, 'ABENAPY', 1.0 )
 
             ! Map PM10_HDSL - HAP inventory PM2.5 diesel mass
             CASE ( ID_PM10_HDSL ) 
               N_VARS = 6
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM10' )
               CALL SET_ELMO_COMP( J, 1, 'ADE_OTHR', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'ADE_EC', 1.0 )
               CALL SET_ELMO_COMP( J, 3, 'ADE_OC', 1.0 )
               CALL SET_ELMO_COMP( J, 4, 'ADE_SO4', 1.0 )
               CALL SET_ELMO_COMP( J, 5, 'ADE_NO3', 1.0 )
               CALL SET_ELMO_COMP( J, 6, 'ADE_CORS', 1.0 )
 
             ! Map PM10_HBE - HAP inventory Beryllium
             CASE ( ID_PM10_HBE ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM10' )
               CALL SET_ELMO_COMP( J, 1, 'ABE', 1.0 )
 
             ! Map PM10_HCD - HAP inventory Cadmium
             CASE ( ID_PM10_HCD ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM10' )
               CALL SET_ELMO_COMP( J, 1, 'ACD', 1.0 )
 
             ! Map PM10_HCR3 - HAP inventory Chromium III
             CASE ( ID_PM10_HCR3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM10' )
               CALL SET_ELMO_COMP( J, 1, 'ACR_III', 1.0 )
 
             ! Map PM10_HCR6 - HAP inventory Chromium VI
             CASE ( ID_PM10_HCR6 ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM10' )
               CALL SET_ELMO_COMP( J, 1, 'ACR_VI', 1.0 )
 
             ! Map PM10_HCR - HAP inventory Total Chromium
             CASE ( ID_PM10_HCR ) 
               N_VARS = 2
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM10' )
               CALL SET_ELMO_COMP( J, 1, 'PM10_HCR3', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'PM10_HCR6', 1.0 )
 
             ! Map PM10_HPB - HAP inventory Lead
             CASE ( ID_PM10_HPB ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM10' )
               CALL SET_ELMO_COMP( J, 1, 'APB', 1.0 )
 
             ! Map PM10_HMN - HAP inventory Manganese
             CASE ( ID_PM10_HMN ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM10' )
               CALL SET_ELMO_COMP( J, 1, 'AMN_HAPS', 1.0 )
 
             ! Map PM10_HNI - HAP inventory Nickel
             CASE ( ID_PM10_HNI ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM10' )
               CALL SET_ELMO_COMP( J, 1, 'ANI', 1.0 )
 
             ! Map PM10_HAS - HAP inventory Arsenic
             CASE ( ID_PM10_HAS ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM10' )
               CALL SET_ELMO_COMP( J, 1, 'AAS', 1.0 )
 
             ! Map PM10_HG - Mercury
             CASE ( ID_PM10_HG ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM10' )
               CALL SET_ELMO_COMP( J, 1, 'APHG', 1.0 )
 
             ! Map PM10_BENAPY - Benzo-a-Pyrene
             CASE ( ID_PM10_BENAPY ) 
               N_VARS = 1
               CALL INIT_ELMO_COMP( J, N_VARS, 'PM10' )
               CALL SET_ELMO_COMP( J, 1, 'ABENAPY', 1.0 )
 
             !!!! Other Variables !!!!  

             ! Map TNO3 - Total Inorganic Nitrate
             CASE ( ID_TNO3 ) 
               N_VARS = 2
               CALL INIT_ELMO_COMP( J, N_VARS, 'ALL' )
               CALL SET_ELMO_COMP( J, 1, 'ANO3', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'HNO3', 1.0 ) 
 
             ! Map TNO3TOT - Total Inorganic+Organic Nitrate
             CASE ( ID_TNO3TOT ) 
               N_VARS = 4
               CALL INIT_ELMO_COMP( J, N_VARS, 'ALL' )
               CALL SET_ELMO_COMP( J, 1, 'ANO3', 1.0 )
               CALL SET_ELMO_COMP( J, 2, 'HNO3', 1.0 ) 
               CALL SET_ELMO_COMP( J, 3, 'AISOPNN', 1.0 ) ! Convert to just NO3
               CALL SET_ELMO_COMP( J, 4, 'AMTNO3', 1.0 )      ! Convert to just NO3
 
          END SELECT

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_CGRID ) THEN
           ! CGRID Variables are already mapped

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_AEROBULK ) THEN
           ! Bulk Aerosols will be retrieved manually

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_FAM ) THEN
           ! Chemical Family
           ELMO_TABLE( J )%L_COMP = .TRUE.
           N_VARS = CHEMFAMILYNUM( ID )
           CALL INIT_ELMO_COMP( J, N_VARS, 'ALL' )
           DO K = 1,N_VARS
             CALL SET_ELMO_COMP( J, K, CHEMFAMILYMEMBERS(ID,K), 1.0 )
           END DO

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_WDEP ) THEN
           ! Wet Dep Variables are already mapped

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_DDEP ) THEN
           ! Dry Dep Variables are already mapped

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_ISAM ) THEN

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_ISAM_WDEP ) THEN

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_ISAM_DDEP ) THEN

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_DDM ) THEN

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_DDM_WDEP ) THEN

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_DDM_DDEP ) THEN

         END IF


      END DO
      END SUBROUTINE MAP_ELMO_VARIABLES
 
