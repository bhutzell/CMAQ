!-------------------------------------------------------------------------
      SUBROUTINE ELMO_KEYWORDS
!     This subroutine allocates and populates the structure of potential
!       keywords to which users may refer to activate various ELMO
!       variables. It also contains the definitions of the ocntents of
!       each keyword.
!-------------------------------------------------------------------------
      USE ELMO_DATA
      USE CGRID_SPCS, ONLY: CGRID_NAME, N_CGRID_SPC
          
      IMPLICIT NONE

      INTEGER IOS, ALLOCSTAT
      CHARACTER(200) :: XMSG
      CHARACTER(16), SAVE :: PNAME = 'ELMO_KEY'

      N_KEY = 26
      ALLOCATE( ELMO_KEY( N_KEY ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_KEY',PNAME )

      ! Define AMET Variables
      ELMO_KEY( 1 )%WORD = 'DEFAULT'
      ELMO_KEY( 1 )%N = 14
      ALLOCATE( ELMO_KEY( 1 )%LIST( 14 ),STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_KEY(1)%LIST',PNAME )

      ELMO_KEY( 1 )%LIST = (/'AMET            ','CUTOFF_FRACTIONS',
     &      'MASS            ','PM_NUM          ','PMF_NUM         ',
     &      'PMC_NUM         ','MET             ','CHEM            ', 
     &      'FINE_SPECIES    ','COARSE_SPECIES  ','PM25_SPECIES    ',
     &      'SIZE            ','AMS             ','PM25TO10_SPECIES' /)

      ELMO_KEY( 2 )%WORD = 'AMET'
      ELMO_KEY( 2 )%N = 36
      ALLOCATE( ELMO_KEY( 2 )%LIST( 36 ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_KEY(2)%LIST',PNAME )

      ELMO_KEY( 2 )%LIST = (/'PMF_MASS    ','PMF_FRM     ','PMC_MASS    ',
     &          'PMF_OC      ','PMF_EC      ','PMF_NA      ','PMF_CL      ',
     &          'PMF_SO4     ','PMF_NO3     ','PMF_NH4     ','PM25        ',
     &          'PM25_FRM    ','PM25_SO4    ','PM25_NO3    ','PM25_NH4    ',
     &          'PM25_OC     ','PM25_EC     ','PMF_FE      ','PMF_AL      ',
     &          'PMF_SI      ','PMF_TI      ','PMF_CA      ','PMF_MG      ',
     &          'PMF_K       ','PMF_MN      ','PMF_SOILIMPV','PMF_UN_IMPV1',
     &          'PMF_NCOM    ','PMF_UN_IMPV2','TNO3        ','PM_MASS     ',
     &          'PMC_MASS    ','MET         ','Tsurf       ','AOD_550     ',
     &          'GAS_CONC    ' /)

      ELMO_KEY( 3 )%WORD = 'MASS'
      ELMO_KEY( 3 )%N = 11
      ALLOCATE( ELMO_KEY( 3 )%LIST( 11 ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_KEY(3)%LIST',PNAME )

      ELMO_KEY( 3 )%LIST = (/'PMF_MASS  ','PMC_MASS  ','PM01      ','PM1       ',
     &            'PM25      ','PM10      ','PM25TO10  ','PMU_MASS  ','PMAMS     ',
     &            'PMAIT_MASS','PMACC_MASS' /)
     
      ELMO_KEY( 4 )%WORD = 'NUMBER'
      ELMO_KEY( 4 )%N = 7
      ALLOCATE( ELMO_KEY( 4 )%LIST( 7 ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_KEY(4)%LIST',PNAME )

      ELMO_KEY( 4 )%LIST = (/'PM_NUM ','PMF_NUM','PMC_NUM','N10    ',
     &                         'N20    ','N40    ','N100   ' /)
     
      ELMO_KEY( 5 )%WORD = 'SIZE'
      ELMO_KEY( 5 )%N = 9
      ALLOCATE( ELMO_KEY( 5 )%LIST( 9 ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_KEY(5)%LIST',PNAME )

      ELMO_KEY( 5 )%LIST = (/'NUMBER  ','DRY_DG  ','WET_DG  ','STDEV   ',
     &              'DRY_M3  ','WET_M3  ','WET_M2  ','DRY_DENS','WET_DENS' /)
     
      ELMO_KEY( 6 )%WORD = 'SURFACE'
      ELMO_KEY( 6 )%N = 5
      ALLOCATE( ELMO_KEY( 6 )%LIST( 5 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(6)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 6 )%LIST = (/'PM_SRF ','PMU_SRF','PMF_SRF','PMC_SRF','WET_M2 '/)
     
      ELMO_KEY( 7 )%WORD = 'ORGANIC'
      ELMO_KEY( 7 )%N = 6
      ALLOCATE( ELMO_KEY( 7 )%LIST( 6 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(7)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 7 )%LIST = (/'FINE_ORG  ','PM1_OC    ','PM1_OA    ',
     &                         'PMAMS_OA  ','PMAMS_OTOC','PM25_OA   '/)

      ELMO_KEY( 8 )%WORD = 'MET'
      ELMO_KEY( 8 )%N = 3
      ALLOCATE( ELMO_KEY( 8 )%LIST( 3 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(8)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 8 )%LIST = (/'TA  ','PRES','RH  ' /) 

      ELMO_KEY( 9 )%WORD = 'CHEM'
      ELMO_KEY( 9 )%N = 7
      ALLOCATE( ELMO_KEY( 9 )%LIST( 7 ),STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(9)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 9 )%LIST = (/'GAMMA_N2O5  ','GAMMA_N2O5K ','YIELD_CLNO2 ',
     &          'YIELD_CLNO2K','GAMMA_IEPOX ','K_IEPOX     ','GAMMA_IMAE  ' /)

      ELMO_KEY( 10 )%WORD = 'FINE_SPECIES'
      ELMO_KEY( 10 )%N = 17
      ALLOCATE( ELMO_KEY( 10 )%LIST( 17 ),STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(10)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 10 )%LIST = (/'PMF_SO4     ','PMF_NO3     ','PMF_NH4     ',
     &           'PMF_CL      ','PMF_NA      ','PMF_EC      ','PMF_OC      ',
     &           'PMF_OA      ','PMF_H2O     ','PMF_SOILIMPV','PMF_UN_IMPV1',
     &           'PMF_UN_IMPV2','PMF_HP      ','PMF_HPMOLAL ','PMF_PH      ',
     &           'FINE_ORG    ','PMF_MASS    ' /)

      ELMO_KEY( 11 )%WORD = 'COARSE_SPECIES'
      ELMO_KEY( 11 )%N = 8
      ALLOCATE( ELMO_KEY( 11 )%LIST( 8 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(11)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 11 )%LIST = (/'PMC_MASS','PMC_SO4 ','PMC_NO3 ','PMC_NH4 ',
     &                          'PMC_NA  ','PMC_MG  ','PMC_K   ','PMC_CA  ' /)

      ELMO_KEY( 12 )%WORD = 'AMS_SPECIES'
      ELMO_KEY( 12 )%N = 7
      ALLOCATE( ELMO_KEY( 12 )%LIST( 7 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(12)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 12 )%LIST = (/'PMAMS     ','PMAMS_SO4 ','PMAMS_NO3 ',
     &             'PMAMS_NH4 ','PMAMS_CL  ','PMAMS_OA  ','PMAMS_OTOC' /)

      ELMO_KEY( 13 )%WORD = 'PM1_SPECIES'
      ELMO_KEY( 13 )%N = 21
      ALLOCATE( ELMO_KEY( 13 )%LIST( 21 ),STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(13)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 13 )%LIST = (/'PM1        ','PM1_SO4    ','PM1_NO3    ',
     &            'PM1_NH4    ','PM1_CL     ','PM1_NA     ','PM1_EC     ',
     &            'PM1_OC     ','PM1_OA     ','PM1_MG     ','PM1_K      ',
     &            'PM1_CA     ','PM1_OTHER  ','PM1_FE     ','PM1_SI     ',
     &            'PM1_TI     ','PM1_MN     ','PM1_AL     ','PM1_SOIL   ',
     &            'PM1_UNSP1  ','PM1_UNSPCRS' /)

      ELMO_KEY( 14 )%WORD = 'PM25_SPECIES'
      ELMO_KEY( 14 )%N = 22
      ALLOCATE( ELMO_KEY( 14 )%LIST( 22 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(14)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 14 )%LIST = (/'PM25        ','PM25_SO4    ','PM25_NO3    ',
     &           'PM25_NH4    ','PM25_CL     ','PM25_NA     ','PM25_EC     ',
     &           'PM25_OC     ','PM25_OA     ','PM25_MG     ','PM25_K      ',
     &           'PM25_CA     ','PM25_OTHER  ','PM25_FE     ','PM25_SI     ',
     &           'PM25_TI     ','PM25_MN     ','PM25_AL     ','PM25_SOIL   ',
     &           'PM25_UNSP1  ','PM25_UNSPCRS','PM25_HP     ' /)

      ELMO_KEY( 15 )%WORD = 'PM25TO10_SPECIES'
      ELMO_KEY( 15 )%N = 6
      ALLOCATE( ELMO_KEY( 15 )%LIST( 6 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(15)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 15 )%LIST = (/'PM25TO10    ','PM25TO10_SO4','PM25TO10_NO3',
     &                          'PM25TO10_NH4','PM25TO10_CL ','PM25TO10_NA ' /)

      ELMO_KEY( 16 )%WORD = 'OPTICAL'
      ELMO_KEY( 16 )%N = 2
      ALLOCATE( ELMO_KEY( 16 )%LIST( 2 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(16)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 16 )%LIST = (/'AOD_550   ','PM_EXT_550'/)

      ELMO_KEY( 17 )%WORD = 'FINE_ORG'
      ELMO_KEY( 17 )%N = 16
      ALLOCATE( ELMO_KEY( 17 )%LIST( 16 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(17)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 17 )%LIST = (/'PMF_POC     ','PMF_SOC     ','PMF_POA     ',
     &           'PMF_SOA     ','PMF_NCOM    ','PMF_OMOC    ','PMF_OTOC    ',
     &           'PMF_ASOA    ','PMF_BSOA    ','PMF_CLDGLY  ','PMF_ISOPSOA ',
     &           'PMF_IEPOXSOA','PMF_MTNSOA  ','PMF_MTSOA   ','PMF_OC      ',
     &           'PMF_OA      ' /)

      ELMO_KEY( 18 )%WORD = 'CUTOFF_FRACTIONS'
      ELMO_KEY( 18 )%N = 6
      ALLOCATE( ELMO_KEY( 18 )%LIST( 6 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(18)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 18 )%LIST = (/'FPM01    ','FPM1     ','FPM25    ',
     &                          'FPM10    ','FPM25TO10','FAMS     ' /)

      ELMO_KEY( 19 )%WORD = 'TOXICS'
      ELMO_KEY( 19 )%N = 21
      ALLOCATE( ELMO_KEY( 19 )%LIST( 21 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(19)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 19 )%LIST = (/'PM25_HDIESEL','PM25_HBE    ','PM25_HCD    ',
     &           'PM25_HCR3   ','PM25_HCR6   ','PM25_HCR    ','PM25_HPB    ',
     &           'PM25_HMN    ','PM25_HNI    ','PM25_HAS    ','PM25_HG     ',
     &           'PM10_HDIESEL','PM10_HBE    ','PM10_HCD    ','PM10_HCR3   ',
     &           'PM10_HCR6   ','PM10_HCR    ','PM10_HPB    ','PM10_HMN    ',
     &           'PM10_HAS    ','PM10_HG     '  /)

      ELMO_KEY( 20 )%WORD = 'SIMPLE'
      ELMO_KEY( 20 )%N = 2
      ALLOCATE( ELMO_KEY( 20 )%LIST( 2 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(20)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 20 )%LIST = (/'PM25','PM10'/)
 
      ELMO_KEY( 21 )%WORD = 'AMS'
      ELMO_KEY( 21 )%N = 5
      ALLOCATE( ELMO_KEY( 21 )%LIST( 5 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(21)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 21 )%LIST = (/'PMAMS_CL ','PMAMS_NH4','PMAMS_NO3',
     &                          'PMAMS_OA ','PMAMS_SO4'/)
 
      ! Satellite Retrieval products
      ELMO_KEY( 22 )%WORD = 'SAT'
      ELMO_KEY( 22 )%N = 2
      ALLOCATE( ELMO_KEY( 22 )%LIST( 2 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(22)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      ELMO_KEY( 22 )%LIST = (/'SATMET ','SATCONC'/)
      
      ! Satellite Retrieval Met Products
      ELMO_KEY( 23 )%WORD = 'SATMET'
      ELMO_KEY( 23 )%N = 7
      ALLOCATE( ELMO_KEY( 23 )%LIST( 7 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(23)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      ELMO_KEY( 23 )%LIST = (/'DENS ','DZ   ','ZH   ','CFRAC','PV   ',
     &                        'PRES ','TA   '/)
 
      ! Satellite Retrieval products
      ELMO_KEY( 24 )%WORD = 'SATCONC'
      ELMO_KEY( 24 )%N = 7
      ALLOCATE( ELMO_KEY( 24 )%LIST( 7 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(24)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      ELMO_KEY( 24 )%LIST = (/'NO2    ','SO2    ','O3     ','CO     ',
     &                        'FORM   ','NH3    ','AOD_550'/)
 
      ! All Concentrations
      ELMO_KEY( 25 )%WORD = 'GAS_CONC'
      ELMO_KEY( 25 )%N = 7
      ALLOCATE( ELMO_KEY( 25 )%LIST( 7 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(25)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      ELMO_KEY( 25 )%LIST = (/'O3  ','NO2 ','NO  ','SO2  ','CO  ',
     &                        'NH3 ','HNO3'/)
 
      ! All Concentrations
      ELMO_KEY( 26 )%WORD = 'ALLCONC'
      ELMO_KEY( 26 )%N = N_CGRID_SPC
      ALLOCATE( ELMO_KEY( 26 )%LIST( N_CGRID_SPC ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(26)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      ELMO_KEY( 26 )%LIST = CGRID_NAME(:)
 
      END SUBROUTINE ELMO_KEYWORDS
