        LOGICAL FUNCTION XTRACT_METGEODATA( VNAME,VNAME_VALUE )

           USE SCENE_DATA
           USE RUNTIME_VARS, ONLY : LOGDEV, OUTDEV

           IMPLICIT NONE

           CHARACTER *(*), INTENT(  IN ) :: VNAME       ! VNAME name
           REAL,           INTENT( OUT ) :: VNAME_VALUE ! 1.0e-30 if not found


           VNAME_VALUE       = 1.0E-30
           XTRACT_METGEODATA = .FALSE.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_2D VNAMEs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            IF ( VNAME .EQ. 'PRSFC' ) THEN
                  VNAME_VALUE = BXM_PRES
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF

            IF ( VNAME .EQ. 'TEMPG' ) THEN
                  VNAME_VALUE  = BXM_TEMP
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF

            IF ( VNAME .EQ. 'TSEASFC' ) THEN
                  VNAME_VALUE  = BXM_TEMP
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF

            IF ( VNAME .EQ. 'TEMP2' ) THEN
                  VNAME_VALUE  = BXM_TEMP
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF

           IF (  VNAME .EQ. 'PRES'  ) THEN
               VNAME_VALUE  = BXM_PRES
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF
         
            IF (  VNAME .EQ. 'WBAR'  ) THEN
               VNAME_VALUE  = BXM_WBAR
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF
                                
            IF (  VNAME .EQ. 'SEAICE'  ) THEN
                 VNAME_VALUE  = BXM_SEAICE
                 XTRACT_METGEODATA = .TRUE.
                 RETURN
            ENDIF

            IF (  VNAME .EQ. 'SNOCOV'  ) THEN
                 VNAME_VALUE  = BXM_SNOCOV
                 XTRACT_METGEODATA = .TRUE.
                 RETURN
            ENDIF

            IF (  VNAME .EQ. 'CLDT'  ) THEN
               VNAME_VALUE  = BXM_CLDT
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'CLDB'  ) THEN
               VNAME_VALUE  = BXM_CLDB
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'CFRAC'  ) THEN
               VNAME_VALUE  = BXM_CFRAC
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF

            IF (  VNAME .EQ. 'SLTYP'  ) THEN
               VNAME_VALUE  = BXM_SLTYP
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF
   
   
            IF ( VNAME .EQ. 'RCA' 
     &            .OR. VNAME .EQ. 'RC' ) THEN
               VNAME_VALUE  = -1.0
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF

            IF ( VNAME .EQ. 'RN' ) THEN
               VNAME_VALUE  = BXM_RN
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF

            IF ( VNAME .EQ. 'MOLI' ) THEN
                VNAME_VALUE  = BXM_MOLI
                XTRACT_METGEODATA = .TRUE.
                RETURN
            ENDIF
  
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_3D VNAMEs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            IF (  VNAME .EQ. 'TA'  ) THEN
                  VNAME_VALUE  = BXM_TEMP
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'PRES'  ) THEN
                  VNAME_VALUE  = BXM_PRES
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'QV'  ) THEN
                  VNAME_VALUE  = BXM_QV
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'QC'  ) THEN
                  VNAME_VALUE  = BXM_QC
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'QR'  ) THEN
                  VNAME_VALUE  = BXM_QR
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'DENS'  ) THEN
                  VNAME_VALUE  = BXM_DENS
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'DENSA_J'  ) THEN
                  VNAME_VALUE  = DENS_J
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'JACOBM'  ) THEN
                  VNAME_VALUE  = JACOBM
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'JACOBF'  ) THEN
                  VNAME_VALUE  = JACOBF
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'ZH'  ) THEN
                  VNAME_VALUE  = BXM_ZH
                  WRITE( LOGDEV, '(A,F,2X,F)'),'XTRACT_METGEODATA: BXM_ZH,ZH = ',BXM_ZH,VNAME_VALUE
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF

            IF (  VNAME .EQ. 'ZF'  ) THEN
                  VNAME_VALUE  = BXM_ZF
                  WRITE( LOGDEV, '(A,F,2X,F)'),'XTRACT_METGEODATA: BXM_ZF,ZF = ',BXM_ZF,VNAME_VALUE
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  OCEAN_1 VNAMEs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            IF (  VNAME .EQ. 'OPEN'  ) THEN
               VNAME_VALUE  = BXM_OPEN
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'SURF'  ) THEN
               VNAME_VALUE  = BXM_SURF
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF


            IF (  VNAME .EQ. 'CHLO'  ) THEN
               VNAME_VALUE  = BXM_CHLO
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF



            IF (  VNAME .EQ. 'DMS'  ) THEN
               VNAME_VALUE  = BXM_DMS
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  GRID_CRO_2D VNAMEs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            IF (  VNAME .EQ. 'LAT'  ) THEN
               VNAME_VALUE  = BXM_LAT
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'LON'  ) THEN
               VNAME_VALUE  = BXM_LON
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF
                                 
            IF (  VNAME .EQ. 'OPEN' ) THEN
               VNAME_VALUE  = BXM_OPEN
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'SURF' ) THEN
               VNAME_VALUE  = BXM_SURF
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'MSFX2' ) THEN
               VNAME_VALUE  = 1.0
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF
   
            IF (  VNAME .EQ. 'HT' ) THEN
               VNAME_VALUE  = BXM_HT
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF

! Assuming NLCD40 is set LANDUSE scheme in centralized_io_module   
            IF ( VNAME(1:7) .EQ. 'LUFRAC_' ) THEN
              IF ( TRIM(VNAME) .EQ. TRIM(BXM_LU) ) THEN
                 VNAME_VALUE = 1.0
                 WRITE( LOGDEV, '(A,A,A3,F)'),'XTRACT_METGEODATA: ',TRIM(VNAME),' = ', VNAME_VALUE
              ELSE
                 VNAME_VALUE = 0.0
              END IF
              XTRACT_METGEODATA = .TRUE.
              RETURN
            END IF

            RETURN

        END FUNCTION XTRACT_METGEODATA
