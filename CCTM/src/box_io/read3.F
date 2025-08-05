
        LOGICAL FUNCTION READ3 ( FNAME, VNAME, 
     &                    LAY1,
     &                    JDATE, JTIME, BUFFER )


C***********************************************************************
C
C  FUNCTION:  Mimics IO/API function DESC3 by putting selected
C             file description data in commons.
C
C  RETURN VALUE:  TRUE iff successful
C
C  PRECONDITIONS REQUIRED: None
C
C  SUBROUTINES AND FUNCTIONS CALLED: None
C
C  REVISION  HISTORY: Prototype created by Golam Sarwar - March, 2004
C  REVISION  HISTORY: This file was Created from INTERP3
C  REVISION  HISTORY: INTERP3 was created Prototype created by Jerry Gipson IN JULY 1997	
C
C***********************************************************************
      USE SCENE_DATA
      USE DRIVER_INPUTS
      USE SCENE_DATA

      IMPLICIT NONE

C...........   INCLUDES:
       INCLUDE SUBST_FILES_ID  ! I/O definitions and declarations 

C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
        INTEGER,       INTENT(IN   ) :: LAY1            !  upper layer bound for READ3
        INTEGER,       INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
        INTEGER,       INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
        REAL,          INTENT(  OUT) :: BUFFER( * )     !  interpolation-output buffer array

        INTEGER  L
        INTEGER  LAYB, LAYT
C.............................................................................
C   begin body of subroutine  OPEN3


      READ3 = .FALSE.      
     
      IF( LAY1 .GT. 1 )THEN
          LAYB = LAY1
          LAYT = LAY1
      ELSE
          LAYB = 1
          LAYT = 1
      END IF

      DO L = LAYB, LAYT
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_2D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( FNAME .EQ. MET_CRO_2D ) THEN
   
            IF ( VNAME .EQ. 'PRES' ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER (L) = BXM_PRES
                  READ3 = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
         
            IF ( VNAME .EQ. 'WBAR' ) THEN
               BUFFER (L) = WBAR
               READ3 = .TRUE.
               RETURN
            ENDIF
                                
            IF ( VNAME .EQ. 'SEAICE' ) THEN
                 BUFFER (L) = BXM_SEAICE
                 READ3 = .TRUE.
                 RETURN
            ENDIF

            IF ( VNAME .EQ. 'CLDT' ) THEN
               BUFFER (L) = BXM_CLDT
               READ3 = .TRUE.
               RETURN
            ENDIF
   
            IF ( VNAME .EQ. 'CLDB' ) THEN
               BUFFER (L) = BXM_CLDB
               READ3 = .TRUE.
               RETURN
            ENDIF
   
            IF ( VNAME .EQ. 'CFRAC' ) THEN
               BUFFER (L) = BXM_CFRAC
               READ3 = .TRUE.
               RETURN
            ENDIF
      
   
         ENDIF
   
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_3D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( FNAME .EQ. MET_CRO_3D ) THEN
   
            IF ( VNAME .EQ. 'TA' ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER (L) = CELL_TEMP
                  READ3 = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( VNAME .EQ. 'PRES' ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER (L) = CELL_PRES
                  READ3 = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( VNAME .EQ. 'QV' ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER (L) = QV
                  READ3 = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( VNAME .EQ. 'DENS' ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER (L) = DENS
                  READ3 = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( VNAME .EQ. 'DENSA_J' ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER (L) = DENS_J
                  READ3 = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( VNAME .EQ. 'ZH' ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER (L) = 50.0
                  READ3 = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
   
         ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  OCEAN_1 variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( FNAME .EQ. OCEAN_1 ) THEN
   
            IF ( VNAME .EQ. 'OPEN' ) THEN
               BUFFER (L) = BXM_OPEN
               READ3 = .TRUE.
               RETURN
            ENDIF
   
            IF ( VNAME .EQ. 'SURF' ) THEN
               BUFFER (L) = BXM_SURF
               READ3 = .TRUE.
               RETURN
            ENDIF
   
         ENDIF
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  GRID_CRO_2D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( FNAME .EQ. GRID_CRO_2D ) THEN
   
            IF ( VNAME .EQ. 'LAT' ) THEN
               BUFFER (L) = BXM_LAT
               READ3 = .TRUE.
               RETURN
            ENDIF
   
            IF ( VNAME .EQ. 'LON' ) THEN
               BUFFER (L) = BXM_LON
               READ3 = .TRUE.
               RETURN
            ENDIF
                                 
            IF ( VNAME .EQ. 'OPEN' ) THEN
               BUFFER (L) = BXM_OPEN
               READ3 = .TRUE.
               RETURN
            ENDIF
   
            IF ( VNAME .EQ. 'SURF' ) THEN
               BUFFER (L) = BXM_SURF
               READ3 = .TRUE.
               RETURN
            ENDIF
   
            IF ( VNAME .EQ. 'MSFX2' ) THEN
               BUFFER (L) = 1.0
               READ3 = .TRUE.
               RETURN
            ENDIF
   
            IF ( VNAME .EQ. 'HT' ) THEN
               BUFFER (L) = BXM_HT
               READ3 = .TRUE.
               RETURN
            ENDIF
   
         ENDIF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  EMIS_1variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       IF ( FNAME .EQ. EMIS_1 ) THEN
!           BUFFER (L) = 0.0
!           READ3 = .TRUE.
!           RETURN
!        ENDIF

            BUFFER (L) = 1.0E-30
            PRINT*,"READ3: Unknown file and Variable, ",TRIM(FNAME)," and ",TRIM(VNAME)
            PRINT*,"Setting ", TRIM(VNAME),' to 1.0E-30'
            READ3 = .TRUE.
         
   
      END DO
      
      RETURN

      END FUNCTION READ3



