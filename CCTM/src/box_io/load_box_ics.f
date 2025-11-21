      SUBROUTINE LOAD_BOX_ICS ( )
C***********************************************************************
C
C  FUNCTION:  Sets initial conditions for model
C
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C***********************************************************************

      USE SCENE_DATA
      USE UTILIO_DEFN
      USE DRIVER_INPUTS
      USE CGRID_SPCS
      USE ASCII_UTILITIES ! , Only : ICDATA_ASCII, GET_ASCII_DATA
      USE RUNTIME_VARS, Only: OUTDEV, LOGDEV

      IMPLICIT NONE
      
C..INCLUDES:
    
C..ARGUMENTS:  NONE

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*80 ENVDESC      !  environment variable description
      CHARACTER*80 XMSG         !  Log message
      CHARACTER*16 DEFNAM       !  String of blanks
      CHARACTER*656 ASCII_FNAM       ! ASCII file name 
      CHARACTER*656 IOAPI_FNAM       ! IOAPI file name 

      INTEGER    STATUS         !  ENVSTR status
      INTEGER    N              !  Loop index

      CHARACTER*16, PARAMETER :: PNAME  = 'LOAD_BOX_ICS'  !  Program name
      CHARACTER*16, PARAMETER :: BLANKS = '        '      !  String of blanks
      CHARACTER*16, PARAMETER :: IOAPI_IC = 'IOAPI_IC'     !  String of blanks

C     LOGICAL    LASCII         ! Flag for ASCII IC file
      LOGICAL    LEXIST         ! Flag for file existance
C     LOGICAL    LIOAPI         ! Flag for IOAPI IC file

C***********************************************************************
      LASCII = .FALSE.
      LIOAPI = .FALSE.

      print*,"INSIDE " // TRIM( PNAME )

      DEFNAM = BLANKS
      ENVDESC = 'Name of ASCII IC file '
      ASCII_FNAM =  ASCII_IC
      INQUIRE ( FILE = ASCII_FNAM , EXIST = LEXIST )
      IF ( LEXIST )THEN
           LASCII = .TRUE.
      ELSE
           WRITE(OUTDEV,*)'ASCII_IC ', ASCII_IC,' not found'
           CALL M3EXIT(PNAME, 0, 0, 'Fatal Error', 1)
      ENDIF

      DEFNAM = BLANKS
      ENVDESC = 'Name of IOAPI IC file '
      IOAPI_FNAM = IOAPI_IC
      IF ( LIOAPI ) THEN
         INQUIRE ( FILE = IOAPI_FNAM , EXIST = LEXIST )
         IF ( LEXIST ) LIOAPI = .TRUE.
      ENDIF

      NUMB_INIT_CONC = N_CGRID_SPC
      ALLOCATE( INIT_CONC_SPCS( NUMB_INIT_CONC ),
     &          INIT_CONC_VALUES(  NUMB_INIT_CONC ),
     &          STAT = STATUS )

       IF ( STATUS .NE. 0 ) THEN
          XMSG = TRIM( PNAME ) // 'Error allocating INIT_CONC_SPCS and INIT_CONC_VALUES '
          WRITE(OUTDEV,'(A)')TRIM( XMSG )
          CALL M3EXIT(PNAME, 0, 0, 'Fatal Error', 1)
       END IF

       INIT_CONC_SPCS = CGRID_NAME
       INIT_CONC_VALUES = 1.0E-30
       INIT_CONC_SPCS( RHOJ_LOC )   = 'BOX_DENS_J'
       INIT_CONC_VALUES( RHOJ_LOC ) = DENS_J

       IF ( .NOT. LASCII ) THEN
         XMSG = TRIM( PNAME ) 
     &        // ': No Initial Box Concentration file specified...species setting to 1.0E-30'
         WRITE(OUTDEV,'(A)')TRIM( XMSG )
       ELSE
         CALL ICDATA_ASCII ( ASCII_FNAM )
       END IF

       INQUIRE ( FILE = ASCII_MET , EXIST = LEXIST )
       IF ( LEXIST )THEN
         IF ( .NOT. GET_ASCII_DATA ( ASCII_MET, NUMB_MET_DATA, MET_DATA_SPCS, MET_DATA_VALUES ) )THEN
            CALL M3EXIT(PNAME, 0, 0, 'Error setting input MET data ', 1)
         END IF
         print*,'GET_ASCII_DATA sets NUMB_MET_DATA = ',NUMB_MET_DATA
! search in MET_DATA for LUFRAC_xy values
         DO N = 1,NUMB_MET_DATA
           IF ( INDEX( MET_DATA_SPCS( N ),"LUFRAC_" ) .GT. 0 ) THEN
             LASCII = .TRUE.
           END IF  
           WRITE(OUTDEV,'(I4,1X,A16,1X,ES12.4)')N,MET_DATA_SPCS( N ),MET_DATA_VALUES( N )
         END DO
         IF ( .NOT. LASCII ) THEN
            WRITE(OUTDEV,*)"ERROR: MET_DATA file does not include LU_FRAC_xy values such as LUFRAC_04."
            WRITE(OUTDEV,*)"File must include at least one value."
            WRITE(OUTDEV,*)"Note that box model uses the NLCD40 Landuse Scheme."
            CALL M3EXIT(PNAME, 0, 0, 'Fatal Error', 1)
         END IF
       ELSE
           WRITE(OUTDEV,*)'ASCII_MET ', ASCII_MET,' not found'
           CALL M3EXIT(PNAME, 0, 0, 'Fatal Error', 1)
       ENDIF

       RETURN
      END SUBROUTINE LOAD_BOX_ICS











