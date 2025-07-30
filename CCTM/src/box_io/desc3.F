
        LOGICAL FUNCTION  DESC3 ( FNAME )

C***********************************************************************
C  function body starts at line 65
C
C  FUNCTION:  Mimics IO/API function DESC3 by putting selected
C             file description data in commons.
C
C  RETURN VALUE:  TRUE iff successful
C
C  PRECONDITIONS REQUIRED: None
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY: Prototype created by Jerry Gipson, July, 1997
C	
C
C***********************************************************************

!     USE UTILIO_DEFN


      IMPLICIT NONE
C...........   INCLUDES:
         INCLUDE 'PARMS3.EXT'        !  I/O API PARAMETERs
         INCLUDE 'FDESC3.EXT'        !  I/O API file headers
         INCLUDE SUBST_FILES_ID  ! I/O definitions and declarations 
C...........   ARGUMENTS and their descriptions:
        CHARACTER(LEN=*), INTENT( IN ) ::  FNAME   !  logical name of file to be described.
C...........   Local:
        INTEGER :: IVAR
        LOGICAL :: FOUND
        

           DESC3 = .TRUE.      
           FOUND = .FALSE.

           NLAYS3D = 1
           TSTEP3D = 010000
           GDNAM3D = "M3BOX"

           FTYPE3D  = 1
           GDTYP3D  = 1

           P_ALP3D  = 0.0D0
           P_BET3D  = 0.0D0
           P_GAM3D  = 0.0D0

           XCENT3D  = 0.0D0
           YCENT3D  = 0.0D0

           XORIG3D  = 0.0D0
           YORIG3D  = 0.0D0

           XCELL3D = 5.0D0
           YCELL3D = 5.0D0

           NROWS3D = 1
           NCOLS3D = 1
           NTHIK3D = 1

           VNAME3D = ''
           UNITS3D = ''
           NVARS3D = 0

      IF ( FNAME .EQ. MET_CRO_2D ) THEN

          DESC3 = .TRUE.

          VNAME3D (  1 )   = 'PSTAR'
          VNAME3D (  2 )   = 'WBAR'
          VNAME3D (  3 )   = 'CLDT'         
          VNAME3D (  4 )   = 'CLDB'   
          VNAME3D (  5 )   = 'CFRAC'
          VNAME3D (  6 )   = 'TEMP2'
          VNAME3D (  7 )   = 'TEMPG'
          VNAME3D (  8 )   = 'TSEASFC'
          VNAME3D (  9 )   = 'PRSFC'
          VNAME3D ( 10 )   = 'MOLI'
          VNAME3D ( 11 )   = 'RCA'

          UNITS3D (  1 )   = 'Pa              '
          UNITS3D (  2 )   = 'G/M**3          '  
          UNITS3D (  3 )   = 'M               '  
          UNITS3D (  4 )   = 'M               '  
          UNITS3D (  5 )   = 'FRACTION (0-1)  '  
          UNITS3D (  6 )   = 'K               '  
          UNITS3D (  7 )   = 'K               '  
          UNITS3D (  8 )   = 'K               '  
          UNITS3D (  9 )   = 'Pa              '  
          UNITS3D ( 10 )   = 'm-1             '  
          UNITS3D ( 11 )   = 'cm              '  

          NVARS3D  = 11

          FOUND = .TRUE.
      
      ENDIF

      IF ( FNAME .EQ. MET_DOT_3D ) THEN
	
                DESC3         = .TRUE.

		VNAME3D( 1 ) = "UWINDC          "
		UNITS3D( 1 ) = "m s-1           "
	
		VNAME3D( 2 ) = "VWINDC          "
		UNITS3D( 2 ) = "m s-1           "

		VNAME3D( 3 ) = "UHAT_JD         "
		UNITS3D( 3 ) = "kg m-1 s-1      "

		VNAME3D( 4 ) = "VHAT_JD         "
		UNITS3D( 4 ) = "kg m-1 s-1      "

		VNAME3D( 5 ) = "UWIND           "
		UNITS3D( 5 ) = "m s-1           "

		VNAME3D( 6 ) = "VWIND           "
		UNITS3D( 6 ) = "m s-1           "

                NROWS3D = NROWS3D + 1
                NLAYS3D = NLAYS3D + 1
                
                NVARS3D = 6

                FOUND = .TRUE.
      END IF

      IF ( FNAME .EQ. MET_CRO_3D .OR. FNAME .EQ. INIT_CONC_1 ) THEN

          DESC3         = .TRUE.


          VGTYP3D = VGSGPN3 ! VGTYP3D
          VGTOP3D =  10000.0 ! VGTOP3D
          VGLVS3D( 1 ) = 1.0    
          VGLVS3D( 2 ) = 0.995


          VNAME3D (  1 )   = 'TA              '
          VNAME3D (  2 )   = 'QV              '
          VNAME3D (  3 )   = 'DENS            '         

          UNITS3D (  1 )   = 'K               '
          UNITS3D (  2 )   = 'KG/KG           '  
          UNITS3D (  3 )   = 'KG/M**3         '  
 
          IF ( FNAME .EQ. MET_CRO_3D ) THEN
            FDESC3D( 1 ) = "WRF ARW V4.5"
            VNAME3D (  4 )   = 'PRES            '         
            UNITS3D (  4 )   = 'Pa              '
            VNAME3D (  5 )   = 'JACOBM          '         
            UNITS3D (  5 )   = 'm               '
            VNAME3D (  6 )   = 'JACOBM          '         
            UNITS3D (  6 )   = 'm               '
            NVARS3D          = 6
          ELSE
            NVARS3D  =  3
          END IF


          FOUND = .TRUE.
      
      ENDIF

      IF ( FNAME .EQ. GRID_CRO_2D ) THEN

          DESC3 = .TRUE.

          VNAME3D (  1 )   = 'LAT             '
          VNAME3D (  2 )   = 'LON             '
          VNAME3D (  3 )   = 'MSFX2           '         
          VNAME3D (  4 )   = 'HT              '
          VNAME3D (  5 )   = 'LUFRAC_04       '

          UNITS3D (  1 )   = 'DEGREES         '
          UNITS3D (  2 )   = 'DEGREES         '  
          UNITS3D (  3 )   = '(M/M)**2        '  
          UNITS3D (  4 )   = 'M               '  
          UNITS3D (  5 )   = 'Fraction        '  

          NVARS3D  =  5

          FOUND = .TRUE.
      
      ENDIF

      IF ( FNAME .EQ. OCEAN_1 )THEN
          DESC3 = .TRUE.
          VNAME3D (  1 )   = 'OPEN            '
          VNAME3D (  2 )   = 'SURF            '
          VNAME3D (  3 )   = 'CHLO            '
          VNAME3D (  4 )   = 'DMS             '

          UNITS3D (  1 )   = ''
          UNITS3D (  2 )   = ''
          UNITS3D (  3 )   = ''
          UNITS3D (  4 )   = ''

          NVARS3D  =  4
          FOUND = .TRUE.
      END IF

      IF ( .NOT. FOUND ) THEN
           VNAME3D = ''
           UNITS3D = ''
           NVARS3D = 0
      END IF

      WRITE( 6,'(a)')TRIM( FNAME )
      IF ( NVARS3D .NE. 0 ) THEN
        DO IVAR = 1,NVARS3D
           WRITE(6,'(I3,1X,2(A16,1X))')IVAR,TRIM(VNAME3D(IVAR)),TRIM(UNITS3D(IVAR))
         END DO
      ELSE
         WRITE(6,'(A)')'NO File Varaibles Defined.'
      END IF

      RETURN

      END FUNCTION DESC3
