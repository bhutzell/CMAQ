
        LOGICAL   FUNCTION  OPEN3 ( FNAME , FSTATUS , PNAME)
C***********************************************************************
C  FUNCTION:  Mimics IO/API function DESC3 by putting selected
C             file description data in commons.
C
C  RETURN VALUE:  TRUE if successful
C
C  PRECONDITIONS REQUIRED: None
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C***********************************************************************

      USE M3UTILIO, ONLY: OUTPUT_FILE,N_OUTPUT_FILES,OUTPUT_FILES,SETUP_OUTPUT_FILE,
     &                    NAMEVAL,MAX_IO_STRLEN

      IMPLICIT NONE

C...........   INCLUDES:
      INCLUDE SUBST_FILES_ID  ! I/O definitions and declarations 
      INCLUDE 'PARMS3.EXT'
      INCLUDE 'FDESC3.EXT'

C...........   ARGUMENTS and their descriptions:
        CHARACTER(LEN=*), INTENT( IN ) :: FNAME   !  logical name of file to be "opened"
        CHARACTER(LEN=*), INTENT( IN ) :: PNAME   !  Calling program name
        INTEGER,          INTENT( IN ) :: FSTATUS       !  Not used

      INTEGER :: FILE_ID
      INTEGER :: NFILE, NVAR
      INTEGER :: IO_UNIT
      INTEGER :: IOS

      CHARACTER( MAX_IO_STRLEN ) :: ACTUAL_NAME

      FILE_ID = 0

      IF ( FNAME .EQ. LUFRAC_CRO ) THEN
        OPEN3 = .FALSE.
        RETURN
      END IF

      DO NVAR = 1,N_FILES
         IF ( INDEX( LGCL_NAME(NVAR),TRIM(FNAME) )  .GT. 0 ) THEN
           FILE_ID = NVAR
           EXIT
         END IF
      END DO

      IF ( FILE_ID .GT. 0 ) THEN
         IF (  FSTATUS .EQ. FSREAD3 ) THEN
            OPEN3 = .TRUE.
            RETURN
         END IF
      END IF

      IF ( FILE_ID .LT. 1 ) THEN
         WRITE(6,'(A)')'OPEN3 NOTE: ' // TRIM(FNAME) // ' is not defined in FILES_CTM.EXT.'
         WRITE(6,'(A)')'File is treated as output file.'
      END IF

      IF( FSTATUS .EQ. FSRDWR3 ) THEN
        OPEN3 = .FALSE.
        RETURN
      END IF 

      OPEN3 = .TRUE.

      CALL NAMEVAL( FNAME,ACTUAL_NAME )
      NVAR = INDEX(ACTUAL_NAME,' -v') - 1
      IF( NVAR .GT. 0 )THEN
        ACTUAL_NAME = ACTUAL_NAME(1:NVAR)
      END IF

      INQUIRE(FILE = ACTUAL_NAME, NUMBER=IOS)
      IF ( FSTATUS .EQ. FSNEW3 .AND. IOS .GT. 0 )THEN
        WRITE(6,'(A)')'OPEN3 NOTE: ' // TRIM(FNAME) // ' is already open.'
        OPEN3 = .FALSE.
        RETURN
      END IF

      OPEN(NEWUNIT = IO_UNIT,FILE = ACTUAL_NAME, FORM ='FORMATTED', STATUS='UNKNOWN',
     &     IOSTAT=IOS, ERR=1000 )
      OPEN3 = .TRUE.

      N_OUTPUT_FILES = N_OUTPUT_FILES + 1
      NVAR = 1
      OPEN3 = SETUP_OUTPUT_FILE( FNAME,NVARS3D,UNITS3D,VNAME3D,
     &                           NVAR,IO_UNIT,SDATE3D,STIME3D,N_OUTPUT_FILES )

      IF( OPEN3 )THEN 
         WRITE(6,'(A)')'OPEN3 created file, ' // TRIM(FNAME) // ', for routine, '
     &                 // TRIM(PNAME) // '.'
         WRITE(IO_UNIT,'(2(A8,", "))',ADVANCE='NO')'JDATE','JTIME'
         WRITE(IO_UNIT,'(4000(A34,", "))')
     &   (TRIM(VNAME3D(NVAR)) // "(" // TRIM(UNITS3D(NVAR)) // ')',NVAR=1,NVARS3D)
         RETURN
         DO NFILE = 1,N_OUTPUT_FILES
           write(6,'(I3,1X,6(A,1X))')NFILE,'OPEN3: Created OUTPUT_FILES(NFILE)% = ',
     &      TRIM(OUTPUT_FILES(NFILE)%FILENAME)
           DO NVAR = 1,OUTPUT_FILES(NFILE)%NVARS
             write(6,'(6(A,1X))')'OPEN3: OUTPUT_FILES(NFILE)%VARNAMES(NVAR),UNITS(NVAR) = ',
     &       TRIM(OUTPUT_FILES(NFILE)%VARNAMES(NVAR)),TRIM(OUTPUT_FILES(NFILE)%UNITS(NVAR))
           END DO
         END DO 
      END IF
1000  OPEN3 = .FALSE.
      WRITE(6,'(A)')'OPEN3 ERROR: Failed to create file, ' // TRIM(FNAME) // ', for routine, '
     &               // TRIM(PNAME) // '.'

      RETURN
      END FUNCTION OPEN3
