
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
        CHARACTER*(*), INTENT( IN ) :: FNAME   !  logical name of file to be "opened"
        CHARACTER*(*), INTENT( IN ) :: PNAME   !  Calling program name
        INTEGER,       INTENT( IN ) :: FSTATUS       !  Not used

      INTEGER :: FILE_ID
      INTEGER :: NFILE, NVAR
      INTEGER :: IO_UNIT
      INTEGER :: IOS
      INTEGER :: L1,L2,LN
      LOGICAL :: ESTAT

      CHARACTER( MAX_IO_STRLEN ) :: ACTUAL_NAME

      FILE_ID = 0

      IF ( FNAME .EQ. LUFRAC_CRO ) THEN
        OPEN3 = .FALSE.
        RETURN
      END IF

      LOOP_FILES:DO NVAR = 1,N_FILES

         IF ( INDEX( LGCL_NAME(NVAR),TRIM(FNAME) )  .GT. 0 ) THEN
!        IF ( LGCL_NAME(NVAR) .EQ. FNAME ) THEN
           FILE_ID = NVAR
!        print*,'OPEN3: STRINGS MATCH, first method.'
           EXIT LOOP_FILES
         END IF
         L1 = LEN_TRIM(LGCL_NAME(NVAR))
         L2 = LEN_TRIM(FNAME)
!        print*,'OPEN3:LEN_TRIM(LGCL_NAME(NVAR)),LEN_TRIM(FNAME) = ', L1,L2
         IF ( L1 .NE. L2 ) CYCLE LOOP_FILES
!        print*,'OPEN3: Scan ' // TRIM(LGCL_NAME(NVAR)) // ' for ' // TRIM(FNAME) 
         LOOP_COMPARE: DO LN = 1,L2
            IF ( LGCL_NAME(NVAR)(LN:LN) .NE. FNAME(LN:LN) ) CYCLE LOOP_FILES
         END DO LOOP_COMPARE
!        print*,'OPEN3: STRINGS MATCH'
         FILE_ID = NVAR
         EXIT LOOP_FILES

      END DO LOOP_FILES

      IF ( FILE_ID .GT. 0 ) THEN
         IF (  FSTATUS .EQ. FSREAD3 ) THEN
            OPEN3 = .TRUE.
            RETURN
         END IF
      END IF

      IF ( FILE_ID .LT. 1 ) THEN
         WRITE(6,'(A)')'OPEN3 NOTE for ' // TRIM(PNAME) // ': ' 
     &    // TRIM(FNAME) // ' is not defined in FILES_CTM.EXT.'
         WRITE(6,'(A)')'File is treated as output file.'
      END IF

      OPEN3 = .TRUE.

      CALL NAMEVAL( FNAME,ACTUAL_NAME )
      NVAR = INDEX(ACTUAL_NAME,' -v') - 1
      IF( NVAR .GT. 0 )THEN
        ACTUAL_NAME = ACTUAL_NAME(1:NVAR)
      END IF

      INQUIRE(FILE = ACTUAL_NAME, OPENED = ESTAT, NUMBER=IOS )
      print*,'OPEN3: IOS = ',IOS
      IF ( FSTATUS .EQ. FSNEW3 .AND. ESTAT )THEN
        WRITE(6,'(A)')'OPEN3 NOTE: ' // TRIM(FNAME) // ' is already open.'
        OPEN3 = .FALSE.
        RETURN
      ELSE IF( FSTATUS .EQ. FSRDWR3 ) THEN
        IF ( ESTAT ) THEN
           WRITE(6,'(A)')'OPEN3 Error for ' // TRIM(PNAME) // ': Read-Write file, ' 
     &     // TRIM(FNAME) // ', is already open.'
           OPEN3 = .TRUE.
        ELSE
           WRITE(6,'(A)')'OPEN3 Error for ' // TRIM(PNAME) // ': Read-Write file, ' 
     &     // TRIM(FNAME) // ', is not open.'
           OPEN3 = .FALSE.
        END IF
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
         WRITE(IO_UNIT,'(2(A7,","))',ADVANCE='NO')'JDATE','JTIME'
         WRITE(IO_UNIT,'(4000(A34,","))')
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
