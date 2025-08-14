
        LOGICAL   FUNCTION  SHUT3 ( )

C***********************************************************************
C
C  FUNCTION:  Mimics IO/API function SHUT3
C
C  RETURN VALUE:  TRUE iff successful
C
C  PRECONDITIONS REQUIRED: None
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C
C***********************************************************************

      USE M3UTILIO, ONLY: OUTPUT_FILE,N_OUTPUT_FILES,OUTPUT_FILES,SETUP_OUTPUT_FILE,
     &                    NAMEVAL

      IMPLICIT NONE

C...........   INCLUDES:
      INCLUDE SUBST_FILES_ID  ! I/O definitions and declarations 
C...........   ARGUMENTS and their descriptions:

C........... Local variables:
        INTEGER, PARAMETER :: MAX_IO_STRLEN = 10000
        INTEGER            :: FSTATUS  ! io status or io unit number for FNAME
        INTEGER            :: NFILE
        INTEGER            :: NVAR

        CHARACTER( MAX_IO_STRLEN ) :: ACTUAL_NAME

        DO NFILE = 1,N_FILES
           CALL NAMEVAL( LGCL_NAME( NFILE ),ACTUAL_NAME )
           INQUIRE(FILE = ACTUAL_NAME, NUMBER=FSTATUS)
           NVAR = INDEX(ACTUAL_NAME,' -v') - 1
           IF( NVAR .GT. 0 )THEN
              ACTUAL_NAME = ACTUAL_NAME(1:NVAR)
           END IF
           IF ( FSTATUS .GT. 0 )THEN
              FLUSH( FSTATUS )
              CLOSE( FSTATUS )
           END IF
        END DO

        DO NFILE = 1,N_OUTPUT_FILES
           FSTATUS = OUTPUT_FILES(NFILE)%IO_UNIT
           IF( FSTATUS .GT. 0 )THEN
               FLUSH( FSTATUS )
               CLOSE( FSTATUS )
           END IF
        END DO

        SHUT3 = .TRUE.      

      RETURN

      END FUNCTION SHUT3
