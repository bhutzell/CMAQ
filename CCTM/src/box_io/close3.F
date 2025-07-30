
        LOGICAL   FUNCTION  CLOSE3 ( FNAME )

C***********************************************************************
C  FUNCTION:  Mimics IO/API function CLOSE3
C
C  RETURN VALUE:  TRUE if successful
C
C  PRECONDITIONS REQUIRED: None
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:
      INCLUDE SUBST_FILES_ID  ! I/O definitions and declarations 
C...........   ARGUMENTS and their descriptions:
            CHARACTER(LEN=*), INTENT(IN   ) :: FNAME   !  logical name of file to be closed

C........... Local variables:
        INTEGER, PARAMETER :: MAX_IO_STRLEN = 10000
        INTEGER            :: FSTATUS  ! io status or io unit number for FNAME
        INTEGER            :: NVAR

        CHARACTER( MAX_IO_STRLEN ) :: ACTUAL_NAME

        INTERFACE
           SUBROUTINE ENVSTR( VAR_NAME, VAR_DESC, VAR_DEFAULT, VAR_VALUE, STATUS )
             CHARACTER*(*), INTENT(  IN ) :: VAR_NAME
             CHARACTER*(*), INTENT(  IN ) :: VAR_DESC
             CHARACTER*(*), INTENT(  IN ) :: VAR_DEFAULT
             CHARACTER*(*), INTENT( OUT ) :: VAR_VALUE
             INTEGER,       INTENT( OUT ) :: STATUS
           END SUBROUTINE ENVSTR
           SUBROUTINE NAMEVAL( VAR_NAME, VAR_VALUE )
             CHARACTER*(*), INTENT(  IN ) :: VAR_NAME
             CHARACTER*(*), INTENT( OUT ) :: VAR_VALUE
           END SUBROUTINE NAMEVAL
        END INTERFACE


        CALL NAMEVAL( FNAME,ACTUAL_NAME )

        NVAR = INDEX(ACTUAL_NAME,' -v') - 1
        IF( NVAR .GT. 0 )THEN
           ACTUAL_NAME = ACTUAL_NAME(1:NVAR)
        END IF

        INQUIRE(FILE = ACTUAL_NAME, NUMBER=FSTATUS)
        IF ( FSTATUS .GT. 0 )CLOSE( FSTATUS )

C   begin body of subroutine  OPEN3
        CLOSE3 = .TRUE.      
        RETURN
      END FUNCTION  CLOSE3
