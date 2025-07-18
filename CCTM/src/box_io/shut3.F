
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

      IMPLICIT NONE

C...........   INCLUDES:
      INCLUDE SUBST_FILES_ID  ! I/O definitions and declarations 
C...........   ARGUMENTS and their descriptions:

C........... Local variables:
        INTEGER, PARAMETER :: MAX_IO_STRLEN = 10000
        INTEGER            :: FSTATUS  ! io status or io unit number for FNAME
        INTEGER            :: NFILE

        CHARACTER( MAX_IO_STRLEN ) :: ACTUALL_NAME

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

        DO NFILE = 1,N_FILES
           CALL NAMEVAL( LGCL_NAME( NFILE ),ACTUALL_NAME )
           INQUIRE(FILE = ACTUALL_NAME, NUMBER=FSTATUS)
           IF ( FSTATUS .GT. 0 )THEN
              FLUSH( FSTATUS )
              CLOSE( FSTATUS )
           END IF
        END DO
C.............................................................................
C   begin body of subroutine  OPEN3


        SHUT3 = .TRUE.      

      RETURN

      END FUNCTION SHUT3
