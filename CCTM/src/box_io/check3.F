
        LOGICAL   FUNCTION  CHECK3 ( FNAME , VNAME , JDATE , JTIME )
C***********************************************************************
C  FUNCTION:  Placeholder for IO/API function CHECK3. Always returns 
C             TRUE.
C
C  RETURN VALUE:  TRUE 
C
C  PRECONDITIONS REQUIRED: None
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C***********************************************************************
        IMPLICIT NONE
C...........   INCLUDES:
        INCLUDE 'PARMS3.EXT'        !  I/O API PARAMETERs
        INCLUDE 'FDESC3.EXT'        !  I/O API file headers
        INCLUDE SUBST_FILES_ID      ! I/O definitions and declarations 
C...........   ARGUMENTS and their descriptions:
        CHARACTER(LEN=*), INTENT( IN ) :: FNAME   !  logical name of file to be "opened"
        CHARACTER(LEN=*), INTENT( IN ) :: VNAME   !  Variable to check
        INTEGER,          INTENT( IN ) :: JDATE       !  Model date
        INTEGER,          INTENT( IN ) :: JTIME       !  Model time

        CHECK3 = .TRUE.      
        RETURN

      END FUNCTION CHECK3
