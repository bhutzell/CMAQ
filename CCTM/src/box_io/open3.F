
        LOGICAL   FUNCTION  OPEN3 ( FNAME , FSTATUS , PNAME)

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

      IMPLICIT NONE

C...........   INCLUDES:
      INCLUDE SUBST_FILES_ID  ! I/O definitions and declarations 
      INCLUDE 'PARMS3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME   !  logical name of file to be "opened"
        CHARACTER(LEN=*), INTENT( IN ) :: PNAME   !  Calling program name
        INTEGER,          INTENT( IN ) :: FSTATUS       !  Not used



C.............................................................................
C   begin body of subroutine  OPEN3


      IF ( FNAME .EQ. INIT_CONC_1 ) THEN
         OPEN3 = .TRUE.
         RETURN      
      ELSE IF ( FNAME .EQ. MET_CRO_2D ) THEN
         OPEN3 = .TRUE.
         RETURN      
      ELSE IF ( FNAME .EQ. MET_CRO_3D ) THEN
         OPEN3 = .TRUE.
         RETURN      
      ELSE IF ( FNAME .EQ. GRID_CRO_2D ) THEN
         OPEN3 = .TRUE.
         RETURN      
      ELSE IF ( FNAME .EQ. GRID_DOT_2D ) THEN
         OPEN3 = .TRUE.
         RETURN      
      ELSE IF ( FNAME .EQ. CTM_CONC_1 .AND. FSTATUS .NE. FSNEW3 ) THEN
         OPEN3 = .TRUE.
         RETURN      
      ELSE IF ( FNAME .EQ. OCEAN_1 ) THEN
         OPEN3 = .TRUE.
         RETURN      
      ELSE IF ( FNAME .EQ. LUFRAC_CRO ) THEN
         OPEN3 = .FALSE.
         RETURN      
      ELSE IF ( FNAME .EQ. A_CONC_1 .AND. FSTATUS .NE. FSNEW3 ) THEN
         OPEN3 = .FALSE.
         RETURN      
      ELSE
         OPEN3 = .TRUE.
      ENDIF
      

      RETURN

      END FUNCTION OPEN3
