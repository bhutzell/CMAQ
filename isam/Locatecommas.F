
      SUBROUTINE LOCATECOMMAS( TXTLINE, NCOMMAS, CMAPOS )  ! KRT 20130313

      IMPLICIT NONE

! Arguments
      CHARACTER( * ), INTENT(  IN ) :: TXTLINE
      INTEGER,         INTENT(  IN ) :: NCOMMAS
      INTEGER,         INTENT( OUT ) :: CMAPOS( NCOMMAS )

! Local
      INTEGER         JCHAR
      INTEGER         CNT

! External
      INTEGER         LEN_TRIM

!--------------------------------------------------------

      CNT = 0

      DO JCHAR = 1, LEN_TRIM(TXTLINE)
        IF ( TXTLINE(JCHAR:JCHAR) .EQ. ',' ) THEN
          CNT = CNT + 1
          CMAPOS( CNT ) = JCHAR
        ENDIF
      ENDDO

      END
