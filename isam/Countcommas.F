
      SUBROUTINE COUNTCOMMAS( TXTLINE, NCOMMAS )  ! KRT 20130313

      IMPLICIT NONE

! Arguments
      CHARACTER( * ), INTENT(  IN ) :: TXTLINE
      INTEGER,        INTENT( OUT ) :: NCOMMAS

! Local
      INTEGER         JCHAR

! External
      INTEGER         LEN_TRIM

!--------------------------------------------------------

      NCOMMAS = 0

      DO JCHAR = 1, LEN_TRIM(TXTLINE)
        IF ( TXTLINE(JCHAR:JCHAR) .EQ. ',' )
     &     NCOMMAS = NCOMMAS + 1
      ENDDO

      END
