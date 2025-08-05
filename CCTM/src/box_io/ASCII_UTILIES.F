      MODULE ASCII_UTILITIES
      
        IMPLICIT NONE
      CONTAINS

      SUBROUTINE ICDATA_ASCII ( ASCII_FNAM )

C***********************************************************************
C
C  FUNCTION:  Sets up defasult physical data for the box model
C
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED:
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, July, 1997
C 
C***********************************************************************
      USE UTILIO_DEFN
      USE CGRID_SPCS
      USE SCENE_DATA

      IMPLICIT NONE
      

C..INCLUDES:
      
C..ARGUMENTS: 
      CHARACTER*(*), INTENT( INOUT ) :: ASCII_FNAM       ! ASCII file name 

C..PARAMETERS:                  ! Total number of species in cgrid array
      CHARACTER(16), PARAMETER :: PNAME  = 'ICDATA_ASCII'         ! Program Name
 
C..SAVED LOCAL VARIABLES: None
      INTEGER, SAVE ::  N_SPC

C..SCRATCH LOCAL VARIABLES:
      CHARACTER(1)     CHR         ! Last character read
      CHARACTER(80)    XMSG        ! Log message
      CHARACTER(100)   INBUF       ! Input buffer -- 1 line
      CHARACTER(16)    WORD        ! Word read on line

      CHARACTER(16), ALLOCATABLE :: SPC ( : )  ! cgrid species names

      INTEGER EOL          ! Last non-blank character on line
      INTEGER FILDEV       ! Unit number for ASCII file
      INTEGER IND          ! Species index
      INTEGER LINNUM       ! Current line number read in ASCII file
      INTEGER LPOS         ! Read position on input line
      INTEGER N            ! Loop index

      LOGICAL, ALLOCATABLE       :: LFOUND ( : )  ! Flag for non-zero IC for a species

      REAL NUMBER          ! Number found
      LOGICAL :: LFIRST = .TRUE.               ! Flag for first time

C***********************************************************************

c..open the ascii file and initialize
!     FILDEV = JUNIT()
      OPEN ( NEWUNIT = FILDEV, FILE = ASCII_FNAM , FORM ='FORMATTED', STATUS='OLD',
     &       ERR = 1000 )

      LINNUM = 0
      LFIRST = .TRUE.

      N_SPC = N_CGRID_SPC
      ALLOCATE( SPC( N_SPC ) )
      ALLOCATE( LFOUND( N_SPC ) )
 
      SPC = CGRID_NAME
      LFOUND = .FALSE.

      DO
         READ( FILDEV , '(A)' , END = 120 ) INBUF
         LINNUM = LINNUM + 1
      END DO
120   REWIND(FILDEV)

      LINNUM = 0
c..read one line of ascii file
   20 READ( FILDEV , '(A)' , END = 100 ) INBUF
      LINNUM = LINNUM + 1

c..skip comment lines and blank lines
      IF ( INBUF (1:1) .EQ. '!' ) THEN 
         GO TO 20
      ELSE
         EOL = TRIMLEN ( INBUF )
      ENDIF

      IF ( EOL .EQ. 1 .AND. INBUF (1:1) .EQ. ' ') GO TO 20

c..find the first word on a line
      NUMBER = 0.0
      LPOS = 0
      CALL GET_CHR  ( FILDEV , INBUF , LPOS, EOL , CHR )
        
      CALL GET_WORD ( FILDEV , INBUF , LPOS, EOL , LINNUM, CHR, WORD )

      IF ( LPOS .GE. EOL ) GO TO 20

      CALL GET_CHR  ( FILDEV , INBUF , LPOS, EOL , CHR )

c..If the name matches a species, get the IC
      IND = INDEX1 ( WORD , N_SPC , SPC )
      IF ( IND .NE. 0 ) THEN
         CALL FETCH_REAL ( FILDEV, INBUF , LPOS, EOL, LINNUM, CHR, NUMBER )
         IF ( NUMBER .GT. 0.0 ) THEN

            INIT_CONC_VALUES ( IND ) = NUMBER

            print*,LINNUM,ind,SPC(ind),NUMBER,INIT_CONC_VALUES ( IND )
            LFOUND ( IND ) = .TRUE.
         ELSE
            WRITE ( LOGDEV , 93000) WORD , LINNUM
         ENDIF
      ELSE 
         GO TO 20        
      ENDIF

      GO TO 20

  100 CONTINUE

c..Report the ICs set
      LFIRST = .TRUE.

      DO  N = 1 , N_SPC
         IF ( .NOT. LFOUND ( N ) .OR. INIT_CONC_VALUES( N ) .LE. 0.0 ) THEN
            IF ( LFIRST ) WRITE ( LOGDEV , 92100 ) 
            WRITE ( LOGDEV , 92020 ) N, SPC ( N )
            LFIRST = .FALSE.
         ENDIF
      ENDDO
 
      LFIRST = .TRUE.

      DO  N = 1 , N_SPC
        IF ( LFOUND ( N ) .AND. INIT_CONC_VALUES( N ) .GT. 0.0 ) THEN
            IF ( LFIRST ) WRITE ( LOGDEV , 92200 ) 
            WRITE ( LOGDEV , 92020 )N,  SPC ( N ) ,  INIT_CONC_VALUES ( N)
            LFIRST = .FALSE.
         ENDIF
      ENDDO

      IF ( LFIRST ) THEN
         XMSG = 'NO ICs have been set ... stopping '
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT3 )
      ENDIF
 
      DEALLOCATE ( SPC,LFOUND )
      CLOSE( FILDEV )

      RETURN


 1000 CONTINUE

      XMSG = 'Error opening ASCII initial concentration file'
      CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )

92000 FORMAT(//1X , 'The following species in the IC file do not match',
     &            ' a model species:')
92020 FORMAT(5X , I3, 2X, A , 2X , 1PE12.4)

92100 FORMAT(//1X , 'ICs for the following species have been set to zero:')

92200 FORMAT(//1X , 'ICs for the following species are non-zero:')

93000 FORMAT(//1X, 'WARNING: Not Using IC for input species ', A ,' at Line no: ',I4,
     &      ' ... IC set to zero')

      END SUBROUTINE ICDATA_ASCII

      SUBROUTINE GET_ASCII_DATA ( ASCII_FNAM , SPC, CGRID )

C***********************************************************************
C
C  FUNCTION:  Sets up defasult physical data for the box model
C
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED:
C
C  REVISION HISTORY: Base on a prototype created by Jerry Gipson
C 
C***********************************************************************

      IMPLICIT NONE
      
C..INCLUDES:
      
C..ARGUMENTS: 

      CHARACTER*(*), INTENT( IN )    :: ASCII_FNAM   ! ASCII file name 
      CHARACTER(16), INTENT( IN )    :: SPC   ( : )  ! species or variable names
      REAL,          INTENT(   OUT ) :: CGRID ( : )  ! value array  
C..PARAMETERS:                  
      CHARACTER( 16 ), PARAMETER :: PNAME = 'GET_ASCII_DATA' 
C..EXTERNAL FUNCTIONS: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER(1)   :: CHR         ! Last character read
      CHARACTER(80)  :: XMSG        ! Log message
      CHARACTER(100) :: INBUF       ! Input buffer -- 1 line
      CHARACTER(16)  :: WORD        ! Word read on line

      INTEGER :: EOL          ! Last non-blank character on line
      INTEGER :: FILDEV       ! Unit number for ASCII file
      INTEGER :: IND          ! Species index
      INTEGER :: LINNUM       ! Current line number read in ASCII file
      INTEGER :: LPOS         ! Read position on input line
      INTEGER :: N            ! Loop index

      REAL    :: REAL_VALUE   ! Number read
      
C..SAVED LOCAL VARIABLES: None
      INTEGER,                     SAVE :: N_SPC
      INTEGER,                     SAVE :: LOGDEV          !  FORTRAN unit number for log file
      LOGICAL,                     SAVE :: LFIRST = .TRUE. ! Flag for first time
      LOGICAL,       ALLOCATABLE,  SAVE :: LFOUND ( : )    ! Flag for non-zero IC for a species


C***********************************************************************

c..open the ascii file and initialize
      LOGDEV = 6

!     FILDEV = 112
      OPEN ( NEWUNIT = FILDEV, FILE = ASCII_FNAM , FORM ='FORMATTED', STATUS='OLD',
     &       ERR = 1000 )

      LINNUM = 0

      N_SPC = SIZE( SPC )
  
      ALLOCATE( LFOUND( N_SPC ) )
      DO N = 1 , N_SPC
         CGRID  ( N ) = 0.0
         LFOUND ( N ) = .FALSE.
      ENDDO
      
      DO 
c..read one line of ascii file
20       READ( FILDEV , '(A)' , END = 100 ) INBUF

         LINNUM = LINNUM + 1
c..skip comment lines and blank lines
         IF ( INBUF (1:1) .EQ. '!' ) THEN 
            CYCLE
!            GO TO 20
         ELSE
            EOL = LEN_TRIM( INBUF )
         ENDIF
         print*,TRIM(INBUF)
         IF ( EOL .EQ. 1 .AND. INBUF (1:1) .EQ. ' ') CYCLE ! GO TO 20

c..find the first word on a line
         REAL_VALUE = 0.0
         LPOS = 0
         CALL GET_CHR  ( FILDEV , INBUF , LPOS, EOL , CHR )
        
         CALL GET_WORD ( FILDEV , INBUF , LPOS, EOL , LINNUM, CHR, WORD )

         IF ( LPOS .GE. EOL ) CYCLE ! GO TO 20

         CALL GET_CHR  ( FILDEV , INBUF , LPOS, EOL , CHR )

c..If the name matches a species, get the IC
         IND = 0
         LOOP_FIND: DO N = 1 , N_SPC
            IF( TRIM( WORD ) .EQ. TRIM( SPC( N ) ) )THEN
               IND = N
               EXIT LOOP_FIND
            END IF   
         END DO LOOP_FIND

         IF ( IND .NE. 0 ) THEN
            CALL FETCH_REAL ( FILDEV, INBUF , LPOS, EOL, LINNUM, CHR, REAL_VALUE )
            IF ( REAL_VALUE .GT. 0.0 ) THEN
               CGRID ( IND ) = REAL_VALUE
               LFOUND ( IND ) = .TRUE.
            ELSE
               WRITE ( LOGDEV , 93000) WORD , LINNUM
            ENDIF
         ELSE 
            CYCLE ! GO TO 20        
         ENDIF

      END DO 


c..Report the ICs set
100   LFIRST = .TRUE.

      DO  N = 1 , N_SPC
         IF ( .NOT. LFOUND ( N ) .OR. CGRID ( N) .LE. 0.0 ) THEN
            IF ( LFIRST ) WRITE ( LOGDEV , 92100 ) 
            WRITE ( LOGDEV , 92020 ) N, SPC ( N )
            LFIRST = .FALSE.
         ENDIF
      ENDDO
 
      LFIRST = .TRUE.

      DO  N = 1 , N_SPC
        IF ( LFOUND ( N ) .AND. CGRID ( N ) .GT. 0.0 ) THEN
            IF ( LFIRST ) WRITE ( LOGDEV , 92200 ) 
            WRITE ( LOGDEV , 92020 )N,  SPC ( N ) , CGRID ( N )
            LFIRST = .FALSE.
         ENDIF
      ENDDO

      IF ( LFIRST ) THEN
         XMSG = TRIM(PNAME) // ': ERROR, NO ICs have been set ... stopping '
         WRITE(LOGDEV,'(A)')TRIM(XMSG)
         STOP
      ENDIF
      CLOSE( FILDEV )
      RETURN


1000  XMSG = TRIM(PNAME) // ': Error opening ASCII initial concentration file'
      WRITE(LOGDEV,'(A)')TRIM(XMSG)
      STOP

92000 FORMAT(//1X , 'The following species in the IC file do not match',
     &            ' a model species:')
92020 FORMAT(5X , I3, 2X, A , 2X , 1PE12.4)

92100 FORMAT(//1X , 'ICs for the following species have been set to zero:')

92200 FORMAT(//1X , 'ICs for the following species are non-zero:')

93000 FORMAT(//1X, 'WARNING: Bad IC for species ', A ,' at Line no: ',I4,
     &      ' ... IC set to zero')

      END SUBROUTINE GET_ASCII_DATA
      SUBROUTINE GET_CHR ( FILDEV, INBUF , LPOS , EOL , CHR )
C***********************************************************************
C
C  FUNCTION: GET_CHR returns the next non-blank character in the input
C            buffer excluding those in comments
C
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Revised on based prototype created by Jerry Gipson,
C
C***********************************************************************
      IMPLICIT NONE
      
C..INCLUDES:
      
C..ARGUMENTS:
      INTEGER,       INTENT( IN    ) :: FILDEV      ! Unit number for ASCII file
      CHARACTER*(*), INTENT( INOUT ) :: INBUF       ! Input buffer -- 1 line
      INTEGER,       INTENT( INOUT ) :: LPOS        ! Read position on input line
      INTEGER,       INTENT( INOUT ) :: EOL         ! Last non-blank character on line
      CHARACTER (1), INTENT( INOUT ) :: CHR         ! Last character read
                                        
C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None 

C..SCRATCH LOCAL VARIABLES:

      INTEGER :: START     ! Starting position in input buffer
      INTEGER :: IC        ! loop count
         
C***********************************************************************
      START = LPOS
      DO IC = START, LEN( INBUF )  
         LPOS = LPOS + 1
          
         CHR = INBUF( LPOS : LPOS )

         IF(CHR.NE.' ') THEN
            RETURN
         ELSE IF ( CHR .EQ. '('  .OR.  CHR .EQ. '{' ) THEN
            CALL SKIP_COMMENT ( FILDEV , INBUF , LPOS, EOL , CHR)
         ENDIF

       END DO
      RETURN
      END SUBROUTINE GET_CHR
      SUBROUTINE GET_WORD ( FILDEV , INBUF , LPOS, EOL , LINNUM, CHR ,
     &                        WORD )
C***********************************************************************
C
C  FUNCTION: GET_WORD gets the next word from the input buffer starting
C            with the current character; the word can be any length,
C            but only the first sixteen characters are retained.  The
C            first character must be alphabetic.
C
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Revised on based prototype created by Jerry Gipson,
C
C***********************************************************************
      IMPLICIT NONE
      
C..INCLUDES: None
      
C..ARGUMENTS: 
      INTEGER,       INTENT( IN    ) :: FILDEV      ! Unit number for ASCII file
      CHARACTER*(*), INTENT( INOUT ) :: INBUF       ! Input buffer -- 1 line
      INTEGER,       INTENT( INOUT ) :: LPOS        ! Read position on input line
      INTEGER,       INTENT( INOUT ) :: EOL         ! Last non-blank character on line
      INTEGER,       INTENT( IN    ) :: LINNUM      ! Current line number read in ASCII file
      CHARACTER (1), INTENT( INOUT ) :: CHR         ! Last character read
      CHARACTER*(*), INTENT(   OUT ) :: WORD        ! Word read
                                        
C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: 
      INTEGER, PARAMETER :: WRDLEN = 16

C..SCRATCH LOCAL VARIABLES:
      CHARACTER(256) :: XMSG         !  Log message

      INTEGER :: START     ! Starting position in input buffer
      INTEGER :: LENGTH    ! Length of word
      INTEGER :: IC        ! loop counter   
C***********************************************************************

c..Setup and make sure the first character is alphabetic
      LENGTH = 0
      START = LPOS
      
      IF( (CHR.LT.'A'.OR.CHR.GT.'Z') .AND. 
     &    (CHR.LT.'a'.OR.CHR.GT.'z') ) THEN
         WRITE( 6, 94000) 
         WRITE( 6, 94020) LINNUM, INBUF 
         WRITE( 6, 94040 ) CHR 
         XMSG =  'GET_WORD: Above Fatal error detected'
         WRITE(6,'(A)') TRIM( XMSG )
         STOP
      ENDIF
c..Get the remaining characters; stop if a word spans a line
      DO IC = START, LEN( INBUF )
         LENGTH = LENGTH + 1
         LPOS = LPOS + 1
c..If at end of line, assume word ends
         IF(LPOS .GT. EOL) EXIT
c..Include all characters in word that are valid; stop reading on an 
c..invalid character
         CHR = INBUF( LPOS:LPOS)
         IF( CHR .EQ. ' ' )EXIT 
         IF(CHR.GE.'A'.AND.CHR.LE.'Z') CYCLE
         IF(CHR.GE.'a'.AND.CHR.LE.'z') CYCLE
         IF(CHR.GE.'0'.AND.CHR.LE.'9') CYCLE
         IF(CHR.EQ.':') CYCLE
         IF(CHR.EQ.'_') CYCLE
         
         IF(CHR.EQ.'('.OR.CHR.EQ.'{') THEN 
           LENGTH = LENGTH - 1
           CALL SKIP_COMMENT(FILDEV, INBUF , LPOS , EOL, CHR)
         ENDIF
      END DO      
c..Truncate the word and return
      LENGTH = MIN( LENGTH , WRDLEN)
      WORD = INBUF( START:START + LENGTH - 1)
      RETURN
      
94000 FORMAT('ERROR: The first character of a word must be alphabetic')
94020 FORMAT('   Line No. ',I4,': ', A )
94040 FORMAT('   First character: ', A )
94060 FORMAT('ERROR: A word cannot span two lines or end a line')
      END SUBROUTINE GET_WORD
      SUBROUTINE SKIP_COMMENT (FILDEV, INBUF , LPOS , EOL , CHR)
C***********************************************************************
C
C  FUNCTION: reads past all characters in comments; comments
C            are enclosed in () or {}
C
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: GETCHR
C                                    RDLINE
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, July, 1997
C
C***********************************************************************
      IMPLICIT NONE
      
C..INCLUDES: NONE
      
C..ARGUMENTS:
      INTEGER,       INTENT( IN    ) :: FILDEV      ! Unit number for ASCII file
      CHARACTER*(*), INTENT( INOUT ) :: INBUF       ! Input buffer -- 1 line
      INTEGER,       INTENT( INOUT ) :: LPOS        ! Read position on input line
      INTEGER,       INTENT( INOUT ) :: EOL         ! Last non-blank character on line
      CHARACTER (1), INTENT( INOUT ) :: CHR         ! Last character read
                                        
C..PARAMETERS:
      CHARACTER(16), PARAMETER :: PNAME = 'SKIP_COMMENT'       ! Program Name
C..SCRATCH LOCAL VARIABLES: 
      CHARACTER(256) :: XMSG      ! Log message
      INTEGER        :: START     ! Starting position in input buffer
      INTEGER        :: IC        ! loop counter   
         
C***********************************************************************

      DO
         LPOS = LPOS + 1
         IF(LPOS .GT. EOL) THEN
           READ( FILDEV , '(A)' , END = 90000 ) INBUF
           EOL = LEN_TRIM( INBUF )
           LPOS = 0
!         GO TO 20
         ELSE
           CHR = INBUF( LPOS:LPOS )
           IF(CHR .NE. ')' .AND. CHR .NE. '}' ) CYCLE
         ENDIF
      END DO   

      RETURN

90000 XMSG = TRIM( PNAME ) // ': ERROR: Comment not closed in below line.'
      WRITE(6,'(A)')TRIM( XMSG )
      WRITE(6,'(A)')TRIM( INBUF )
      STOP

      END SUBROUTINE SKIP_COMMENT
      SUBROUTINE FETCH_REAL ( FILDEV , INBUF , LPOS, EOL , LINNUM, CHR,
     &                        REAL_VALUE )
C***********************************************************************
C
C  FUNCTION: converts a number from character to floating point representation
C
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Revised on based prototype created by Jerry Gipson,
C
C***********************************************************************
      IMPLICIT NONE
      
C..INCLUDES:
      
C..ARGUMENTS: 
      INTEGER,       INTENT( IN    ) :: FILDEV      ! Unit number for ASCII file
      CHARACTER*(*), INTENT( INOUT ) :: INBUF       ! Input buffer -- 1 line
      INTEGER,       INTENT( INOUT ) :: LPOS        ! Read position on input line
      INTEGER,       INTENT( IN    ) :: EOL         ! Last non-blank character on line
      INTEGER,       INTENT( IN    ) :: LINNUM      ! Current line number read in ASCII file
      CHARACTER (1), INTENT( INOUT ) :: CHR         ! Last character read
      REAL,          INTENT(   OUT ) :: REAL_VALUE  ! Number read

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None 

C..SCRATCH LOCAL VARIABLES:
      CHARACTER(20) :: NUMSTRING  ! String holding number
      CHARACTER(80) :: XMSG       ! Log message

      INTEGER ICHR      ! ASCII code number of character
      INTEGER LENGTH    ! Length of numeric string
      INTEGER NOSIGNS   ! Number of pos or neg signs found
      INTEGER START     ! Starting position in input buffer

      LOGICAL LDECIMAL  ! Flag to indicate decimal point found
      LOGICAL LEXP      ! Flag to indicate exponent found
         
C***********************************************************************

      START   = LPOS
      LENGTH  = 0
      NOSIGNS = 0
      LDECIMAL = .FALSE.
      LEXP     = .FALSE.

c..Top of loop for getting characters in the number string
      DO 
         IF( LENGTH .NE. 0 ) THEN
            LPOS = LPOS + 1
            IF(LPOS.GT.EOL) THEN
               CHR = ' '
               EXIT 
            ELSE
               CHR = INBUF(LPOS:LPOS)
            ENDIF
         ENDIF
         ICHR = ICHAR( CHR )
         LENGTH = LENGTH + 1

c..Check for decimal point
         IF( CHR .EQ. '.' ) THEN
            IF( .NOT. LDECIMAL ) THEN
               LDECIMAL = .TRUE.
               CYCLE 
            ELSE
                  WRITE( 6, 94000 ) 
                  WRITE( 6, 94500 ) LINNUM, INBUF
                  WRITE( 6, 94520 ) INBUF( START:LPOS )
                  XMSG =  'FETCH_REAL: Above Fatal error detected'
                  WRITE(6,'(A)') TRIM( XMSG )
                  STOP
            ENDIF        
         ENDIF   
c..numeric value
         IF( ICHR .GE. 48 .AND. ICHR .LE. 57) CYCLE 
C..Convet real(8) format to exponent term         
         IF( CHR .EQ. 'D' .OR. CHR .EQ. 'd' ) CHR = 'E'
c..Exponent term
         IF( CHR .EQ. 'E' .OR. CHR .EQ. 'e' ) THEN
            IF( .NOT. LEXP) THEN
               LEXP = .TRUE.
               CYCLE 
            ELSE
                  WRITE( 6, 94020) 
                  WRITE( 6, 94500) LINNUM, INBUF
                  WRITE( 6, 94520 ) INBUF(START:LPOS)
                  XMSG =  'FETCH_REAL: Above Fatal error detected'
                  WRITE(6,'(A)') TRIM( XMSG )
                  STOP
            ENDIF
         ENDIF
      
         IF( CHR .EQ. '+' .OR. CHR .EQ. '-' ) THEN
            NOSIGNS = NOSIGNS + 1
            IF( NOSIGNS .LE. 2) THEN
               CYCLE 
            ELSE
                  WRITE( 6, 94040 ) 
                  WRITE( 6, 94500 ) LINNUM, INBUF
                  WRITE( 6, 94520 ) INBUF( START:LPOS )
                  XMSG =  'FETCH_REAL: Above Fatal error detected'
                  WRITE(6,'(A)') TRIM( XMSG )
                  STOP
            ENDIF
         ENDIF
      END DO

c..End of the numeric string; convert to real number
      NUMSTRING = INBUF( START:LPOS-1 )
      IF( ( .NOT.LEXP ) .AND. ( .NOT.LDECIMAL ) ) THEN
         NUMSTRING = NUMSTRING( 1:LENGTH ) // '.'
         LENGTH = LENGTH + 1
      ENDIF
      READ( NUMSTRING( 1:LENGTH ), '(E20.6)' , ERR = 999) REAL_VALUE
      RETURN

c..Error converting numeric string to real number
999   WRITE( 6, 94060) 
      WRITE( 6, 94500 ) LINNUM, INBUF
      WRITE( 6, 94520) INBUF( START:LPOS )
      XMSG =  'FETCH_REAL: Above Fatal error detected'
      WRITE(6,'(A)') TRIM( XMSG )
      STOP
      
      
C*********************** FORMAT STATEMENTS ***************************** 
     

94000 FORMAT('ERROR: Two decimal points found in a numeric field')
94020 FORMAT('ERROR: More than one E or e found in a numeric field')
94040 FORMAT('ERROR: Too many + or - signs found in a numeric field')
94060 FORMAT('ERROR: Numeric field contains an invalid character')
94500 FORMAT('       Line No. ',I4,': ',A)
94520 FORMAT('       Numeric field: ',A)
      END SUBROUTINE FETCH_REAL

      
        !==============================================================================================
      SUBROUTINE WRITE_OUTPUT(T, Y, NAMES, FNAME)
        !==============================================================================================

            ! This subroutine saves the output data from the integration solver such that the first
            ! column contains the time data, and the other columns correspond to the values of
            ! the unknowns at the corresponding times.
            !
            ! input:
            !   t, double precision, dimension(:)
            !       the times the solution was evaluated [n_rows]
            !   y, double precision, dimension(:,:)
            !       the approximate solution at the respetive times [n_rows, n_cols]
            !   n_rows, integer
            !       the number of rows in the approximate solution to write
            !   n_cols, integer
            !       the number of columns in the approximate solution to write
            !   fname, character(len=*)
            !       the file name

            IMPLICIT NONE

            REAL, INTENT(IN) :: Y( :,: )
            REAL, INTENT(IN) :: T(:)
            CHARACTER*(*), INTENT(IN) :: NAMES( : )
            CHARACTER*(*), INTENT(IN) :: FNAME

            INTEGER :: I, J ! loop counters
            INTEGER :: IOUT ! unit number of output file
            INTEGER :: IOS
            INTEGER :: N_ROWS
            INTEGER  :: N_COLS


            IOUT = 101

            N_ROWS = SIZE( T )
            N_COLS = SIZE( Y,DIM=2 )

            OPEN(UNIT=IOUT, FILE= (TRIM(FNAME) // '.csv'), STATUS='UNKNOWN', IOSTAT=IOS, ERR=1000)

            IF ( IOS .NE. 0 ) THEN
               WRITE(6,95102)
               STOP
            END IF

            ! write the data
            WRITE(IOUT,95100)(NAMES(J),J=1,N_COLS)
            DO I=1, N_ROWS
                WRITE(IOUT,95101)I,T(I),(Y(I,J),J=1,N_COLS)
            END DO

            CLOSE(IOUT)
            RETURN

1000        WRITE(7,95102)
            STOP

95100       FORMAT(" STEP ",",","      TIME      ",",    ",1001(A16,","))
95101       FORMAT(I6,",",1002(ES16.6,","))
95102       FORMAT('WRITE_OUTPUT: Error opening ASCII output concentration file')

        END SUBROUTINE WRITE_OUTPUT

      SUBROUTINE OUTPUT ( DATEOUT, TIMEOUT, ELPSTIM , COUT, NSTEPS )
C     %W%  %P%  %G%  %U%
C***********************************************************************
C
C  FUNCTION:  Controls form of concentration output file(s) 
C
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, July, 1997
C 
C***********************************************************************
      USE UTILIO_DEFN
      USE DRIVER_INPUTS
      USE CGRID_SPCS
      USE SCENE_DATA

      IMPLICIT NONE
      
C..INCLUDES:

     
C..ARGUMENTS: 
      INTEGER, INTENT ( IN ) ::   NSTEPS               ! number of time steps
      INTEGER, INTENT ( IN ) ::   DATEOUT ( : )   ! Output date
      INTEGER, INTENT ( IN ) ::   TIMEOUT ( : )   ! Output time

      REAL, INTENT ( IN ) ::    COUT ( :,: )       ! Output concentration array
      REAL, INTENT ( IN ) ::    ELPSTIM ( : )         ! Output elapsed time, min


C..PARAMETERS: 
      INTEGER, SAVE ::  N_SPC

c      INTEGER    XSTAT0         ! Normal, successful completion
c      PARAMETER (XSTAT0 = 0)

c      INTEGER    XSTAT1         ! File I/O error
c      PARAMETER (XSTAT1 = 1)

c      INTEGER    XSTAT2         ! Execution error
c      PARAMETER (XSTAT2 = 2)  

c      INTEGER    XSTAT3         ! Special  error
c      PARAMETER (XSTAT3 = 3)

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*80 ENVDESC        !  environment variable description
      CHARACTER*16 DEFNAM         !  Default NO flag
      CHARACTER*16 ENVFLAG        ! Value of environment variable

      INTEGER    STATUS         !  ENVSTR status

C***********************************************************************

      N_SPC = N_GC_SPC + 1 + N_AE_SPC + N_NR_SPC + N_TR_SPC  

       CALL UPCASE ( BXM_SAS )
       IF(BXM_SAS .EQ. 'YES' ) CALL OUTPUT_SAS( DATEOUT, TIMEOUT,
     &                                ELPSTIM , COUT, NSTEPS )


      RETURN
      END SUBROUTINE OUTPUT
      SUBROUTINE OUTPUT_SAS ( DATEOUT, TIMEOUT, ELPSTIM , COUT, NSTEPS )
C     %W%  %P%  %G%  %U%
C***********************************************************************
C
C  FUNCTION:  Produces an ASCII output file that can be easily read into
C             SAS
C
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, July, 1997
C 
C***********************************************************************

      USE UTILIO_DEFN
      USE CGRID_SPCS
      USE SCENE_DATA

      IMPLICIT NONE
      
C..INCLUDES:
     
C..ARGUMENTS: 
      INTEGER, INTENT ( IN ) ::   NSTEPS                ! number of time steps
      INTEGER, INTENT ( IN ) ::   DATEOUT ( : )   ! Output date
      INTEGER, INTENT ( IN ) ::   TIMEOUT ( : )   ! Output time

      REAL, INTENT ( IN ) ::    ELPSTIM ( : )         ! Output elapsed time, min
      REAL, INTENT ( IN ) ::    COUT ( :,: )       ! Output concentration array


C..PARAMETERS: 
      INTEGER, SAVE ::  N_SPC

c      INTEGER    XSTAT0         ! Normal, successful completion
c      PARAMETER (XSTAT0 = 0)

c      INTEGER    XSTAT1         ! File I/O error
c      PARAMETER (XSTAT1 = 1)

c      INTEGER    XSTAT2         ! Execution error
c      PARAMETER (XSTAT2 = 2)  

c      INTEGER    XSTAT3         ! Special  error
c      PARAMETER (XSTAT3 = 3)

c      INTEGER    NYCOLS
c      PARAMETER  ( NYCOLS = 7 )  ! max number of y columns in a graph

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER(80) XMSG      ! Log message
      CHARACTER(256)  FILNAM
      CHARACTER(16), ALLOCATABLE, SAVE ::  SPC ( : )  ! cgrid species names

      INTEGER    NS             !  Step loop index
      INTEGER    N              !  Species loop indices
      INTEGER    IND            !  Species index
      LOGICAL, SAVE :: FIRSTCALL = .TRUE.
      integer n_spc_out

C***********************************************************************
      DATA FILNAM  /'SAS_ASCII_OUT'/


      OPEN ( UNIT = 88 , FILE = FILNAM, ERR = 1000 )

      IF( FIRSTCALL )THEN
         
         N_SPC = N_GC_SPC + 1 + N_AE_SPC + N_NR_SPC + N_TR_SPC

         ALLOCATE( SPC( N_SPC ) )

         FIRSTCALL = .FALSE.

      ENDIF

      DO N = 1 , N_GC_SPC
         SPC ( N ) = GC_SPC ( N )
         IF( GC_SPC( N ) .EQ. 'TERP' ) SPC( N ) = 'TERPB'
      ENDDO

      SPC ( N_GC_SPC + 1 ) = 'AIR'

      DO N = 1 , N_AE_SPC
         IND = N_GC_SPC + 1 + N
         SPC ( IND ) = AE_SPC ( N )
      ENDDO

      DO N = 1 , N_NR_SPC
         IND = N_GC_SPC + 1 + N_AE_SPC + N
         SPC ( IND ) = NR_SPC ( N )
      ENDDO

      DO N = 1 , N_TR_SPC
         IND = N_GC_SPC + 1 + N_AE_SPC + N_NR_SPC + N
         SPC ( IND ) = TR_SPC ( N )
      ENDDO

      WRITE ( 88 , 92000 ) N_GC_SPC, NSTEPS

      DO NS = 1, N_GC_SPC

         WRITE( 88, '( A )' )  SPC ( NS )

         DO N = 1, NSTEPS + 1
            WRITE( 88, 92020 ) ELPSTIM ( N ) ,  COUT ( N , NS )
         ENDDO

      ENDDO

      CLOSE ( 88 )            

      RETURN

 1000 CONTINUE
      XMSG = 'Error Opening SAS ASCII output file...quitting'
      CALL M3MESG( XMSG )
      RETURN

92000 FORMAT ( 2I6 )
92020 FORMAT ( F12.4, 9X, E11.5 )

      END SUBROUTINE OUTPUT_SAS
      END MODULE ASCII_UTILITIES
