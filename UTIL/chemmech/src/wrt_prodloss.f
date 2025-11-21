C***************************************************************************
C  Significant portions of Models-3/CMAQ software were developed by        *
C  Government employees and under a United States Government contract.     *
C  Portions of the software were also based on information from non-       *
C  Federal sources, including software developed by research institutions  *
C  through jointly funded cooperative agreements. These research institu-  *
C  tions have given the Government permission to use, prepare derivative   *
C  works, and distribute copies of their work to the public within the     *
C  Models-3/CMAQ software release and to permit others to do so. EPA       *
C  therefore grants similar permissions for use of Models-3/CMAQ software, *
C  but users are requested to provide copies of derivative works to the    *
C  Government without re-strictions as to use by others.  Users are        *
C  responsible for acquiring their own copies of commercial software       *
C  associated with the Models-3/CMAQ release and are also responsible      *
C  to those vendors for complying with any of the vendors' copyright and   *
C  license restrictions. In particular users must obtain a Runtime license *
C  for Orbix from IONA Technologies for each CPU used in Models-3/CMAQ     *
C  applications.                                                           *
C                                                                          *
C  Portions of I/O API, PAVE, and the model builder are Copyrighted        *
C  1993-1997 by MCNC--North Carolina Supercomputing Center and are         *
C  used with their permissions subject to the above restrictions.          *
C***************************************************************************

       SUBROUTINE WRT_PRODLOSS
C***********************************************************************
C
C  Function:  Write YDOT = dc/dt for each species to output file. YDOT is the
C             net rate of change in species concentrations resulting
C             from chemical production minus chemical loss.
C
C  Preconditions: None
C                                                                     
C  Key Subroutines/Functions Called: None
C
C***********************************************************************

      USE MECHANISM_DATA
      USE GET_ENV_VARS
      USE GET_MECHDEF_DATA
      USE BASIC_WRITE_ROUTINES

      IMPLICIT NONE

C..Includes: None



C..Parameters: None

C..External Functions: None

      INTEGER, EXTERNAL :: JUNIT   ! defines IO unit

C..Local Variables:
      INTEGER ISP              ! Loop index for species
      INTEGER ISP1, ISP2, ISP3 ! Pointers to species numbers
      INTEGER NP               ! Loop index for number of products
      INTEGER NR               ! Loop index for number of reactants
      INTEGER NRK              ! Reaction number
      INTEGER NRX              ! Loop index for number of reactions
      INTEGER IOUT
      INTEGER N_TERMS

      CHARACTER( 132 ), ALLOCATABLE :: STR_RXRAT ( : )      ! reaction rate strings
      CHARACTER( 132 ), ALLOCATABLE :: STR_RXN   ( : )      ! reaction strings
      INTEGER,          ALLOCATABLE :: TRUE_EFFECT( :, : )  ! reaction's net effect on species
      REAL( 8 ),        ALLOCATABLE :: TRUE_RCOEFF( :, : )
    
C***********************************************************************      

       ALLOCATE ( STR_RXRAT( NRXNS)  )
       ALLOCATE ( STR_RXN  ( NRXNS)  )

       IF( .NOT. ALLOCATED( TRUE_EFFECT ) )THEN
           ALLOCATE( TRUE_EFFECT( NUMB_MECH_SPCS, NRXNS) )
       END IF

       IF( .NOT. ALLOCATED( TRUE_RCOEFF ) )THEN
           ALLOCATE( TRUE_RCOEFF( NUMB_MECH_SPCS, NRXNS) )
       END IF

       TRUE_EFFECT    = 0     ! default setting, species not net reactant or product
       TRUE_RCOEFF = 0.0D0 ! initialize   
               
       OPEN(FILE = TRIM(OUTDIR)//'/evaluate_dydt.F', FORM='FORMATTED', STATUS='UNKNOWN', NEWUNIT=IOUT)
       WRITE(6,'(2A)')"evaluate_dydt.F is ",TRIM(OUTDIR)//'/evaluate_dydt.f'

       WRITE(IOUT, 95550)
! define integer parameters pointing to species concentrations in YIN array
       DO ISP = 1, NUMB_MECH_SPCS
          WRITE(IOUT, 94998)MECHANISM_SPC( ISP ),ISP
       END DO
       WRITE(IOUT, 95551)     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Initialize dc/dt
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      WRITE(IOUT, 94999)
94999 FORMAT(/'c  Initialize dc/dt to zero'
     &      /7X,'YDOT = 0.0D+0 ')
               
      DO NRK = 1, NRXNS
c..write reaction rates
             IF ( NREACT( NRK ) .EQ. 1 ) THEN
                ISP1 = IRR( NRK, 1 )
                WRITE(STR_RXRAT( NRK ),95000)NRK, MECHANISM_SPC( ISP1 ) ! , 'Reaction ' // RXLABEL( NRK )
95000           FORMAT('RKI( ', I4,' ) * YIN( ', A,' )  ')
                WRITE(STR_RXN( NRK ),96000) TRIM( RXLABEL( NRK ) ), TRIM( MECHANISM_SPC( ISP1 ) )
96000           FORMAT(A,': ',A,' ----> ')
             ELSE IF ( NREACT( NRK ) .EQ. 2 ) THEN
                ISP1 = IRR( NRK, 1 )
                ISP2 = IRR( NRK, 2 )
                WRITE(STR_RXRAT( NRK ),95001)NRK, MECHANISM_SPC( ISP1 ), MECHANISM_SPC( ISP2 ) ! , 'Reaction ' // RXLABEL( NRK )
95001           FORMAT('RKI( ', I4,' ) * YIN( ', A,' ) * YIN(  ', A, ' )  ' )
                WRITE(STR_RXN( NRK ),96001) TRIM( RXLABEL( NRK ) ), TRIM( MECHANISM_SPC( ISP1 ) ), 
     &          TRIM( MECHANISM_SPC( ISP2 ) )
96001           FORMAT(A,': ',A,' + ',A,' ----> ')
             ELSE IF ( NREACT( NRK ) .EQ. 3 ) THEN
                ISP1 = IRR( NRK, 1 )
                ISP2 = IRR( NRK, 2 )
                ISP3 = IRR( NRK, 3 )
                WRITE(STR_RXRAT( NRK ),95002)NRK, MECHANISM_SPC( ISP1 ), MECHANISM_SPC( ISP2 ), 
     &          MECHANISM_SPC( ISP3 )  ! , 'Reaction ' // RXLABEL( NRK )
95002           FORMAT('RKI(  ', I4,' ) * YIN( ', A,' ) * YIN(  ', A, ' ) * YIN(  ', A, ' )  ' )
                WRITE(STR_RXN( NRK ),96002) TRIM( RXLABEL( NRK ) ), TRIM( MECHANISM_SPC( ISP1 ) ), 
     &          TRIM( MECHANISM_SPC( ISP2 ) ),TRIM( MECHANISM_SPC( ISP3 ) )
96002           FORMAT(A,': ',A,' + ',A,' + ',A,' ----> ')
             ELSE IF ( NREACT( NRK ) .EQ. 0 ) THEN
                WRITE(STR_RXRAT( NRK ),95003)NRK ! , 'Reaction ' // RXLABEL( NRK )
95003           FORMAT('RKI(  ', I4,' ) ')
                WRITE(STR_RXN( NRK ),96003) TRIM( RXLABEL( NRK ) )
96003           FORMAT(A,': NO REACTANTs  ----> ')
             END IF
             WRITE( IOUT,85000)NRK,TRIM( STR_RXRAT( NRK ) )
85000        FORMAT(11X,'RXRAT(  ', I4,') = ',A)   


100   END DO

      DO NRK = 1, NRXNS
C...Set TRUE_EFFECT for reactants 
         DO NP = 1, NPRDCT( NRK )
            ISP1 = IRR( NRK, NP+3 )
            
            TRUE_RCOEFF( ISP1, NRK ) = TRUE_RCOEFF( ISP1, NRK )
     &                               + REAL(SC( NRK,NP ), 8)
            IF( TRUE_RCOEFF( ISP1, NRK ) .EQ. 0.0D0 )THEN
                TRUE_EFFECT( ISP1, NRK )  =  0
            ELSE IF( TRUE_RCOEFF( ISP1, NRK ) .LT. 0.0D0 )THEN ! net loss
              IF( TRUE_RCOEFF( ISP1, NRK ) .EQ. -1.0D0 )THEN ! only loss process
                  TRUE_EFFECT( ISP1, NRK ) = -1
              ELSE
                  TRUE_EFFECT( ISP1, NRK ) = -2
              END IF
            ELSE IF( TRUE_RCOEFF( ISP1, NRK ) .GT. 0.0D0 )THEN ! loss is not 100% 
              TRUE_EFFECT( ISP1, NRK ) = 3
            END IF
         END DO
C..Check whether reaction has a species as both reactant and product
         DO NR = 1, NREACT( NRK )
            ISP = IRR( NRK, NR )
            TRUE_RCOEFF( ISP, NRK ) = TRUE_RCOEFF( ISP, NRK ) - 1.0D0
            DO NP = 1, NPRDCT( NRK )
               ISP1 = IRR( NRK, NP+3 )
               IF( ISP .EQ. ISP1 )THEN
                   IF( TRUE_RCOEFF( ISP, NRK ) .EQ. 0.0D0 )THEN ! reaction has no net effect
                      TRUE_EFFECT( ISP, NRK ) = 0
                   ELSE IF( TRUE_RCOEFF( ISP, NRK ) .LT. 0.0D0 )THEN ! net loss
                       IF( TRUE_RCOEFF( ISP, NRK ) .EQ. -1.0D0 )THEN ! only loss process
                           TRUE_EFFECT( ISP, NRK ) = -1
                       ELSE
                           TRUE_EFFECT( ISP, NRK ) = -2
                       END IF
                   ELSE IF( TRUE_RCOEFF( ISP, NRK ) .GT. 0.0D0 )THEN ! loss is not 100% 
                       TRUE_EFFECT( ISP, NRK ) = 2
                   END IF
               END IF
            END DO
            IF( TRUE_RCOEFF( ISP, NRK ) .LT. 0.0D0 )THEN
                IF( TRUE_RCOEFF( ISP, NRK ) .EQ. -1.0D0 )THEN
                     TRUE_EFFECT( ISP, NRK ) = -1
                ELSE
                     TRUE_EFFECT( ISP, NRK ) = -2
                END IF
            END IF                  
         END DO                      

!        WRITE(6,'(5A,I2,A,ES12.4)')'For reactant ', TRIM(MECHANISM_SPC( ISP )),' : reaction ',
!    &   RXLABEL( NRK ),' TRUE_EFFECT = ',TRUE_EFFECT( ISP, NRK ),' TRUE_RCOEFF = ', TRUE_RCOEFF( ISP, NRK )
                       
      END DO  ! END LOOP for determining net efffect of each reaction on species
!  write expression to calculate dc/dt values
      DO ISP = 1, NUMB_MECH_SPCS
         N_TERMS = 0        
         IF( N_TERMS .EQ. 0 )THEN
           WRITE(IOUT,95008)MECHANISM_SPC( ISP )
           WRITE(IOUT,95004)MECHANISM_SPC( ISP )
         END IF
         DO NRK = 1, NRXNS
            IF( TRUE_EFFECT( ISP, NRK ) .EQ. 0 )CYCLE            
            SELECT CASE( TRUE_EFFECT( ISP, NRK ) )
               CASE( 2, 3 )
                  IF( TRUE_RCOEFF( ISP, NRK ) .NE. 1.0D0 )THEN
                    WRITE(IOUT, 95026)TRUE_RCOEFF( ISP, NRK ), NRK,
     &             'RXN_LABEL : ',TRIM( STR_RXN( NRK ) )
                  ELSE 
                    WRITE(IOUT, 95037)NRK,
     &             'RXN_LABEL : ',TRIM( STR_RXN( NRK ) )
                  END IF
                  N_TERMS = N_TERMS + 1
             END SELECT
         END DO
         DO NRK = 1, NRXNS
             IF( TRUE_EFFECT( ISP, NRK ) .EQ. 0 )CYCLE
             SELECT CASE( TRUE_EFFECT( ISP, NRK ) )
                CASE( -1 )
                   N_TERMS = N_TERMS + 1
                   WRITE(IOUT, 95025)NRK, 
     &            'RXN_LABEL : ',TRIM( STR_RXN( NRK ) )
                CASE( -2 )
                   N_TERMS = N_TERMS + 1
                   WRITE(IOUT, 95036)ABS(TRUE_RCOEFF( ISP, NRK )),
     &             NRK, 'RXN_LABEL : ',TRIM( STR_RXN( NRK ) )
             END SELECT
         END DO
      END DO
      
      WRITE(IOUT, 97911)

95008 FORMAT(/ 'C... dc/dt for ', A )
94998 FORMAT(7X,'INTEGER, PARAMETER :: ',A16, ' = ', I4)
95004 FORMAT(11X,'YDOT(  ', A16, ' ) = ')
95005 FORMAT(5X,'&',11X,'        - ', A16, 14X,' ! ',A,I4)
95006 FORMAT(5X,'&',11X,'        + ', 1PD10.4,' * ', A16,' ! ', A, I4)
95016 FORMAT(5X,'&',11X,'        - ', 1PD10.4,' * ', A16,' ! ', A, I4)
95017 FORMAT(5X,'&',17X,'        + ', A16, 14X,' ! ', A, I4)
95025 FORMAT(5X,'&',17X,'        - RXRAT(  ', I4,' ) ! ', 13X, A,A)
95026 FORMAT(5X,'&',17X,'        + ', 1PD10.4,' * RXRAT(  ', I4 ,' ) ! ', A, A)
95036 FORMAT(5X,'&',17X,'        - ', 1PD10.4,' * RXRAT(  ', I4 ,' ) ! ', A, A)
95037 FORMAT(5X,'&',17X,'        + RXRAT(  ', I4,' ) ! ', 13X, A, A)
!95025 FORMAT(5X,'&',17X,'        - RXRAT(  ', I4,' ) ! ', 13X, A,I4)
!95026 FORMAT(5X,'&',17X,'        + ', 1PD10.4,' * RXRAT(  ', I4 ,' ) ! ', A, I4)
!95036 FORMAT(5X,'&',17X,'        - ', 1PD10.4,' * RXRAT(  ', I4 ,' ) ! ', A, I4)
!95037 FORMAT(5X,'&',17X,'        + RXRAT(  ', I4,' ) ! ', 13X, A, I4)
95550 FORMAT(7X,'SUBROUTINE EVALUATE_DYDT( RKI, YIN, YDOT )'
     &  /'C***********************************************************************' 
     &  /'C'
     &  /'C  Function:  Compute YDOT = dc/dt for each species. YDOT is the'
     &  /'C             net rate of change in species concentrations resulting'
     &  /'C             from chemical production minus chemical loss.'
     &  /'C'
     &  /'C  Preconditions: None'
     &  /'C'
     &  /'C  Key Subroutines/Functions Called: None'
     &  /'C'
     &  /'C'
     &  /'C***********************************************************************' 
     &  /7X,'IMPLICIT NONE'/
     &  /'C..Includes:'
     &  /7X,'USE RXNS_DATA ' //
     &  /'C... arguments'
     &  /7X,'REAL( 8 ), INTENT(  IN )  ::   YIN(  : )       ! Species concs, ppm'
     &  /7X,'REAL( 8 ), INTENT(  IN )  ::   RKI(  : )       ! Reaction Rate Constant so YDOTs are in ppm/min'
     &  /7X,'REAL( 8 ), INTENT( OUT )  ::   YDOT( : )       ! Species rates of change, ppm/min'
     &  /'C... local'
     &  /7X,'INTEGER   ISP'/
     &  /7X,'REAL( 8 ) RXRAT( NRXNS )' /
     &  /'C... Parameters: ')
95551  FORMAT(////'c  Initialize reaction rates and dc/dt to zero'
     &  /7X,'RXRAT = 0.0D+0 '
     &  /7X,'YDOT  = 0.0D+0 '
     &  //7X,'IF ( NSPECIAL_RXN .GT. 0 ) CALL SPECIAL_RATES( YIN, RKI )',
     &    4X,'! calculate special rate coefficients '/ )
     

97911   FORMAT(// 7X
     &          / 7X, 'RETURN'
     &          / 7X, 'END SUBROUTINE EVALUATE_DYDT' )

      CLOSE(IOUT)
      END SUBROUTINE WRT_PRODLOSS

