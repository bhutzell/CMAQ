        LOGICAL FUNCTION XTRACT_METGEODATA( VNAME,VNAME_VALUE )

           USE SCENE_DATA

           IMPLICIT NONE

           CHARACTER *(*), INTENT(  IN ) :: VNAME       ! VNAME name
           REAL,           INTENT( OUT ) :: VNAME_VALUE ! 1.0e-30 if not found

           REAL    :: TEST_VALUE
           INTEGER :: NVAR
           INTEGER :: LUFRAC_LEN

           VNAME_VALUE = 1.0E-30
           TEST_VALUE  = 1.0E-30
           LUFRAC_LEN  = MIN( LEN(VNAME),7 )
           XTRACT_METGEODATA = .FALSE.

! force convective rainfall to negative and prevent convective clouds in cloud process
            IF ( VNAME .EQ. 'RCA'
     &            .OR. VNAME .EQ. 'RC' ) THEN
               VNAME_VALUE  = -1.0
               XTRACT_METGEODATA = .TRUE.
               RETURN
            ENDIF
! Initialize all landuse catagories to zero
           IF ( VNAME(1:LUFRAC_LEN) .EQ. 'LUFRAC_' ) THEN
             VNAME_VALUE = 0.0
             XTRACT_METGEODATA = .TRUE.
           END IF
! Search for value for VNAME
           LOOP_FIND_VAR: DO NVAR = 1,NUMB_MET_DATA
              IF ( VNAME .EQ. MET_DATA_SPCS(NVAR) ) THEN
                  VNAME_VALUE = MET_DATA_VALUES(NVAR)
                  XTRACT_METGEODATA = .TRUE.
                  RETURN
              END IF
           END DO LOOP_FIND_VAR

           RETURN

        END FUNCTION XTRACT_METGEODATA
