
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

      PROGRAM CMAQ

!-----------------------------------------------------------------------
! Function:
!    CMAQ CTM main program
!
! Preconditions:
!
! Subroutines and functions called:
!    GET_ENV, CMAQ_DRIVER
!
! Revision History:
!    prototype 08/2018 by David Wong
!-----------------------------------------------------------------------

        USE GET_ENV_MODULE
        USE UTILIO_DEFN
 
        IMPLICIT NONE

        CHARACTER( 16 ), PARAMETER :: CTM_STDATE = 'CTM_STDATE'
        CHARACTER( 16 ), PARAMETER :: CTM_STTIME = 'CTM_STTIME'
        CHARACTER( 16 ), PARAMETER :: CTM_TSTEP  = 'CTM_TSTEP'
        CHARACTER( 16 ), PARAMETER :: CTM_RUNLEN = 'CTM_RUNLEN'

        INTEGER :: MODEL_TSTEP, STDATE, STTIME, RUNLEN, NSTEPS, ISTEP,
     $             JDATE, JTIME, TOTSECS, STEPSECS
        CHARACTER( 96 ) :: MSG = ' '

        INTERFACE
          SUBROUTINE CMAQ_DRIVER ( MODEL_STDATE, MODEL_STTIME, MODEL_TSTEP,
     $                             MODEL_JDATE, MODEL_JTIME, LAST_STEP,
     $                             COUPLE_TSTEP, NCOLS_IN, NLAYS_IN)
            INTEGER, INTENT( IN )  :: MODEL_STDATE, MODEL_STTIME, MODEL_TSTEP
            INTEGER, INTENT( OUT ) :: MODEL_JDATE, MODEL_JTIME
            LOGICAL, INTENT( IN )  :: LAST_STEP
            INTEGER, INTENT( IN ), OPTIONAL :: COUPLE_TSTEP
            INTEGER, INTENT( IN ), OPTIONAL :: NCOLS_IN, NLAYS_IN
          END SUBROUTINE CMAQ_DRIVER
        END INTERFACE

        CALL GET_ENV (MODEL_TSTEP, CTM_TSTEP, 10000)
        CALL GET_ENV (STDATE, CTM_STDATE, 2000001)
        CALL GET_ENV (STTIME, CTM_STTIME, 0)
        CALL GET_ENV (RUNLEN, CTM_RUNLEN, 240000)

        IF ( RUNLEN .LT. 1000000 ) THEN
           TOTSECS  = TIME2SEC( RUNLEN )
        ELSE
           RUNLEN = RUNLEN - 1000000
           TOTSECS  = TIME2SEC( RUNLEN )
           TOTSECS  = TOTSECS + 360000
        END IF
        STEPSECS = TIME2SEC( MODEL_TSTEP  )

        IF ( MOD( TOTSECS, STEPSECS ) .EQ. 0 ) THEN
           NSTEPS = TOTSECS / STEPSECS
        ELSE
           MSG = 'EXIT: Output time step ' // HHMMSS( MODEL_TSTEP ) //
     &           ' does not divide duration ' // HHMMSS( RUNLEN )
           CALL M3EXIT( 'CMAQ_MAIN', STDATE, STTIME, MSG, XSTAT1 )
        END IF

        DO ISTEP = 1, NSTEPS
           CALL CMAQ_DRIVER (STDATE, STTIME, MODEL_TSTEP, JDATE, JTIME, (ISTEP .EQ. NSTEPS))
        END DO

      END PROGRAM CMAQ
