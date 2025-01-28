
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE INIT_CCTM_MODULES ( CGRID, JDATE, JTIME, TSTEP, ASTEP )

      USE RUNTIME_VARS, ONLY : COLUMN_MODEL
      USE ELMO_INIT_DEFN, ONLY : ELMO_INIT
      USE ELMO_DATA, ONLY : L_ELMO, ELMO_NSTEP
      USE ELMO_PROC, ONLY : ELMO_DRIVER  
      USE ELMO_INIT_DEFN, ONLY : ELMO_INIT, OPEN_ELMO
      USE PA_DEFN, ONLY : BUDGET_DIAG
      USE BUDGET_DEFN, ONLY : BUDGET_INIT
      USE GRID_CONF, ONLY: IO_PE_INCLUSIVE

      IMPLICIT NONE

C Arguments:
      REAL, POINTER :: CGRID( :,:,:,: )

      INTEGER, INTENT(IN)    :: JDATE        ! current model date, coded YYYYDDD
      INTEGER, INTENT(IN)    :: JTIME        ! current model time, coded HHMMSS
      INTEGER, INTENT(IN)    :: TSTEP( 3 )   ! time step vector (HHMMSS)
                                             ! TSTEP(1) = local output step
                                             ! TSTEP(2) = sciproc sync. step (chem)
                                             ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                             !            step and wrf/cmaq call frequency

      INTEGER, INTENT(IN)    :: ASTEP( : )   ! layer advection time step
      

      INTERFACE
#ifndef mpas
         SUBROUTINE HADV ( CGRID, JDATE, JTIME, TSTEP, ASTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
            INTEGER, INTENT( IN )     :: ASTEP( : )
         END SUBROUTINE HADV
         SUBROUTINE ZADV ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE ZADV
         SUBROUTINE COUPLE ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE COUPLE
         SUBROUTINE DECOUPLE ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE DECOUPLE
         SUBROUTINE HDIFF ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE HDIFF
#endif
         SUBROUTINE VDIFF ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE VDIFF
         SUBROUTINE PHOT ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( : )
         END SUBROUTINE PHOT
         SUBROUTINE CLDPROC ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE CLDPROC
         SUBROUTINE CHEM ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE CHEM
         SUBROUTINE PVO3 ( CGRID, JDATE, JTIME )
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
         END SUBROUTINE PVO3
         SUBROUTINE AERO ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE AERO
         SUBROUTINE WR_CGRID( CGRID, JDATE, JTIME, OUTSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: OUTSTEP
         END SUBROUTINE WR_CGRID
      END INTERFACE

      CALL VDIFF ( CGRID, JDATE, JTIME, TSTEP )

      CALL PHOT ( CGRID, JDATE, JTIME, TSTEP )    
      CALL CLDPROC ( CGRID, JDATE, JTIME, TSTEP )
      !CALL CHEM ( CGRID, JDATE, JTIME, TSTEP )
      !CALL AERO ( CGRID, JDATE, JTIME, TSTEP )

      CALL WR_CGRID ( CGRID, JDATE, JTIME, TSTEP( 1 ) )

#ifdef isam
      CALL WR_AVG_SA ( JDATE, JTIME, TSTEP( 1 ) )
      CALL WR_SA_CGRID ( JDATE, JTIME, TSTEP( 1 ) )
#endif

#ifdef sens
      CALL WR_SENGRID ( JDATE, JTIME, TSTEP( 1 )  )
#endif

      ! Initialize ELMO Arrays and Maps
      CALL ELMO_INIT( TSTEP(1) )
      
      ! Compute ELMO Values at Beginning of Main Time Step
      IF ( L_ELMO ) THEN

        ! Open the aerosol parameters file (diameters and standard deviations).
        IF ( IO_PE_INCLUSIVE ) THEN
           CALL OPEN_ELMO ( JDATE, JTIME, TSTEP( 1 ) )
           ELMO_NSTEP = 0.0
        END IF

        ! Calculate all ELMO variables for both Instantaneous and
        ! Average parameters
        CALL ELMO_DRIVER( CGRID, JDATE, JTIME, TSTEP, INIT_TIME_IN=.TRUE.)
        
      END IF

      ! Initialize Budget Output File and Concentration Array
      IF ( BUDGET_DIAG ) CALL BUDGET_INIT( CGRID, JDATE, JTIME, TSTEP )

      END SUBROUTINE INIT_CCTM_MODULES


 
