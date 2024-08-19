
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

!-------------------------------------------------------------------------
      SUBROUTINE WRITE_ELMO ( JDATE, JTIME, TSTEP, INIT_TIME )
! Revision history
!   20 Feb - B. Murphy: Created
!-------------------------------------------------------------------------

      USE RUNTIME_VARS
      USE ELMO_DATA, ONLY : N_ELMO_FILES, ELMO_FILE, ELMO_NSTEP,
     &                      ELMO_CONC, ELMO_TABLE, 
     &                      ET_WDEP, ET_DDEP, ET_ISAM_WDEP,
     &                      ET_DDM_WDEP, ET_ISAM_DDEP, ET_DDM_DDEP,
     &                      ET_CMPST_DEP, ET_ISAM_CMPST_DEP, ET_DDM_CMPST_DEP
      USE UTILIO_DEFN

      IMPLICIT NONE 

      INCLUDE SUBST_FILES_ID

      INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP(3)
      LOGICAL, INTENT( IN ) :: INIT_TIME
      INTEGER MDATE, MTIME
      INTEGER IVAR, IV, IFL, J
      CHARACTER( 300 ) XMSG

      CHARACTER( 16 ), SAVE :: PNAME = 'WRITE_ELMO'

C *** If IO Proceesor, then Write Data
      DO IFL = 1,N_ELMO_FILES
        SELECT CASE ( ELMO_FILE( IFL )%TMODE )

        CASE ( 0 )
          ! *** Write data to the scalar output file.
#ifndef mpas
          MDATE = JDATE
          MTIME = JTIME

          IF ( .NOT. WRITE3( ELMO_FILE( IFL )%FLOGICAL, 
     &       ALLVAR3, MDATE, MTIME,
     &       ELMO_CONC(IFL)%DATA(:,:,:,:) ) ) THEN
             XMSG = 'Could not write ' // 
     &              ELMO_FILE(IFL)%FLOGICAL // ' file'
             CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
          END IF

          WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                   'Timestep written to', ELMO_FILE(IFL)%FLOGICAL,
     &                   'for date and time', MDATE, MTIME

#endif
        CASE ( 1 )
          ! *** Write data to the average aerosol diagnostic file.
          IF ( .NOT.INIT_TIME ) THEN
#ifndef mpas
            MDATE = JDATE
            MTIME = JTIME
            IF ( .NOT. END_TIME ) THEN   ! ending time timestamp
               CALL NEXTIME ( MDATE, MTIME, -TSTEP(1) )
            END IF
           
            ! Finalize Values for Averaged Variables
            DO IVAR = 1,ELMO_FILE( IFL )%N_VARS 
                J = ELMO_FILE( IFL )%TABID( IVAR )
                IV = ELMO_TABLE( J )%VAR_TYPE
                IF ( IV .NE. ET_WDEP .AND. IV .NE. ET_DDEP .AND.
     &               IV .NE. ET_ISAM_WDEP .AND. IV .NE. ET_ISAM_DDEP .AND.
     &               IV .NE. ET_DDM_WDEP .AND. IV .NE. ET_DDM_DDEP .AND.
     &               IV .NE. ET_CMPST_DEP .AND. IV .NE. ET_ISAM_CMPST_DEP .AND.
     &               IV .NE. ET_DDM_CMPST_DEP   ) THEN
                ELMO_CONC(IFL)%DATA( :,:,:,IVAR ) = ELMO_CONC(IFL)%DATA( :,:,:,IVAR )
     &                                        / 2.0 / (ELMO_NSTEP - 1.0)
                END IF
            END DO
           
            ! Write Data
            IF ( .NOT. WRITE3( ELMO_FILE( IFL )%FLOGICAL, 
     &           ALLVAR3, MDATE, MTIME, 
     &           ELMO_CONC(IFL)%DATA(:,:,:,:) ) ) THEN
               XMSG = 'Could not write ' // 
     &                ELMO_FILE( IFL )%FLOGICAL // ' file'
               CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
           
            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                       'Timestep written to', CTM_AELMO_1,
     &                       'for date and time', MDATE, MTIME
           
           
#endif     
          END IF
        END SELECT
      END DO

      IF ( .NOT. INIT_TIME ) THEN
         ELMO_NSTEP = 0.
      END IF
      
      RETURN 

      END SUBROUTINE WRITE_ELMO

