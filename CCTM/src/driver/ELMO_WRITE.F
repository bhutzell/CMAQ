
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

      IMPLICIT NONE 

      INCLUDE SUBST_FILES_ID

      INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP(3)
      LOGICAL, INTENT( IN ) :: INIT_TIME
      INTEGER MDATE, MTIME
      CHARACTER( 300 ) XMSG

      CHARACTER( 16 ), SAVE :: PNAME = 'WRITE_ELMO'

C *** If IO Proceesor, then Write Data
         MDATE = JDATE
         MTIME = JTIME

C *** Write data to the scalar output file.
         IF ( INST_ACTIVE ) THEN
#ifndef mpas
            IF ( .NOT. WRITE3( CTM_ELMO_1, 
     &           ALLVAR3, MDATE, MTIME,
     &           ELMO_INST(:,:,:,:) ) ) THEN
               XMSG = 'Could not write ' // CTM_ELMO_1 // ' file'
               CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                     'Timestep written to', CTM_ELMO_1,
     &                     'for date and time', MDATE, MTIME

#endif
         END IF
     
C *** Write data to the average aerosol diagnostic file.
         IF ( .NOT.INIT_TIME ) THEN
#ifndef mpas
           IF ( AVRG_ACTIVE ) THEN
            IF ( .NOT. END_TIME ) THEN   ! ending time timestamp
               CALL NEXTIME ( MDATE, MTIME, -TSTEP(1) )
            END IF
          
            IF ( .NOT. WRITE3( CTM_AELMO_1, 
     &             ALLVAR3, MDATE, MTIME, 
     &             ELMO_AVRG(:,:,:,:) / 2.0 / 
     &             (ELMO_NSTEP-1.0) ) ) THEN
               XMSG = 'Could not write ' // CTM_AELMO_1 // ' file'
               CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
          
            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                       'Timestep written to', CTM_AELMO_1,
     &                       'for date and time', MDATE, MTIME
          
          
           END IF
#endif
           ELMO_NSTEP = 0.
         END IF
      RETURN 

      END SUBROUTINE WRITE_ELMO

