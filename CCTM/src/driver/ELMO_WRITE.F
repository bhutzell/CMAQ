
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
     &                      ELMO_OUT, ELMO_TABLE, 
     &                      ET_WDEP, ET_DDEP, ET_ISAM_WDEP,
     &                      ET_DDM_WDEP, ET_ISAM_DDEP, ET_DDM_DDEP,
     &                      ET_CMPST_DEP, ET_ISAM_CMPST_DEP, ET_DDM_CMPST_DEP,
     &                      ET_DRVD_DDEP, ET_ISAM_DRVD_DDEP, ET_DDM_DRVD_DDEP,
     &                      ET_DRVD_WDEP, ET_ISAM_DRVD_WDEP, ET_DDM_DRVD_WDEP 
      USE UTILIO_DEFN
      use mio_module

      IMPLICIT NONE 

      INCLUDE SUBST_FILES_ID

      INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP(3)
      LOGICAL, INTENT( IN ) :: INIT_TIME
      logical, save         :: firstime = .true.
      logical, allocatable, save :: firstwrite(:)
      logical               :: subset_lay
      INTEGER MDATE, MTIME
      INTEGER IVAR, IV, IFL, J, ISTAT
      CHARACTER( 300 ) XMSG
      character( 20 ) :: timestamp
      CHARACTER( 16 ), SAVE :: PNAME = 'WRITE_ELMO'
      CHARACTER( 16 )       :: VNAME

      if (firstime) then
         firstime = .false.
         allocate( firstwrite(N_ELMO_FILES), stat=istat )
         if (istat .ne. 0) then
           xmsg = 'failure allocating firstwrite'
           call m3exit(pname, jdate, jtime, xmsg, 1)
         end if
         firstwrite = .TRUE.
      end if

      DO IFL = 1,N_ELMO_FILES
        
        if ( firstwrite( IFL ) ) then
           call mio_setfile( MET_CRO_3D )
           subset_lay = ( ELMO_FILE( IFL )%NLAYS .NE. mio_nlays )
           if ( subset_lay ) mio_nlays = ELMO_FILE( IFL )%NLAYS
           call mio_fcreate( ELMO_FILE( IFL )%FLOGICAL, mio_new_file )
           if ( subset_lay ) then
              if ( mype .eq. 0 ) then
                 call mio_set_global_attr( ELMO_FILE( IFL )%FLOGICAL, 'NLAYS', 
     &                                     ELMO_FILE( IFL )%NLAYS )
                 call mio_set_global_attr( ELMO_FILE( IFL )%FLOGICAL, 
     &                                     'VGLVLS', MET_CRO_3D,
     &                                     ELMO_FILE( IFL )%LAY_TOP, 
     &                                     ELMO_FILE( IFL )%LAY_BOT+1 )
              end if
           end if
           firstwrite( IFL ) = .FALSE.
        end if

        SELECT CASE ( ELMO_FILE( IFL )%TMODE )

        CASE ( 0 )
          ! *** Write data to an instantaneous output file.
#ifndef mpas
          MDATE = JDATE
          MTIME = JTIME
          call mio_time_format_conversion( MDATE, MTIME, TIMESTAMP )

          DO IVAR = 1, ELMO_FILE( IFL )%N_VARS
            VNAME = ELMO_TABLE( ELMO_FILE( IFL )%TABID( IVAR ) )%OUT_NAME 
            call mio_fwrite( ELMO_FILE( IFL )%FLOGICAL, 
     &          VNAME, PNAME,
     &          ELMO_OUT(IFL)%DATA(IVAR,:,:,:), TIMESTAMP ) 
          END DO
          WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                   'Timestep written to', trim(ELMO_FILE(IFL)%FLOGICAL),
     &                   'for date and time', MDATE, MTIME

#endif
        CASE ( 1 )
          ! *** Write data to a time-averaged output file.
          IF ( .NOT.INIT_TIME ) THEN
#ifndef mpas
            MDATE = JDATE
            MTIME = JTIME
            IF ( .NOT. END_TIME ) THEN   ! ending time timestamp
               CALL NEXTIME ( MDATE, MTIME, -TSTEP(1) )
            END IF
            call mio_time_format_conversion( MDATE, MTIME, TIMESTAMP )

            ! Finalize Values for Averaged Variables
!            DO IVAR = 1,ELMO_FILE( IFL )%N_VARS 
!                J = ELMO_FILE( IFL )%TABID( IVAR )
!                IV = ELMO_TABLE( J )%VAR_TYPE
!                IF ( IV .NE. ET_WDEP .AND. IV .NE. ET_DDEP .AND.
!     &               IV .NE. ET_ISAM_WDEP .AND. IV .NE. ET_ISAM_DDEP .AND.
!     &               IV .NE. ET_DDM_WDEP .AND. IV .NE. ET_DDM_DDEP .AND.
!     &               IV .NE. ET_CMPST_DEP .AND. IV .NE. ET_ISAM_CMPST_DEP .AND.
!     &               IV .NE. ET_DDM_CMPST_DEP .AND. IV .NE. ET_DRVD_DDEP .AND.
!     &               IV .NE. ET_ISAM_DRVD_DDEP .AND. IV .NE. ET_DDM_DRVD_DDEP .AND.
!     &               IV .NE. ET_DRVD_WDEP .AND. IV .NE. ET_ISAM_DRVD_WDEP .AND. 
!     &               IV .NE. ET_DDM_DRVD_WDEP ) THEN
!                ELMO_OUT(IFL)%DATA( :,:,:,IVAR ) = ELMO_OUT(IFL)%DATA( :,:,:,IVAR )
!     &                                        / 2.0 / (ELMO_NSTEP - 1.0)
!                END IF
!            END DO
           
            ! Write Data
            DO IVAR = 1, ELMO_FILE(IFL)%N_VARS
               vname = ELMO_TABLE( ELMO_FILE(IFL)%TABID(IVAR) )%OUT_NAME
               call mio_fwrite( ELMO_FILE( IFL )%FLOGICAL, 
     &                          VNAME, PNAME,
     &                          ELMO_OUT(IFL)%DATA(:,:,:,IVAR), TIMESTAMP ) 
            END DO
           
            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                       'Timestep written to', trim(ELMO_FILE(IFL)%FLOGICAL),
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

