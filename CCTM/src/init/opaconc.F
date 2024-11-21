
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

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPACONC ( JDATE, JTIME, TSTEP )

! Revision History:
!   D. Wong - July 15: initial version
!   D. Wong 19 May 2016 - renamed ACONC_END_TIME to AVG_FILE_ENDTIME
!   F. Sidi 09 Sep 2019 - Replaced L_ACONC_WVEL(depreciated) with W_VEL
!-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE AVG_CONC              ! integral average CONC
      USE UTILIO_DEFN
      USE MIO_ASCII
#ifndef mpas
#ifdef parallel    
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif

      IMPLICIT NONE

! Include Files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

      INTEGER, INTENT (IN ) :: JDATE        ! current model date, coded YYYYDDD
      INTEGER, INTENT (IN ) :: JTIME        ! current model time, coded HHMMSS
      INTEGER, INTENT (IN ) :: TSTEP        ! output timestep (HHMMSS)

! Local variables:

      INTEGER      MDATE        ! modified model date, coded YYYYDDD
      INTEGER      MTIME        ! modified model time, coded HHMMSS

      CHARACTER( 16 ) :: PNAME = 'OPACONC'
      CHARACTER( 80 ) :: VARDESC = ' '
      CHARACTER( 96 ) :: XMSG = ' '

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL OK

      INTEGER      L, K, KD, VAR, SPC ! loop counters
      INTEGER      STATUS

      INTEGER TSTEP_RF, NTHIK_RF, NCOLS_RF, NROWS_RF, GDTYP_RF
      REAL( 8 ) :: P_ALP_RF, P_BET_RF, P_GAM_RF
      REAL( 8 ) :: XCENT_RF, YCENT_RF
      REAL( 8 ) :: XORIG_RF, YORIG_RF
      REAL( 8 ) :: XCELL_RF, YCELL_RF
      INTEGER VGTYP_RF
      REAL VGTOP_RF
!-----------------------------------------------------------------------

! Change output date/time to starting date/time - e.g. timestamp 1995196:090000
! represents data computed from time 1995196:090000 to 1995196:100000

#ifndef mpas 
! Override default beginning time timestamp for ACONC?      
      IF ( END_TIME ) THEN   ! ending time timestamp
         MDATE = JDATE; MTIME = JTIME
         CALL NEXTIME ( MDATE, MTIME, TSTEP )
      ELSE                   ! beginning time timestamp
         MDATE = JDATE; MTIME = JTIME
      END IF
 
            VAR = 0
            NVARS3D = N_ACONC_VARS

            DO SPC = 1, N_A_GC_SPC
               VAR = VAR + 1
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = A_GC_SPC( SPC )
               UNITS3D( VAR ) = 'ppmV'
               VDESC3D( VAR ) = 'Average Molar Mixing Ratio of ' // VNAME3D( VAR )
            END DO

            DO SPC = 1, N_A_AE_SPC
               VAR = VAR + 1
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = A_AE_SPC( SPC )
               IF ( VNAME3D( VAR )(1:3) .EQ. 'NUM' ) THEN
                  UNITS3D( VAR ) = 'm-3'
               ELSE IF ( VNAME3D( VAR )(1:3) .EQ. 'SRF' ) THEN
                  UNITS3D( VAR ) = 'm2 m-3'
               ELSE
                  UNITS3D( VAR ) = 'ug m-3'
               END IF
               VDESC3D( VAR ) = 'Average Concentrations of ' // VNAME3D( VAR )
            END DO

            DO SPC = 1, N_A_NR_SPC
               VAR = VAR + 1
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = A_NR_SPC( SPC )
               UNITS3D( VAR ) = 'ppmV'
               VDESC3D( VAR ) = 'Average Molar Mixing Ratio of ' // VNAME3D( VAR )
            END DO

            DO SPC = 1, N_A_TR_SPC
               VAR = VAR + 1
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = A_TR_SPC( SPC )
               UNITS3D( VAR ) = 'ppmV'
               VDESC3D( VAR ) = 'Average Molar Mixing Ratio of ' // VNAME3D( VAR )
            END DO

            IF ( W_VEL ) THEN
               VAR = VAR + 1 
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = 'W_VEL'
               UNITS3D( VAR ) = 'm s-1'
               VDESC3D( VAR ) = 'Vertical Wind Velocity'
            END IF
             
            IF ( L_ACONC_RH ) THEN
               VAR = VAR + 1 
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = 'RH'
               UNITS3D( VAR ) = '1'
               VDESC3D( VAR ) = 'Fractional Relative Humidity'
            END IF
             
            IF ( L_ACONC_TA ) THEN
               VAR = VAR + 1 
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = 'TA'
               UNITS3D( VAR ) = 'K'
               VDESC3D( VAR ) = 'Air Temperature'
            END IF
          
            IF ( L_ACONC_PRES ) THEN
               VAR = VAR + 1 
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = 'PRES'
               UNITS3D( VAR ) = 'Pa'
               VDESC3D( VAR ) = 'Air Pressure'
            END IF

            DO SPC = 1, VAR
               WRITE( LOGDEV,'( 7X, "=> VNAME3D(", I3, " ): ", A )' )
     &                SPC, VNAME3D( SPC )
            END DO

            ! Store MIO Metadata
            NDIMS3D( 1:NVARS3D ) = 4
            L_TSTEP( 1:NVARS3D ) = .True.
            L_LAY  ( 1:NVARS3D ) = .True.
            L_COL  ( 1:NVARS3D ) = .True.
            L_ROW  ( 1:NVARS3D ) = .True.
            L_VEXT ( 1:NVARS3D ) = .False.
            
            CALL LOAD_MIO_FILE ( A_CONC_1, NLAYS3D, ACONC_BLEV, ACONC_ELEV,
     &           VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D), 
     &           VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D), 
     &           L_LAY(1:NVARS3D),   L_COL(1:NVARS3D), L_ROW(1:NVARS3D),
     &           L_VEXT(1:NVARS3D) )


#endif

      END SUBROUTINE OPACONC
