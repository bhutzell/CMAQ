#ifdef sens


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
      SUBROUTINE OPASENS ( JDATE, JTIME, TSTEP )

! Revision History:
!   D. Wong - July 15: initial version
!   D. Wong 19 May 2016 - renamed ACONC_END_TIME to AVG_FILE_ENDTIME
!   F. Sidi 09 Sep 2019 - Replaced L_ACONC_WVEL(depreciated) with W_VEL
C   S.Napelenok 26 Nov 2019 - create from opaconc.f for cmaq-ddm3d-v5.3.1
!-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE AVG_CONC              ! integral average CONC
      USE UTILIO_DEFN
      USE RUNTIME_VARS
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif

      USE DDM3D_DEFN, ONLY: NP, NPMAX, SEN_PAR, N_ASENS_VARS
      USE MIO_ASCII

      IMPLICIT NONE

! Include Files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

      INTEGER, INTENT (IN ) :: JDATE        ! current model date, coded YYYYDDD
      INTEGER, INTENT (IN ) :: JTIME        ! current model time, coded HHMMSS
      INTEGER, INTENT (IN ) :: TSTEP        ! output timestep (HHMMSS)

! Local variables:

      INTEGER      MDATE        ! modified model date, coded YYYYDDD
      INTEGER      MTIME        ! modified model time, coded HHMMSS

      CHARACTER( 16 ) :: PNAME = 'OPASENS'
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
! Define N_ASENS_VARS when a subset of species is requested and it is
! not defined elsewhere

      IF ( N_ASENS_VARS .EQ. 0 ) THEN ! this happens when 'ALL' keyword
                                      !is not specified in the run script
         IF ( W_VEL ) THEN
            N_ASENS_VARS = N_ACONC_VARS - 1
         ELSE
            N_ASENS_VARS = N_ACONC_VARS
         END IF
      END IF

         IF ( IO_PE_INCLUSIVE ) THEN

            NVARS3D = N_ASENS_VARS * NPMAX
            NLAYS3D = A_NLAYS

            VAR = 0

            DO SPC = 1, N_A_GC_SPC
               DO NP = 1, NPMAX
                  VAR = VAR + 1
                  VTYPE3D( VAR ) = M3REAL
                  VNAME3D( VAR ) = TRIM(A_GC_SPC( SPC )(1:12)) 
     &                             // '_' // SEN_PAR(NP)
                  UNITS3D( VAR ) = 'ppmV'
                  VDESC3D( VAR ) = 'Sensitivity of ' // A_GC_SPC( SPC )
     &                             // ' to ' // SEN_PAR(NP)
               END DO
            END DO

            DO SPC = 1, N_A_AE_SPC
               DO NP = 1, NPMAX
                  VAR = VAR + 1
                  VTYPE3D( VAR ) = M3REAL
                  VNAME3D( VAR ) = TRIM(A_AE_SPC( SPC )(1:12))
     &                             // '_' // SEN_PAR(NP)
                  IF ( VNAME3D( VAR )(1:3) .EQ. 'NUM' ) THEN
                     UNITS3D( VAR ) = 'm-3'
                  ELSE IF ( VNAME3D( VAR )(1:3) .EQ. 'SRF' ) THEN
                     UNITS3D( VAR ) = 'm2 m-3'
                  ELSE
                     UNITS3D( VAR ) = 'ug m-3'
                  END IF
                  VDESC3D( VAR ) = 'Sensitivity of ' // A_AE_SPC( SPC )
     &                            // ' to ' // SEN_PAR(NP)
               END DO
            END DO

            DO SPC = 1, N_A_NR_SPC
               DO NP = 1, NPMAX
                  VAR = VAR + 1
                  VTYPE3D( VAR ) = M3REAL
                  VNAME3D( VAR ) = TRIM(A_NR_SPC( SPC )(1:12))
     &                             // '_' // SEN_PAR(NP)
                  UNITS3D( VAR ) = 'ppmV'
                  VDESC3D( VAR ) = 'Sensitivity of ' // A_NR_SPC( SPC )
     &                            // ' to ' // SEN_PAR(NP)
               END DO
            END DO

            DO SPC = 1, N_A_TR_SPC
               DO NP = 1, NPMAX
                  VAR = VAR + 1
                  VTYPE3D( VAR ) = M3REAL
                  VNAME3D( VAR ) = TRIM(A_TR_SPC( SPC )(1:12))
     &                             // '_' // SEN_PAR(NP)
                  UNITS3D( VAR ) = 'ppmV'
                  VDESC3D( VAR ) = 'Sensitivity of ' // A_TR_SPC( SPC )
     &                            // ' to ' // SEN_PAR(NP)
               END DO
            END DO

            DO SPC = 1, VAR
               WRITE( LOGDEV,'( 7X, "=> VNAME3D(", I5, " ): ", A )' )
     &                SPC, VNAME3D( SPC )
            END DO

            ! Store MIO Metadata
            NDIMS3D( 1:NVARS3D ) = 4
            L_TSTEP( 1:NVARS3D ) = .True.
            L_LAY  ( 1:NVARS3D ) = .True.
            L_COL  ( 1:NVARS3D ) = .True.
            L_ROW  ( 1:NVARS3D ) = .True.
            L_VEXT ( 1:NVARS3D ) = .False.

            CALL LOAD_MIO_FILE ( 'A_SENS_1', NLAYS3D, 1, NLAYS3D,
     &           VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &           VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &           L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),
     &           L_VEXT(1:NVARS3D) )


         END IF   ! MYPE = 0


      END SUBROUTINE OPASENS


#endif
