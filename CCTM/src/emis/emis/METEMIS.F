
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

      MODULE  METEMIS

        !!-----------------------------------------------------------------------
        !!  DESCRIPTION:
        !!      This module initializes data structures for, reads (temperature-binned)
        !!      pseudo-layered gridded emissions from program MOVESTABLE and calculates
        !!      gridded speciated emissions
        !!
        !!  PRECONDITIONS REQUIRED:
        !!      setenv METEMIS_TBL    <path for pseudo-layered emissions>
        !!
        !!  REVISION  HISTORY:
        !!      5/2021: initial version by C.J.Coats & B.H. Baek
        !!      7/2022: initial version by B.H. Baek
        !!-----------------------------------------------------------------------

        USE DESID_VARS
        USE RUNTIME_VARS, ONLY: MYPE
        USE HGRD_DEFN             ! horizontal domain specifications
        USE ASX_DATA_MOD          ! all gridded input variables, as Met_Data%*(ncols,nrows)
        USE UTILIO_DEFN

        IMPLICIT NONE
        
        INCLUDE SUBST_FILES_ID  ! file name parameters

        PUBLIC METEMIS_INIT, GET_METEMIS

        INTEGER           , PUBLIC, PROTECTED, SAVE :: NMETSPC
        CHARACTER(NAMLEN3), PUBLIC, PROTECTED, SAVE :: MTNAME( MXVARS3 )
        CHARACTER(NAMLEN3), PUBLIC, PROTECTED, SAVE :: MTUNIT( MXVARS3 )
        CHARACTER(NAMLEN3), PUBLIC, PROTECTED, SAVE :: MTBASIS( MXVARS3 )
        REAL              , PUBLIC, PROTECTED, SAVE :: MET_WT( MXVARS3 )
        REAL,  ALLOCATABLE, PUBLIC, PROTECTED, SAVE :: MTEMPS( : )
        REAL,  ALLOCATABLE, PUBLIC, PROTECTED, SAVE :: VDEMIS_MV( :,:,: ) !! ( NMETSPC,NCOLS,NROWS )
        REAL,  ALLOCATABLE, PRIVATE, PROTECTED, SAVE :: VDEMIS_MVDIAG( :,:,: ) !! ( NMETSPC,NCOLS,NROWS )


        PRIVATE     !!  everything else

        LOGICAL  INITFLAG

        !!........  PARAMETERs:

        REAL        , PARAMETER :: CTOK    = 273.15
        REAL        , PARAMETER :: CTOF    = 9.0 / 5.0

        !!........  SAVEd Local Variables:

        INTEGER          GXOFF, GYOFF        ! global origin offset from file
        INTEGER, SAVE :: STRTCOL, ENDCOL     !!  full-grid starting, ending col,
        INTEGER, SAVE :: STRTROW, ENDROW     !!  row subscripts

        INTEGER, SAVE :: NMTEMP, I

        INTEGER, SAVE :: SDATEMT
        INTEGER, SAVE :: STIMEMT
        INTEGER, SAVE :: TSTEPMT

        CHARACTER*16, SAVE :: TBLNAME = 'METEMIS_TBL'

        REAL, ALLOCATABLE, SAVE :: ETABLE( :,:,:,: )  !! ( NCOLS,NROWS,NMTEMP,NMETSPC )
        REAL, ALLOCATABLE, SAVE :: TEMPVAL( :,: )     !! ( NCOLS,NROWS ) for temperature
        REAL, ALLOCATABLE, SAVE :: QVMOL( :,: )       !! ( NCOLS,NROWS ) for specific humidity 

        INTEGER,SAVE  :: NTICS = 0 ! num. of substeps within an output tstep
        INTEGER,SAVE  :: MTICS = 0 ! temporary sub for NTICS before it is set to 0
        INTEGER,SAVE  :: LDATE     ! test date to update emissions diag avg
        INTEGER,SAVE  :: LTIME     ! test time to update emissions diag avg
        INTEGER       :: FTIME     ! Start time to write to diagnostic files
        INTEGER,SAVE  :: MV_TSTEP
        INTEGER       :: MV_TSTEP_F  ! same time step info as MV_TSTEP but in HHMMSS format


      CONTAINS !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        LOGICAL FUNCTION METEMIS_INIT( JDATE, JTIME, TSTEP )

            USE RUNTIME_VARS, ONLY: MYPE

            INTEGER, INTENT(IN   ) :: JDATE, JTIME
            INTEGER, INTENT(IN   ) :: TSTEP

            CHARACTER*24, PARAMETER :: PNAME = 'METEMIS:METEMIS_INIT'

            INTEGER         ISTAT, ISPC, K, N
            CHARACTER*256   MESG

            !!........  Open MVMT and read header

            IF ( .NOT.OPEN3( TBLNAME, FSREAD3, PNAME ) ) THEN
                CALL M3WARN( PNAME, 0,0, 'Could not open "'//TRIM(TBLNAME)//'"')
                METEMIS_INIT = .FALSE.
                RETURN
            ELSE IF ( .NOT.DESC3( TBLNAME ) ) THEN
                CALL M3WARN( PNAME, 0,0, 'Could not DESC3( MTEMP )')
                METEMIS_INIT = .FALSE.
                RETURN
            ELSE IF ( .NOT.FILCHK3( TBLNAME,  GRDDED3,
     &                              GL_NCOLS, GL_NROWS, NLAYS3D, NTHIK3D ) ) THEN
                CALL M3WARN( PNAME, 0,0, 'Inconsistent dims for "'//TRIM(TBLNAME)//'"')
                METEMIS_INIT =.FALSE.
                RETURN
            ELSE IF ( .NOT.GRDCHK3( TBLNAME,
     &                              P_ALP_GD, P_BET_GD, P_GAM_GD, XCENT_GD, YCENT_GD,
     &                              XORIG_GD, YORIG_GD, XCELL_GD, YCELL_GD,
     &                              NLAYS3D, VGTYP3D, VGTOP3D, VGLVS3D ) ) THEN
                CALL M3WARN( PNAME, 0,0, 'Inconsistent coord/grid  for "'//TRIM(TBLNAME)//'"')
                METEMIS_INIT = .FALSE.
                RETURN
            ELSE
                SDATEMT = SDATE3D
                STIMEMT = STIME3D
                TSTEPMT = TSTEP3D
                NMTEMP  = NLAYS3D
                NMETSPC = NVARS3D
            END IF      !!  if not open3() ...else...

            CALL SUBHFILE( TBLNAME, GXOFF, GYOFF, STRTCOL, ENDCOL,
     &                     STRTROW, ENDROW )

         !!........  Allocate input-related arrays:
            ALLOCATE( DESID_EMVAR( IMETSRM )%ARRY ( NMETSPC ),
     &                DESID_EMVAR( IMETSRM )%UNITS( NMETSPC ),
     &                DESID_EMVAR( IMETSRM )%MW   ( NMETSPC ),
     &                DESID_EMVAR( IMETSRM )%USED ( NMETSPC ),
     &                DESID_EMVAR( IMETSRM )%CONV ( NMETSPC ),
     &                DESID_EMVAR( IMETSRM )%BASIS( NMETSPC ),
     &                DESID_EMVAR( IMETSRM )%LAREA( NMETSPC ),
     &                DESID_EMVAR( IMETSRM )%LAREAADJ( NMETSPC ),
     &                ETABLE( NCOLS,NROWS,NMTEMP,NMETSPC ),
     &                TEMPVAL( NCOLS,NROWS ), MTEMPS( NMTEMP ),
     &                QVMOL( NCOLS,NROWS), VDEMIS_MV( NMETSPC,NCOLS,NROWS ),
     &                VDEMIS_MVDIAG( NMETSPC,NCOLS,NROWS ), STAT = ISTAT )
            ETABLE = 0.0
            TEMPVAL= 0.0
            QVMOL= 0.0
            VDEMIS_MVDIAG = 0.0

            MTEMPS( 1:NMTEMP )  = VGLVS3D( 1:NMTEMP )
            MTNAME( 1:NMETSPC ) = VNAME3D( 1:NMETSPC )
            MTUNIT( 1:NMETSPC ) = UNITS3D( 1:NMETSPC )
            MTBASIS( 1:NMETSPC) = 'UNIT'

            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I9 )' ) 'Error allocating ETABLE:  STATUS=', ISTAT
                CALL M3WARN( PNAME, 0, 0, MESG )
                METEMIS_INIT = .FALSE.
                RETURN
            END IF

            DESID_EMVAR( IMETSRM )%LEN   = NMETSPC
            DESID_EMVAR( IMETSRM )%ARRY  = MTNAME( 1:NMETSPC )
            DESID_EMVAR( IMETSRM )%UNITS = MTUNIT( 1:NMETSPC )
            DESID_EMVAR( IMETSRM )%MW    = MET_WT( 1:NMETSPC )
            DESID_EMVAR( IMETSRM )%USED  = .FALSE.
            DESID_EMVAR( IMETSRM )%CONV  = 1.0
            DESID_EMVAR( IMETSRM )%BASIS = MTBASIS(1:NMETSPC )
            DESID_EMVAR( IMETSRM )%LAREA = .FALSE.
            DESID_EMVAR( IMETSRM )%LAREAADJ = .FALSE.
            
            MV_TSTEP = TIME2SEC( TSTEP )
            MV_TSTEP_F = TSTEP

            LDATE = STDATE; LTIME = STTIME

            IF ( .NOT.METEMDIAG ) THEN
                CONTINUE

            ELSE IF( IO_PE_INCLUSIVE ) THEN

                FTYPE3D = GRDDED3
                SDATE3D = JDATE
                STIME3D = JTIME
                TSTEP3D = TSTEP
                NCOLS3D = GL_NCOLS
                NROWS3D = GL_NROWS
                NLAYS3D = 1
                NTHIK3D = 1
                GDTYP3D = GDTYP_GD
                GDNAM3D = GRID_NAME  ! from HGRD_DEFN
                P_ALP3D = P_ALP_GD
                P_BET3D = P_BET_GD
                P_GAM3D = P_GAM_GD
                XORIG3D = XORIG_GD
                YORIG3D = YORIG_GD
                XCENT3D = XCENT_GD
                YCENT3D = YCENT_GD
                XCELL3D = XCELL_GD
                YCELL3D = YCELL_GD
                VGTYP3D = VGTYP_GD
                VGTOP3D = VGTOP_GD

                NVARS3D = NMETSPC

                !! borrow rest of FDESC from input-file...
                FDESC3D      = ' '
                FDESC3D( 1 ) = 'hourly met-modulated emissions (diagnostic outputs)'
                IF ( .NOT. OPEN3( CTM_METEMIS_DIAG, FSUNKN3, PNAME ) ) THEN
                    CALL M3WARN( PNAME, 0,0, 'Could not open '//CTM_METEMIS_DIAG )
                    METEMIS_INIT = .FALSE.
                    RETURN
                END IF

            END IF

            METEMIS_INIT = .TRUE.
            RETURN

        END FUNCTION METEMIS_INIT


        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        SUBROUTINE GET_METEMIS( JDATE, JTIME, TSTEP, L_DESID_DIAG )

            USE DESID_VARS
            USE RUNTIME_VARS, ONLY: MYPE
            USE HGRD_DEFN             ! horizontal domain specifications
            USE ASX_DATA_MOD          ! all gridded input variables, as Met_Data%*(ncols,nrows)
            USE UTILIO_DEFN
            USE CENTRALIZED_IO_MODULE

            IMPLICIT NONE

            INTEGER, INTENT(IN   ) :: JDATE, JTIME
            INTEGER, INTENT(IN   ) :: TSTEP( 3 )    ! time step vector (HHMMSS)\
            LOGICAL, INTENT(IN   ) :: L_DESID_DIAG  ! flag for determining whether or not 
                                                    !   DESID is in diagnostic mode

            CHARACTER*24, PARAMETER :: PNAME = 'METEMIS:GET_METEMIS'

            REAL            EMDIAG( NCOLS,NROWS )

            INTEGER         C, R, K, L, LL, V, IDX1, IDX2

            INTEGER,SAVE  ::PRVHR=-10000
            LOGICAL,SAVE  :: LASTTIC   ! true: last sync step this output step
            INTEGER,SAVE  :: TOT_TSTEP 
            INTEGER       :: ADJ_LDATE, ADJ_LTIME
            LOGICAL,SAVE  :: R_READY = .TRUE.

            CHARACTER( 120 )  :: XMSG = ' '
            LOGICAL,SAVE  :: FIRSTIME = .TRUE.

            REAL            TF2, PP, E1, E2
            CHARACTER*16    VNAME
            CHARACTER*512   MESG
        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

            IF ( FIRSTIME ) THEN
               TOT_TSTEP = 0
               FIRSTIME = .FALSE.
            END IF

            WRITE( MESG, '(A,I8)' ),
     &         '     Processing Met-modulated '
     &          //'emissions table for date:', JDATE
            CALL LOG_MESSAGE( LOGDEV, MESG )

            ! Read METEMIS_TBL Data if this is the first timestep past
            ! the most recent time point on the table.
            STIMEMT = INT( JTIME/10000 ) * 10000
            IF ( PRVHR .NE. STIMEMT ) THEN
                IF( .NOT.XTRACT3( TBLNAME, ALLVAR3, 1, NMTEMP,
     &                         STRTROW, ENDROW, STRTCOL, ENDCOL,
     &                         SDATEMT, STIMEMT, ETABLE ) ) THEN
                    MESG = 'Error(s) reading "'//TRIM(TBLNAME)//'"'
                    CALL M3EXIT( PNAME, SDATEMT, STIMEMT, MESG, 2 )
                END IF
                PRVHR = STIMEMT
            END IF
            
            !!.......   Retrieve Met variable (TEMP2) to compute MetEmis
            CALL interpolate_var ( 'TEMP2', JDATE, JTIME, TEMPVAL )
            CALL interpolate_var ( 'QV', JDATE, JTIME, QVMOL )

            !!........  Compute-process:  interpolate between pseudo-layers in table
            !!........  Do not extrapolate beyond temperature-range of table.

            DO R = 1, NROWS
            DO C = 1, NCOLS

                TF2 = ( TEMPVAL( C,R ) - CTOK ) * CTOF + 32.0 !!  K ~~> F conversion

                IF ( TF2 .LT. MTEMPS( 1 ) ) THEN     !!  error?  below bottom of table-temperatures

                    DO V = 1, NMETSPC
                        VDEMIS_MV( V,C,R ) =  ETABLE( C,R,1,V )
                    END DO

                ELSE IF ( TF2 .GT. MTEMPS( NMTEMP ) ) THEN     !!  error?  above top of table-temperatures

                    DO V = 1, NMETSPC
                        VDEMIS_MV( V,C,R ) = ETABLE( C,R,NMTEMP,V )
                    END DO
                ELSE

                    DO LL = NMTEMP, 1, -1
                       IF( TF2 < MTEMPS( LL ) ) CYCLE
                       IF( TF2 == MTEMPS( LL ) ) THEN
                           IDX1 = LL
                           IDX2 = LL
                       ELSE
                           IDX1 = LL
                           IDX2 = LL + 1
                       END IF
                       EXIT
                    END DO

                    PP = 1.0
                    IF( IDX1 .NE. IDX2 ) THEN
                        PP = ( TF2 - MTEMPS( IDX1 ) ) / ( MTEMPS( IDX2 ) - MTEMPS( IDX1 ) )
                    END IF

                    DO V = 1, NMETSPC
                        E1 = ETABLE( C,R,IDX1,V )
                        E2 = ETABLE( C,R,IDX2,V )
                        VDEMIS_MV( V,C,R ) = E1 + PP * ( E2 - E1 )
                    END DO

                END IF

            END DO
            END DO

C ..........  Determine the time to read and/or write the hourly files
            IF ( .NOT. L_DESID_DIAG ) THEN
              NTICS = NTICS + 1
              TOT_TSTEP = TOT_TSTEP + TIME2SEC( TSTEP( 2 ) )
              LASTTIC = TOT_TSTEP .GE. MV_TSTEP
              IF ( LASTTIC ) THEN
                 ADJ_LDATE = LDATE
                 ADJ_LTIME = LTIME
                 MTICS = NTICS
                 NTICS = 0
                 TOT_TSTEP = 0
                 CALL NEXTIME( LDATE, LTIME, MV_TSTEP_F)
                 R_READY = .TRUE.
              END IF


C ..........  Output diagnostic emissions
              IF ( METEMDIAG ) THEN
                
                 VDEMIS_MVDIAG = VDEMIS_MVDIAG + VDEMIS_MV   ! array assignment
                 IF ( LASTTIC ) THEN   ! time to write out
                    VDEMIS_MVDIAG = VDEMIS_MVDIAG / REAL( MTICS, 4 )  ! array assignment
                    DO V = 1, NMETSPC
                      IF ( .NOT. WRITE3( CTM_METEMIS_DIAG, MTNAME(V), 
     &                                   ADJ_LDATE, ADJ_LTIME,VDEMIS_MVDIAG( V,:,:) ) ) THEN
                         XMSG = 'Could not write to ' // TRIM( CTM_METEMIS_DIAG )
                         CALL M3EXIT( PNAME, ADJ_LDATE, ADJ_LTIME, XMSG, XSTAT2 )
                      END IF
                    END DO
                    WRITE( LOGDEV,94040 )
     &                     'Timestep written to', TRIM( CTM_METEMIS_DIAG ),
     &                     'for date and time', ADJ_LDATE, ADJ_LTIME
                    VDEMIS_MVDIAG = 0.0   ! array assignment
                 END IF ! LASTTIC

              END IF      !!  if metemdiag
            END IF

            RETURN

94040 FORMAT( /5X, 3( A, :, 1X ), I8, ":", I6.6 )

        END SUBROUTINE GET_METEMIS


      END MODULE  METEMIS
