
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
        USE MIO_MODULE
        USE MIO_ASCII

        IMPLICIT NONE
        
        INCLUDE SUBST_FILES_ID  ! file name parameters

        PUBLIC METEMIS_INIT, GET_METEMIS

        INTEGER           , PUBLIC, PROTECTED, SAVE :: NMETSPC
        CHARACTER( 16 )   , PUBLIC, PROTECTED, SAVE :: MTNAME( MXVARS3 )
        CHARACTER( 16 )   , PUBLIC, PROTECTED, SAVE :: MTUNIT( MXVARS3 )
        CHARACTER( 16 )   , PUBLIC, PROTECTED, SAVE :: MTBASIS( MXVARS3 )
        REAL              , PUBLIC, PROTECTED, SAVE :: MET_WT( MXVARS3 )
        REAL,  ALLOCATABLE, PUBLIC, PROTECTED, SAVE :: MTEMPS( : )
        REAL,  ALLOCATABLE, PUBLIC, PROTECTED, SAVE :: VDEMIS_MV( :,:,: ) !! ( NMETSPC,NCOLS,NROWS )
        REAL,  ALLOCATABLE, PRIVATE, PROTECTED, SAVE :: VDEMIS_MVDIAG( :,:,: ) !! ( NMETSPC,NCOLS,NROWS )
        LOGICAL, ALLOCATABLE,PRIVATE,PROTECTED, SAVE :: MT_GSL( : ), MT_DSL( : )


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
        REAL, ALLOCATABLE, SAVE :: QVMOL( :,:,: )       !! ( NCOLS,NROWS, NLAYS ) for specific humidity 

        INTEGER,SAVE  :: NTICS = 0 ! num. of substeps within an output tstep
        INTEGER,SAVE  :: MTICS = 0 ! temporary sub for NTICS before it is set to 0
        INTEGER,SAVE  :: LDATE     ! test date to update emissions diag avg
        INTEGER,SAVE  :: LTIME     ! test time to update emissions diag avg
        INTEGER       :: FDATE     ! Start date to Write to Diagnostic files
        INTEGER       :: FTIME     ! Start time to Write to Diagnostic files
        INTEGER,SAVE  :: MV_TSTEP
        INTEGER       :: MV_TSTEP_F  ! same time step info as MV_TSTEP but in HHMMSS format


      CONTAINS !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        LOGICAL FUNCTION METEMIS_INIT( JDATE, JTIME, TSTEP )

            USE RUNTIME_VARS, ONLY: MYPE

            INTEGER, INTENT(IN   ) :: JDATE, JTIME
            INTEGER, INTENT(IN   ) :: TSTEP

            CHARACTER*24, PARAMETER :: PNAME = 'METEMIS:METEMIS_INIT'

            INTEGER         ISTAT, ISPC, K, N, V, FLOC
            CHARACTER*256   MESG

            ! Open Temperature-Binned Mobile Emission File and read header
            CALL MIO_FOPEN( TBLNAME, MIO_READ_ONLY )
            FLOC = MIO_SEARCH( TBLNAME )          

            CALL MIO_GET_GLOBAL_ATTR (TBLNAME, 'SDATE', SDATEMT )
            CALL MIO_GET_GLOBAL_ATTR (TBLNAME, 'STIME', STIMEMT )
            CALL MIO_GET_GLOBAL_ATTR (TBLNAME, 'TSTEP', TSTEPMT )
            CALL MIO_GET_GLOBAL_ATTR (TBLNAME, 'NLAYS', NMTEMP )
            CALL MIO_GET_GLOBAL_ATTR (TBLNAME, 'NVARS', NMETSPC )


            call mio_get_global_attr( MET_CRO_3D, 'GDNAM', GDNAME_GD )
            call mio_get_global_attr( MET_CRO_3D, 'VGTYP', VGTYP_GD )
            call mio_get_global_attr( MET_CRO_3D, 'VGTOP', VGTOP_GD )
            VGTPUN_GD = 'Pa'
            VGLVUN_GD = 'none'


            CALL SUBHFILE( TBLNAME, GXOFF, GYOFF, STRTCOL, ENDCOL,
     &                     STRTROW, ENDROW )

            ! Allocate and initialize input-related arrays:
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
     &                VDEMIS_MV( NMETSPC,NCOLS,NROWS ),
     &                VDEMIS_MVDIAG( NMETSPC,NCOLS,NROWS ), STAT = ISTAT )
            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I9 )' ) 'Error allocating ETABLE:  STATUS=', ISTAT
                CALL M3WARN( PNAME, 0, 0, MESG )
                METEMIS_INIT = .FALSE.
                RETURN
            END IF
            
            ETABLE = 0.0
            TEMPVAL= 0.0
            VDEMIS_MVDIAG = 0.0
            
            CALL MIO_GET_GLOBAL_ATTR (TBLNAME, 'VGLVLS', MTEMPS )
            CALL MIO_GET_GLOBAL_ATTR (TBLNAME, 'VAR_NAME', MTNAME )
            CALL MIO_GET_GLOBAL_ATTR (TBLNAME, 'UNITS', MTUNIT )
            MTBASIS( 1:NMETSPC) = 'UNIT'

            ! Map Emission Variables Needed for Humidity Adjustment
            IF (HUM_NOX_YN) THEN
                ALLOCATE( QVMOL( NCOLS,NROWS,NLAYS), STAT = ISTAT ) 
                QVMOL= 0.0
                ALLOCATE( MT_GSL( NMETSPC ), MT_DSL( NMETSPC ), STAT=ISTAT )
                MT_GSL = .FALSE.
                MT_DSL = .FALSE.

                DO ISPC = 1,NMETSPC
                    IF ( MTNAME( ISPC ) .EQ. 'NO_GSL' .OR.
     &                   MTNAME( ISPC ) .EQ. 'NO2_GSL' .OR.
     &                   MTNAME( ISPC ) .EQ. 'HONO_GSL' ) THEN
                      MT_GSL( ISPC ) = .TRUE.
                    ELSE IF( MTNAME( ISPC ) .EQ. 'NO_DSL' .OR.
     &                       MTNAME( ISPC ) .EQ. 'NO2_DSL' .OR.
     &                       MTNAME( ISPC ) .EQ. 'HONO_DSL' ) THEN
                      MT_DSL( ISPC ) = .TRUE.
                    END IF
               END DO
            END IF

            ! Populate Data-Structures needed for DESID
            DESID_EMVAR( IMETSRM )%LEN   = NMETSPC
            DESID_EMVAR( IMETSRM )%ARRY  = MTNAME( 1:NMETSPC )
            DESID_EMVAR( IMETSRM )%UNITS = MTUNIT( 1:NMETSPC )
            DESID_EMVAR( IMETSRM )%MW    = MET_WT( 1:NMETSPC )
            DESID_EMVAR( IMETSRM )%USED  = .FALSE.
            DESID_EMVAR( IMETSRM )%CONV  = 1.0
            DESID_EMVAR( IMETSRM )%BASIS = MTBASIS(1:NMETSPC )
            DESID_EMVAR( IMETSRM )%LAREA = .FALSE.
            DESID_EMVAR( IMETSRM )%LAREAADJ = .FALSE.
            
            ! Initialize Time Parameters
            MV_TSTEP = TIME2SEC( TSTEP )
            MV_TSTEP_F = TSTEP
            LDATE = STDATE; LTIME = STTIME


            ! Open Diagnostic File if Requested
            IF ( .NOT.METEMDIAG ) THEN
                CONTINUE

            ELSE IF( IO_PE_INCLUSIVE ) THEN
#ifdef mpas
                call mio_fcreate( CTM_METEMIS_DIAG, mio_new_file )
#else
                
                NVARS3D = NMETSPC
                NLAYS3D = 1

                DO V = 1,NVARS3D
                  VTYPE3D( V ) = M3REAL
                  VNAME3D( V ) = MTNAME( V )
                  UNITS3D( V ) = MTUNIT( V )
                  VDESC3D( V ) = 'met-modulated mobile emission rate for '//TRIM( VNAME3D( V ) )
                END DO

                ! Store MIO Metadata
                NDIMS3D( 1:NVARS3D ) = 3
                L_TSTEP( 1:NVARS3D ) = .True.
                L_LAY  (1 :NVARS3D ) = .True. ! even if 2D
                L_COL  (1 :NVARS3D ) = .True. 
                L_ROW  (1 :NVARS3D ) = .True. 
                L_VEXT (1 :NVARS3D ) = .False.

                CALL LOAD_MIO_FILE ( 'CTM_MGEM_1', 0, 0, 0,
     &               VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &               VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &               L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),
     &               L_VEXT(1:NVARS3D) )

#endif
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
            INTEGER, INTENT(IN   ) :: TSTEP( 3 )    ! time step vector (HHMMSS)
            LOGICAL, INTENT(IN   ) :: L_DESID_DIAG  ! flag for determining whether or not 
                                                    !   DESID is in diagnostic mode

            CHARACTER*24, PARAMETER :: PNAME = 'METEMIS:GET_METEMIS'

            REAL            EMDIAG( NCOLS,NROWS )

            INTEGER         C, R, K, L, LL, V, IDX1, IDX2

            INTEGER,SAVE  ::PRVHR=-10000
            LOGICAL,SAVE  :: LASTTIC   ! true: last sync step this output step
            INTEGER,SAVE  :: TOT_TSTEP 
            LOGICAL,SAVE  :: R_READY = .TRUE.

            CHARACTER( 120 )  :: XMSG = ' '
            LOGICAL,SAVE  :: FIRSTIME = .TRUE.

            REAL            TF2, QV, QVM, ADJ, PP, E1, E2, A, B
            CHARACTER*16    VNAME
            CHARACTER*512   MESG
            CHARACTER( 20 ) :: TIMESTAMP
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
            IF (HUM_NOX_YN) 
     &          CALL interpolate_var ( 'QV', JDATE, JTIME, QVMOL )

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

C.................  NOx humidity corrections for mobile sources (MOVES3)
                IF (HUM_NOX_YN) THEN
                  QV  = QVMOL( C,R,1 ) * 1000.0  ! convert from kg water/kg dry air to g/kg

                  DO V = 1, NMETSPC
                      IF( MT_GSL( V ) ) THEN
                          ! Correct Gasoline Emission (NO, NO2, and HONO) for Humidity
                          A = MIN( QV, 17.71 )
                          B = MAX( 3.0, A )
                          ADJ = 1.0 - 0.0329 * ( B - 10.71 )   ! NOx humidity correction for Gasoline fuel
                          VDEMIS_MV( V,C,R ) = VDEMIS_MV( V,C,R ) * ADJ

                      ELSE IF( MT_DSL( V ) ) THEN
                          ! Correct Diesel Emission (NO, NO2, and HONO) for Humidity
                          QVM = QV * 0.001607524  ! convert from g of water/kg of dry air to moles of water/moles
                          A = MIN( QVM, 0.035 )
                          B = MAX( 0.002, A )
                          ADJ = 1.0 / ( 9.953 * B  + 0.832 )  ! NOx humidity correction for Diesel fuel
                          VDEMIS_MV( V,C,R ) = VDEMIS_MV( V,C,R ) * ADJ
                      END IF
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
                 MTICS = NTICS
                 NTICS = 0
                 TOT_TSTEP = 0
                 CALL NEXTIME( LDATE, LTIME, MV_TSTEP_F)
                 CALL MIO_TIME_FORMAT_CONVERSION( LDATE, LTIME, TIMESTAMP )
                 R_READY = .TRUE.
              END IF


C ..........  Output diagnostic emissions
              IF ( METEMDIAG ) THEN
                
                 VDEMIS_MVDIAG = VDEMIS_MVDIAG + VDEMIS_MV   ! array assignment
                 IF ( LASTTIC ) THEN   ! time to write out
                    VDEMIS_MVDIAG = VDEMIS_MVDIAG / REAL( MTICS, 4 )  ! array assignment
                    DO V = 1, NMETSPC
                        CALL MIO_FWRITE( CTM_METEMIS_DIAG, MTNAME(V), PNAME,
     &                                   VDEMIS_MVDIAG( V,:,:), TIMESTAMP ) 
                    END DO
                    WRITE( LOGDEV,94040 )
     &                     'Timestep written to', TRIM( CTM_METEMIS_DIAG ),
     &                     'for date and time', LDATE, LTIME
                    VDEMIS_MVDIAG = 0.0   ! array assignment
                 END IF ! LASTTIC

              END IF      !!  if metemdiag
            END IF

            RETURN

94040 FORMAT( /5X, 3( A, :, 1X ), I8, ":", I6.6 )

        END SUBROUTINE GET_METEMIS


      END MODULE  METEMIS
