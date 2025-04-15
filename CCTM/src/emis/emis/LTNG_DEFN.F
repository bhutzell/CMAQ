
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

!C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE LTNG_DEFN

!C----------------------------------------------------------------------
!C Function: production of NO from lightning

!C Revision History:
!C   1 Jul 2010 Jeff Young
!C   1 Jan 2011 Rob Pinder: added support for online calculation of 
!C               lightning NO from convective precip and LTNG data
!C  10 Mar 2011 Jeff Young: code revisions
!C  11 Apr 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
!C  11 May 2011 D.Wong: incorporated twoway model implementation
!C   6 May 2013 D.Wong: replaced species name RC with RCA in the twoway model
!C  10 Jun 2013 D.Wong: modified the code to work in the twoway model mode
!C  24 Sep 2013 D.Wong: modified the code to allow finer met data time step
!C                      rather than fixed with one hour data
!C  15 Aug 2016 D.Wong: Replaced MYPE with IO_PE_INCLUSIVE for parallel I/O
!C                      implementation
!C  16 Aug 2016 D.Kang: Updated the lightning NO calculation with LTNG hourly
!C                      data directly and using LTNG-RC log linear relationship,
!C                      completely changing the data input process   
!C   1 Feb 2017 D.Kang: Modify the parameter (linear-log linear regression) scheme 
!C                      to set log-linear when RC is greater than 
!C                      exp(log-intercept), and move the LTNOx
!C                      production rate to environment variable to be set in run script
!C  18 Apr 2017 D. Kang: Make routine compatible with the two-way coupled model
!C  17 Jul 2017 D. Wong: In subroutine GET_LTNG: 
!C                       * implemented a bug fix for determining LASTTIC base on 
!C                         accumulation of time step, TSTEP(2)
!C                       * called SUBHFILE only once to improve code efficiency
!C  13 Jun 2018 D. Kang: Changes to accommodate the twoway WRF-CMAQ coupled model 
!C                       with/without data passing (lightning assimilation and NO emissions)
!C   5 Feb 2019 D. Kang: Changed the Lightning flash variable name (LTNGstrk) and make it 
!C                       consistent with the name (LNT) used in lightning assimilation;
!C                       In the meantime, it still can read the files with old name by 
!C                       defining a LT_NAME variable. A bug fix related to read RC variable
!C                       from met input file is also implemented.
!C  01 Feb 2019 D. Wong: Implemented centralized I/O approach, removed all MY_N clauses
!C  12 Mar 2019 D. Wong: Implemented centralized I/O approach to the twoway portion of the code
!C                       and fixed a bug to handle the output time step is less than 1 hour
!C                       scenario properly
!C  3 Sep 2019 D. Kang: Added to scale factors for the vertical distribution and to make sure 
!C                      the profile is similar to those reported in literature and fixed a bug 
!C                      that was introduced two years ago for generating diagnotic 3D lightning emissions.
!C 4 Feb 2020 D. Kang: Remove the SATLAM function call that is redundant and clean up the code to comply with 
!C                      the implementation of CIO.Though there is no effect when run the model over the continental
!C                      US (Lambert projection), when run over the hemisphere and turning lightning NOx on,
!C                       the run would crash due to uncompatible projection (polar)
!C 6 Jan 2020 D. Kang: Correct the time steps in the lightning NOx !diagnostic files from 1:00 - 0:00 to 0:00 to 23:00
!C  2024 D. Kang:  Clean up of variable names and implement
!                       GLM/WWLLN energy based LNOx producion
!C----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE DESID_VARS

      IMPLICIT NONE

      REAL,    ALLOCATABLE, SAVE :: VDEMIS_LT( :,:,: )   ! lightning emis

!C lightning emis species name
      CHARACTER( 16 ), PARAMETER :: LTSPC = 'NO'
      
      PUBLIC :: VDEMIS_LT, LTNG_INIT, GET_LTNG, LTSPC
      PRIVATE

!C lightning log linear regression parameters with RC 
      REAL,                 SAVE :: SQUAREKM
      REAL,                 SAVE :: SCL_FACTOR

      INTEGER,              SAVE :: LTLYRS     ! no. of emissions layers
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_LTDIAG( :,:,: ) ! lightning NO diagnostic
      REAL,    ALLOCATABLE, SAVE :: COLUMN_DIAG( :,:,:) ! column total NO

      CHARACTER( 16 ),      SAVE :: RC_NAME       ! RC name: old is RC and CCnew is RCA

      REAL,    ALLOCATABLE, SAVE :: LTNG_PRSFC     ( :,: ) ! surface pressure
      REAL,    ALLOCATABLE, SAVE :: LTNG_RC        ( :,: ) ! convective rainfall
!C allocate these if LPARAM
      REAL,    ALLOCATABLE, SAVE :: LTNG_DATA    ( :,:,: ) ! Hourly LTNG data
      REAL,    ALLOCATABLE, SAVE :: ICCG           ( :,: ) ! intercloud strikes per cloud to ground strike
!C Vertical coord values
      REAL,    ALLOCATABLE, SAVE :: VGLVSLT        ( : )
      REAL, SAVE      :: YNaBeta
      REAL, SAVE      :: BetaF2E  ! GLM mean daily energy/flash
      REAL, SAVE      :: GamaF2E  ! WWLLN mean daily energy/stroke
      REAL, SAVE      :: sGamaF2E  ! WWLLNs mean daily energy/stroke


!C    scenario time/date needed for diagnostic output
      INTEGER,              SAVE :: NTICS = 0 ! no. of substeps within an output tstep
      INTEGER,              SAVE :: MTICS = 0 ! temporary sub for NTICS before it set to 0 
      INTEGER,              SAVE :: LDATE     ! test date to update emissions diag avg
      INTEGER,              SAVE :: LTIME     ! test time to update emissions diag avg
      INTEGER                    :: FDATE     ! Start date to Write to Diagnostic files
      INTEGER                    :: FTIME     ! Start time to Write to Diagnostic files

      INTEGER,              SAVE :: LT_TSTEP
      INTEGER                    :: LT_TSTEP_F  ! same time step info as LT_TSTEP but in HHMMSS format
      INTEGER                    :: GflIdx, GegIdx, WflIdx, WegIdx, WsflIdx, WsegIdx ! lightning data variable index


      CONTAINS

!C======================================================================
!C Initialize lightning routines

         FUNCTION LTNG_INIT ( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )

         USE GRID_CONF ! horizontal & vertical domain specifications
         USE CGRID_SPCS         ! CGRID mechanism species
         USE UTILIO_DEFN
         USE CENTRALIZED_IO_MODULE, only : RCA_AVAIL, ICCG_SUM, ICCG_WIN,
     &                                     FILE_TSTEP, F_MET, F_LTNG, LT_NAME, 
     &                                     LT_VARS,FILE_XCELL,
     &                                     FILE_YCELL,LTNG_DATA4D
         USE MIO_ASCII

#ifdef twoway
         USE twoway_data_module
#endif

         IMPLICIT NONE

!C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters

!C Arguments:
         INTEGER :: JDATE, JTIME, TSTEP

         LOGICAL SUCCESS

!C External Functions:
         LOGICAL,     EXTERNAL :: CHKGRID

!C value to start log linear regression  
         CHARACTER( 16 )       :: PNAME = 'LTNG_INIT'
         CHARACTER( 80 )       :: VARDESC   ! env variable description
         CHARACTER( 120 )      :: XMSG = ' '
         REAL                  :: X, Y, VAL
         INTEGER               :: C, R

         LOGICAL LTNGPARAM           ! env var to use lightning NO parameters file
         INTEGER LSPCS               ! no. of lightning species
         INTEGER               :: I, J, K, L, V, STATUS
         INTEGER               :: CJDATE
         CHARACTER( 7 )        :: SJDATE
         REAL                       :: mEngPfl   ! mean engergy per flash
         REAL                       :: smEngPfl  ! mean energy per flash for the WWLLNs data: s indicats WWLLNs 
         REAL                       :: WLNsd     ! tThe standard deviation (sd) for the WWLLN data 
         REAL                       :: sWLNsd    ! the standard deviation (sd) for the WWLLNs data
         REAL                       :: zScore    ! the ratio of CC/CG lightning strikes for WWLLN data
         REAL                       :: szScore   ! the ratio of CC/CG lightning strikes for WWLLNs data 
         REAL,    Allocatable       :: gZWLNeng(:) ! greater than zero (gZ) WWLLN energy record (grid-cell-hourly)
         REAL,    Allocatable       :: gZWLNseng(:) ! gZ WWLLNs engergy records
         REAL,    Allocatable       :: gZWLNfl(:)   ! gZ WWLLN strike records
         REAL,    Allocatable       :: gZWLNsfl(:)  ! gZ WWLLNs strike records
         REAL,    Allocatable       :: EngPfl(:)    ! the buffer holding the WWLLN energy per flash with gZ records
         REAL,    Allocatable       :: sEngPfl(:)   !the buffer holding the WWLLNs energy per flash with gZ records
                                       
         INTEGER                    :: WLNgZlen     ! the length of nonzero records for the WWLLN/WWLLNs data

!-----------------------------------------------------------------------

         SUCCESS = .TRUE.

!C Lightning NO production
         IF ( .NOT. LTNG_NO ) RETURN

!C Populate Emissions Species Record
         DESID_EMVAR( ILTSRM )%LEN = 1
         ALLOCATE( DESID_EMVAR( ILTSRM )%ARRY ( 1 ) )
         ALLOCATE( DESID_EMVAR( ILTSRM )%UNITS( 1 ) )
         ALLOCATE( DESID_EMVAR( ILTSRM )%MW   ( 1 ) )
         ALLOCATE( DESID_EMVAR( ILTSRM )%USED ( 1 ) )
         ALLOCATE( DESID_EMVAR( ILTSRM )%CONV ( 1 ) )
         ALLOCATE( DESID_EMVAR( ILTSRM )%BASIS( 1 ) )
         ALLOCATE( DESID_EMVAR( ILTSRM )%LAREA( 1 ) )
         ALLOCATE( DESID_EMVAR( ILTSRM )%LAREAADJ( 1 ) )
         
         DESID_EMVAR( ILTSRM )%ARRY  = "NO"
         DESID_EMVAR( ILTSRM )%UNITS = 'MOLES/S'
         DESID_EMVAR( ILTSRM )%MW    = 30.0
         DESID_EMVAR( ILTSRM )%USED  = .FALSE.
         DESID_EMVAR( ILTSRM )%CONV  = 1.0
         DESID_EMVAR( ILTSRM )%BASIS = 'MOLE'
         DESID_EMVAR( ILTSRM )%LAREA = .FALSE.
         DESID_EMVAR( ILTSRM )%LAREAADJ = .FALSE.
 
!C Read in scenario time/date

         WRITE( SJDATE, '(I7)' ) JDATE
         READ( SJDATE, '(4X, I3)' ) CJDATE

#ifndef mpas
            XMSG = 'Using in-line lightning NO production'
            CALL M3MSG2( XMSG )

#ifdef twoway
           IF (wrf_lightning_assim) THEN
              LTNGON_DATA = .FALSE.
           END IF
#endif
           SQUAREKM = REAL( ( XCELL_GD * YCELL_GD * 1.0D-6 ), 4 )
           IF ( RCA_AVAIL ) THEN
               RC_NAME = 'RCA'
           ELSE
               RC_NAME = 'RC'
           END IF
           IF ( LTNGON_DATA ) THEN
                  XMSG = 'Using hourly LTNG data'
                  CALL M3MSG2( XMSG )
               IF(LNO_OPTION == 1) THEN
                 IF(LT_VARS == 1) THEN
                     WsflIdx = 1
                 ELSE
                     WsflIdx = INDEX1("WLNsfl", LT_VARS, LT_NAME)
                 ENDIF
               ELSE IF (LNO_OPTION == 2) THEN
                    GflIdx = INDEX1("GLMfl", LT_VARS, LT_NAME)
               ELSE IF (LNO_OPTION == 3) THEN
                    GegIdx = INDEX1("GLMeng", LT_VARS, LT_NAME)
                    GflIdx = INDEX1("GLMfl", LT_VARS, LT_NAME)
                    IF(sum(LTNG_DATA4D(:,:,1,:,GflIdx)) > 0) then
                       BetaF2E = sum(LTNG_DATA4D(:,:,1,:,GegIdx))/(sum(LTNG_DATA4D(:,:,1,:,GflIdx))*SQUAREKM)
                       BetaF2E = MOLSNCG/BetaF2E                 !moles N per flash energy of GLM
                    ELSE
                       BetaF2E = 0
                    ENDIF
               ELSE IF (LNO_OPTION >= 4) THEN
                    GflIdx = INDEX1("GLMfl", LT_VARS, LT_NAME)
                    GegIdx = INDEX1("GLMeng", LT_VARS, LT_NAME)
                    WflIdx = INDEX1("WLNfl", LT_VARS, LT_NAME)
                    WegIdx = INDEX1("WLNeng", LT_VARS, LT_NAME)
                    WsflIdx = INDEX1("WLNsfl", LT_VARS, LT_NAME)
                    WsegIdx = INDEX1("WLNseng", LT_VARS, LT_NAME)
                    WLNgZlen = COUNT(LTNG_DATA4D(:,:,1,:,WflIdx) > 0)  ! gZ: greater than zero
                    ALLOCATE(gZWLNeng(WLNgZlen),gZWLNseng(WLNgZlen), gZWLNfl(WLNgZlen))
                    ALLOCATE(gZWLNsfl(WLNgZlen), EngPfl(WLNgZlen), sEngPfl(WLNgZlen))
                    gZWLNfl = PACK(LTNG_DATA4D(:,:,1,:,WflIdx), LTNG_DATA4D(:,:,1,:,WflIdx) > 0)
                    gZWLNsfl = PACK(LTNG_DATA4D(:,:,1,:,WsflIdx), LTNG_DATA4D(:,:,1,:,WsflIdx) > 0)
                    gZWLNeng = PACK(LTNG_DATA4D(:,:,1,:,WegIdx), LTNG_DATA4D(:,:,1,:,WegIdx) > 0)
                    gZWLNseng = PACK(LTNG_DATA4D(:,:,1,:,WsegIdx), LTNG_DATA4D(:,:,1,:,WsegIdx) > 0)
                    EngPfl = gZWLNeng/(gZWLNfl*SQUAREKM)
                    sEngPfl = gZWLNseng/(gZWLNsfl*SQUAREKM)
                    mEngPfl = sum(EngPfl)/WLNgZlen
                    smEngPfl = sum(sEngPfl)/WLNgZlen
                    WLNsd   = SQRT(sum(EngPfl**2)/size(EngPfl) - mEngPfl**2)
                    sWLNsd   = SQRT(sum(sEngPfl**2)/size(sEngPfl) - smEngPfl**2)
                    zScore = mEngPfl + 3*WLNsd
                    szScore = smEngPfl + 3*sWLNsd
! filter out possible spurious WWLLN energy records that are outliers
! and recompute using the strokes and the mean energy per stroke
                  DO i = 1, size(LTNG_DATA4D,4) 
                    DO r = 1, NROWS
                      DO c = 1, NCOLS
                        IF((LTNG_DATA4D(c,r,1,i,WflIdx) > 0) .and.  
     &                     (LTNG_DATA4D(c,r,1,i,WegIdx)/(LTNG_DATA4D(c,r,1,i,WflIdx)*SQUAREKM) 
     &                    > zScore)) THEN
                          LTNG_DATA4D(c,r,1,i,WegIdx) = LTNG_DATA4D(c,r,1,i,WflIdx) * mEngPfl
                        ENDIF
                        IF((LTNG_DATA4D(c,r,1,i,WsflIdx) > 0) .and. 
     &                    (LTNG_DATA4D(c,r,1,i,WsegIdx)/(LTNG_DATA4D(c,r,1,i,WsflIdx)*SQUAREKM) >
     &                   szScore)) THEN
                          LTNG_DATA4D(c,r,1,i,WsegIdx) = LTNG_DATA4D(c,r,1,i,WsflIdx) * smEngPfl
                        ENDIF
                      ENDDO
                    ENDDO
                 ENDDO
                 IF(sum(LTNG_DATA4D(:,:,1,:,GflIdx)) > 0) then
                    BetaF2E = sum(LTNG_DATA4D(:,:,1,:,GegIdx))/(sum(LTNG_DATA4D(:,:,1,:,GflIdx))*SQUAREKM)
                    BetaF2E = MOLSNCG/BetaF2E                 !moles N per flash energy of GLM
                 ELSE 
                    BetaF2E = 0
                 ENDIF 
                 IF(sum(LTNG_DATA4D(:,:,1,:,WflIdx)) > 0) then
                   GamaF2E = sum(LTNG_DATA4D(:,:,1,:,WegIdx))/(sum(LTNG_DATA4D(:,:,1,:,WflIdx))*SQUAREKM)
                   GamaF2E = MOLSNCG/GamaF2E                 !moles N per flash energy of WWLLN
                 ELSE 
                   GamaF2E = 0
                 ENDIF 
                 IF(sum(LTNG_DATA4D(:,:,1,:,WsflIdx)) > 0) then
                  sGamaF2E = sum(LTNG_DATA4D(:,:,1,:,WsegIdx))/(sum(LTNG_DATA4D(:,:,1,:,WsflIdx))*SQUAREKM)
                  sGamaF2E = MOLSNCG/sGamaF2E                 !moles N per flash energy of WWLLN
                 ELSE 
                  sGamaF2E = 0
                 ENDIF 

               ELSE
                   XMSG = 'LNO_OPTION has to be in the range from 1 to 5'
                   CALL M3MSG2( XMSG )
               END IF

               LT_TSTEP = TIME2SEC ( FILE_TSTEP(F_LTNG)  )
               LT_TSTEP_F = FILE_TSTEP(F_LTNG) 
            ELSE
                 LT_TSTEP = TIME2SEC ( TSTEP )
                 LT_TSTEP_F = TSTEP 
                 XMSG = 'Using derived parameters for KF scheme '
                 CALL M3MSG2( XMSG )
            END IF  !END if LTNGON_DATA 

!            LT_TSTEP_F = SEC2TIME (LT_TSTEP)

!C Set up vertical layers
            ALLOCATE( VGLVSLT( 0:DESID_LAYS ), STAT = STATUS )
            CALL CHECKMEM( STATUS, 'VGLVSLT', PNAME )

            ALLOCATE( ICCG        ( NCOLS,NROWS ),
     &                   LTNG_PRSFC  ( NCOLS,NROWS ),
     &                   LTNG_DATA ( NCOLS,NROWS,LT_VARS ),
     &                   LTNG_RC     ( NCOLS,NROWS ),STAT = STATUS )
               IF ( STATUS .NE. 0 ) THEN
                  XMSG = 'ICCG, OCEAN_MASK, LTNG_Parameters'
     &                 // '  memory allocation failed'
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF
               IF ( CJDATE .GT. 90 .AND. CJDATE .LT. 274 ) THEN
                  ICCG = ICCG_SUM
               ELSE
                  ICCG = ICCG_WIN
               END IF
!            END IF  !LPARAM
!C Store local layer information
            DO L = DESID_LAYS, 0, -1
                  VGLVSLT( L ) = VGLVS_GD( L+1 )
!                  WRITE( LOGDEV,'(5X, A, I3, A, F11.7)' ) 'VGLVSLT(', L, ' ):', VGLVSLT( L )
            END DO


            LDATE = STDATE; LTIME = STTIME
            IF ( LTNGDIAG ) THEN
!C Build description for, and open lightning diagnostic file
!C (all but variables-table and horizontal domain in description is borrowed from MNAME)
               FDATE = STDATE
               FTIME = STTIME
               CALL NEXTIME( FDATE, FTIME, TSTEP )
               SDATE3D = FDATE
               STIME3D = FTIME
               TSTEP3D = TSTEP

               FTYPE3D = GRDDED3
               NCOLS3D = GL_NCOLS
               NROWS3D = GL_NROWS
               NTHIK3D = 1
               GDTYP3D = GDTYP_GD
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
               GDNAM3D = GRID_NAME  ! from HGRD_DEFN

               NLAYS3D = DESID_LAYS
               DO L = 1, NLAYS3D + 1
                  VGLVS3D( L ) = VGLVS_GD( L )
               END DO

               NVARS3D = 1
               VNAME3D( 1 ) = LTSPC
               VDESC3D( 1 ) = 'hourly average NO produced from lightning'
               VTYPE3D( 1 ) = M3REAL
               UNITS3D( 1 ) = 'mol s-1'

               FDESC3D = ' '   ! array assignment
               FDESC3D( 1 ) = 'Gridded lightning NO production from CMAQ'
               FDESC3D( 2 ) = '/from/ ' // PNAME
               FDESC3D( 3 ) = '/Version/ CMAQ'

               ! Store MIO Metadata
               NDIMS3D( 1:NVARS3D ) = 4
               L_TSTEP( 1:NVARS3D ) = .True.
               L_LAY  ( 1:NVARS3D ) = .True.
               L_COL  ( 1:NVARS3D ) = .True.
               L_ROW  ( 1:NVARS3D ) = .True.
               L_VEXT ( 1:NVARS3D ) = .False.
          
               CALL LOAD_MIO_FILE ( 'CTM_LTNGDIAG_1', NLAYS3D, 1, NLAYS3D,
     &              VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &              VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &              L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),
     &              L_VEXT(1:NVARS3D) )
               
               ! Store MIO Metadata
               NDIMS3D( 1:NVARS3D ) = 3
               L_TSTEP( 1:NVARS3D ) = .True.
               L_LAY  ( 1:NVARS3D ) = .False.
               L_COL  ( 1:NVARS3D ) = .True.
               L_ROW  ( 1:NVARS3D ) = .True.
               L_VEXT ( 1:NVARS3D ) = .False.
               
               CALL LOAD_MIO_FILE ( 'CTM_LTNGDIAG_2', 0, 0, 0,
     &              VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &              VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &              L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),
     &              L_VEXT(1:NVARS3D) )
               
!C Open output file (mol s-1)
               IF ( IO_PE_INCLUSIVE ) THEN
                  IF ( .NOT. OPEN3( CTM_LTNGDIAG_1, FSUNKN3, PNAME ) ) THEN
                     XMSG = 'Could not open ' // TRIM( CTM_LTNGDIAG_1 )
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
                  NLAYS3D = 1
                  VDESC3D( 1 ) = 'Column NO produced from lightning'
                  IF ( .NOT. OPEN3( CTM_LTNGDIAG_2, FSUNKN3, PNAME ) ) THEN
                     XMSG = 'Could not open ' // TRIM( CTM_LTNGDIAG_2 )
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF

               END IF

               ALLOCATE( VDEMIS_LTDIAG( NCOLS,NROWS,DESID_LAYS ), STAT = STATUS )
               IF ( STATUS .NE. 0 ) THEN
                  XMSG = 'VDEMIS_LTDIAG memory allocation failed'
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF
               VDEMIS_LTDIAG = 0.0   ! array assignment
               ALLOCATE( COLUMN_DIAG( NCOLS,NROWS,1), STAT = STATUS )
               IF ( STATUS .NE. 0 ) THEN
                  XMSG = 'COLUMN_DIAG memory allocation failed'
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF
               COLUMN_DIAG = 0.0   ! array assignment
            END IF   ! LTNGDIAG

#endif

         LTLYRS = DESID_LAYS

!C Build Emissions Buffer
         ALLOCATE( VDEMIS_LT( NCOLS,NROWS,LTLYRS ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS_LT memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
 
!         RETURN

         END FUNCTION LTNG_INIT

!C======================================================================
!C Get NO produced from lightning in VDEMIS_LT

         SUBROUTINE GET_LTNG ( JDATE, JTIME, TSTEP, L_DESID_DIAG )

         USE GRID_CONF             ! horizontal & vertical domain specifications
         USE UTILIO_DEFN
         USE CENTRALIZED_IO_MODULE, only : interpolate_var,
     &                                     OCEAN_MASK,
     &                                     SLOPE,
     &                                     INTERCEPT,
     &                                     SLOPE_lg,
     &                                     INTERCEPT_lg,
     &                                     ICCG_SUM,
     &                                     ICCG_WIN,
     &                                     LT_NAME,
     &                                     LT_VARS
#ifdef mpas
         use util_module, only : time2sec, nextime, upcase
#else
#ifdef twoway
         USE twoway_data_module
#endif
#endif
         IMPLICIT NONE

         INCLUDE SUBST_FILES_ID  ! file name parameters

         INTEGER JDATE, JTIME, TSTEP( 3 )
         INTEGER STATUS
         LOGICAL, INTENT( IN ) :: L_DESID_DIAG ! flag determining whether or not DESID
                                               !   is in diagnostic mode              
         REAL,   PARAMETER :: CONVPA = 1.0E-2  ! convert Pa to hPa
!        REAL,   PARAMETER :: WK = 8.0         ! shape parameter for weibull distribution
!        REAL,   PARAMETER :: WLAMBDA = 700.0  ! scale parameter for weibull distribution
         REAL,   PARAMETER :: WMU = 350.0      ! mean
         REAL,   PARAMETER :: WSIGMA = 200.0   ! standard deviation
         REAL,   PARAMETER :: W2MU = 600.0     ! mean
         REAL,   PARAMETER :: W2SIGMA = 50.0   ! standard deviation
         REAL,   PARAMETER :: SQRT2 = 1.414213562731
         REAL,   PARAMETER :: SFACTOR1 = 0.95  ! the scaling factor for the wider distribution (WMU350)
         REAL,   PARAMETER :: SFACTOR2 = 0.12  ! the scaling factor for the wider distribution (WMU350

         INTEGER COL, ROW, LAY ! iterator variables
         INTEGER v

         REAL    PCALC    ! pressure level for NO vertical distribution (hPa)
         REAL    BOTTOM   ! pressure at bottom of grid cell (hPa)
         REAL    TOP      ! pressure at top of grid cell (hPa)        
         REAL    BOTTOM_FRAC, TOP_FRAC ! their difference is the fraction of lightning NO in this grid cell
         REAL    BOTTOM_FRAC2, TOP_FRAC2
         REAL    SUM_FRAC ! stores the sum of vertical fractions to re-normalize the column
         REAL    WEIGHT ! used to normalize emissions to total amount
         REAL    inErfB, inErfT !  nputs to error funciton calculation
         REAL    outErfB, outErfT ! outputs from error funciton calculation
         REAL :: LTEMIS( LTLYRS )
         REAL    XCELLR, YCELLR   ! cell spacing ratio to 36Km
         REAL    FLASH_FAC        ! lightning flashes factor

         LOGICAL,     SAVE :: LASTTIC   ! true: last sync step this output tstep
         REAL              :: DIVFAC   ! averaging factor for diagnostic file

         CHARACTER( 16 )   :: MNAME
         CHARACTER( 16 )   :: PNAME = 'GET_LTNG'
         CHARACTER( 120 )  :: XMSG = ' '
         REAL, ALLOCATABLE :: COLUMN_LTNG_NO ( :,: ) ! column total NO
         
         INTEGER           :: cjdate
         INTEGER           :: DTSTEP
         CHARACTER( 7 )    :: SJDATE

         LOGICAL,     SAVE :: R_READY = .TRUE.
         LOGICAL,     SAVE :: wrf_assim = .FALSE.
         INTEGER,     SAVE :: TOT_TSTEP
         LOGICAL,     SAVE :: FIRSTIME = .TRUE.

         REAL :: LOC_VGTOP_GD

!-----------------------------------------------------------------------
         
         IF ( LTNG_NO ) THEN
!C case of inline lightning NO production
!C initialize output array
            VDEMIS_LT = 0.0

            DTSTEP = TIME2SEC( TSTEP( 1 ) ) / TIME2SEC( TSTEP( 2 ) ) 
            !C Open me filet
!C Get domain window info for met_cro_2d file
            IF (FIRSTIME) THEN
                TOT_TSTEP = 0
                FIRSTIME = .FALSE.
            END IF
            WRITE( SJDATE, '(I7)') JDATE
            READ( SJDATE, '(4X, I3)') CJDATE

!C read in the hourly lightning strike file
#ifdef twoway
            IF(wrf_lightning_assim) THEN
              call interpolate_var ('LNT', jdate, jtime, LTNG_DATA(:,:,1))
!C Interpret the timestep LTNG_DATA value into hourly value
              LTNG_DATA(:,:,1) = LTNG_DATA(:,:,1)*60/LT_ASM_DT
               wrf_assim = .TRUE.
!               WRITE(*,*) "Max LTNG_DATA = ", MAXVAL(LTNG_DATA)
            ELSE
              IF ( .NOT. LTNGON_DATA ) THEN
                call interpolate_var (RC_NAME, jdate, jtime, LTNG_RC)
!C Interpret the timestep RC value into hourly value
                LTNG_RC = LTNG_RC*DTSTEP
              END IF
            END IF
            call interpolate_var ('PRSFC', jdate, jtime, LTNG_PRSFC)
#else
            IF ( R_READY ) THEN
               IF ( .NOT. LTNGON_DATA ) THEN
                  call interpolate_var (RC_NAME, ldate, ltime, LTNG_RC)
               END IF
               call interpolate_var ('PRSFC', ldate, ltime, LTNG_PRSFC)

               IF ( .NOT. LTNGON_DATA ) THEN
                  R_READY = .FALSE.
               END IF
            END IF
#endif
           ! call desc3 (file_name)
            
            IF( R_READY ) THEN
               IF ( LTNGON_DATA ) THEN
                 DO v = 1, LT_VARS 
                   call interpolate_var (LT_NAME(v), ldate, ltime, LTNG_DATA(:,:,v))
                   R_READY = .FALSE.
                 ENDDO
              END IF  ! LTNGON_DATA
            END IF     !R_READY
            ALLOCATE( COLUMN_LTNG_NO( NCOLS,NROWS ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = 'COLUMN_LTNG_NO memory allocation failed'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            COLUMN_LTNG_NO( :,: ) = 0.0
!C Iterate over each grid cell and distribute lightning NO vertically
               DO ROW = 1, NROWS
                  DO COL = 1, NCOLS
                     IF ( LTNGON_DATA .or. wrf_assim ) THEN 
                       IF(LNO_OPTION == 1) THEN
                          COLUMN_LTNG_NO( COL,ROW ) =
     &                     ( LTNG_DATA( COL,ROW,WsflIdx )
     &                       * SQUAREKM     ! LTNG_DATA in km-2
     &                       * OCEAN_MASK( COL,ROW ) ! reduce offshore strikes
     &                       * ( MOLSNCG + MOLSNIC * ICCG( COL,ROW ) ) )
     &                       / 3600.0 ! get time units right: emissions are in the unit of moles/s
                       ELSE IF (LNO_OPTION == 2) THEN
                          COLUMN_LTNG_NO( COL,ROW ) =
     &                     ( LTNG_DATA( COL,ROW,GflIdx )
     &                       * SQUAREKM     ! LTNG_DATA in km-2
     &                       * OCEAN_MASK( COL,ROW ) ! reduce offshore strikes
     &                       * ( MOLSNCG + MOLSNIC * ICCG( COL,ROW ) ) )
     &                       /3600.0 ! get time units right: emissions are in the unit of moles/s
                       ELSE IF (LNO_OPTION == 3) THEN
                          COLUMN_LTNG_NO( COL,ROW ) =
     &                     ( LTNG_DATA( COL,ROW,GegIdx )*BetaF2E)
     &                       / 3600.0 ! get time units right: emissions are in the unit of moles/s
! LNO using maximum of GLMeng and WLNeng with daily Beta/Gama values
                       ELSE IF (LNO_OPTION == 4) THEN 
                          COLUMN_LTNG_NO( COL,ROW ) =
     &                     max( LTNG_DATA( COL,ROW,GegIdx )*BetaF2E,
     &                       LTNG_DATA( COL,ROW,WegIdx )*GamaF2E)   
     &                       / 3600.0 ! get time units right: emissions are in the unit of moles/s
! LNO using maximum of GLMeng and WLNseng with daily Beta/sGama values
! and ICCG adjustmenet (setting the upper bound)
                       ELSE IF (LNO_OPTION == 5) THEN  !
                          COLUMN_LTNG_NO( COL,ROW ) =
     &                     max( LTNG_DATA( COL,ROW,GegIdx )*BetaF2E,
     &                       LTNG_DATA( COL,ROW,WsegIdx )
     &                       * ( 1 + ICCG( COL,ROW ) )*sGamaF2E )
     &                       / 3600.0 ! get time units right: emissions are in the unit of moles/s
                       ELSE
                         XMSG = 'LNO_OPTION is not correctly set; correct range is from 1 to 5'
                         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                       ENDIF
                     ELSE
                        IF ( LTNG_RC( COL,ROW ) .GT. 0 ) THEN
                           SCL_FACTOR = EXP(INTERCEPT_lg(COL, ROW))
                           IF ( LTNG_RC( COL,ROW ) .GT. SCL_FACTOR .AND. OCEAN_MASK( COL,ROW ) .GT. 0.2 )THEN
                              COLUMN_LTNG_NO( COL,ROW ) =
     &                         ( EXP( SLOPE_LG( COL,ROW ) * LOG( LTNG_RC( COL,ROW ) )
     &                           + INTERCEPT_LG( COL,ROW) )
     &                           * SQUAREKM   ! the relation is built on the unit of flash/km2*hr
     &                           * OCEAN_MASK( COL,ROW ) ! reduce offshore strikes
     &                           * ( MOLSNCG  ! moles N per flash intercloud strikes per cloud-to-ground strike
     &                             + ( MOLSNIC * ICCG( COL,ROW ) ) ) )
     &                         /3600.0          ! get time units right
                              IF ( COLUMN_LTNG_NO( COL,ROW ) .LT. 0 ) COLUMN_LTNG_NO( COL,ROW ) = 0.0
                           ELSE
                              COLUMN_LTNG_NO( COL,ROW ) =
     &                         ( ( SLOPE( COL,ROW ) * LTNG_RC( COL,ROW ) + INTERCEPT( COL,ROW ) )
     &                           * SQUAREKM   ! the relation is built on flash/km2*hr
     &                           * OCEAN_MASK( COL,ROW ) ! reduce offshore strikes
     &                           * ( MOLSNCG  ! moles N per flash intercloud strikes per cloud-to-ground strike
     &                             + ( MOLSNIC * ICCG( COL,ROW ) ) ) )
     &                         / 3600.0        ! get time units right 
                              IF ( COLUMN_LTNG_NO( COL,ROW ) .LT. 0 ) COLUMN_LTNG_NO( COL,ROW ) = 0.0
                           END IF
                        ELSE
                           COLUMN_LTNG_NO( COL,ROW ) = 0.0
                        END IF
                     END IF   ! LTNGON_DATA
                  END DO   ! COL
               END DO   ! ROW

            VDEMIS_LT = 0.0   ! array assignment

            DO ROW = 1, NROWS
               DO COL = 1, NCOLS

!C check to see if there are lightning strikes for this grid cell
!C only calculate lightning for cloud top greater than 6500 meters

                  IF ( COLUMN_LTNG_NO( COL,ROW ) .LE. 0.0 ) CYCLE
                  SUM_FRAC = 0.0
                  LTEMIS = 0.0   ! array assignment

#ifdef mpas
                  LOC_VGTOP_GD = 1000.0    ! for simplicity, we have decided to set VGTOP_GD to 1000.0
#else
                  LOC_VGTOP_GD = VGTOP_GD
#endif

                  DO LAY = 1, LTLYRS

!C Get pressures: Use SIGMA values and surface pres.
!p=sigma*(psfc-ptop)+ptop
                     BOTTOM = ( VGLVSLT( LAY-1 )
     &                      * ( LTNG_PRSFC( COL,ROW ) - LOC_VGTOP_GD )
     &                      + LOC_VGTOP_GD ) * CONVPA
!                           write( logdev,* ) "bottom: ", bottom
                     TOP    = ( VGLVSLT( LAY )
     &                      * ( LTNG_PRSFC( COL,ROW ) - LOC_VGTOP_GD )
     &                      + LOC_VGTOP_GD ) * CONVPA

!C Find the bottom and top of each layer, and calculate the fraction 
!C of the column emissions for that layer
!C Use normal distribution, mean = wmu, standard deviation = wsigma
                     inErfB      = ( BOTTOM - WMU ) / ( WSIGMA * SQRT2 )
                     inErfT      = ( TOP - WMU ) / ( WSIGMA * SQRT2 )
                     outErfB     = ERF( inErfB )
                     outErfT     = ERF( inErfT )
                     BOTTOM_FRAC = 0.5 * ( 1.0 + outErfB )
                     TOP_FRAC    = 0.5 * ( 1.0 + outErfT )

!C Find the bottom and top of each layer, and calculate the fraction
!C of the column emissions for that layer
!C use normal distribution, mean = wmu, standard deviation = wsigma
                     inErfB       = ( BOTTOM - W2MU ) / ( W2SIGMA * SQRT2 )
                     inErfT       = ( TOP - W2MU ) / ( W2SIGMA * SQRT2 )
                     outErfB      = ERF( inErfB )
                     outErfT      = ERF( inErfT )
                     BOTTOM_FRAC2 = 0.5 * ( 1.0 + outErfB )
                     TOP_FRAC2    = 0.5 * ( 1.0 + outErfT )

!C Add weighted contribution to this level
                     WEIGHT = ( BOTTOM_FRAC - TOP_FRAC ) * SFACTOR1
     &                      + ( BOTTOM_FRAC2 - TOP_FRAC2 ) * SFACTOR2 

                     LTEMIS( LAY ) = WEIGHT * COLUMN_LTNG_NO( COL,ROW )

!C Sum weights in order to normalize to 1
                     SUM_FRAC = SUM_FRAC + WEIGHT

!C If emissions are less than 0, generate an error message in the log
                     IF ( LTEMIS( LAY ) .LT. 0.0 ) THEN
                        WRITE( LOGDEV,* ) LTEMIS( LAY ),
     &                                    COLUMN_LTNG_NO( COL,ROW ),
     &                                    BOTTOM_FRAC, TOP_FRAC
                        XMSG = '*** Ltng NO emis is less than zero'
                        CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                     END IF

                  END DO   ! end layers loop 

                  DO LAY = 1, LTLYRS
!C Re-normalize, in some cases area under the error function is not 1
                     VDEMIS_LT( COL,ROW,LAY ) = LTEMIS( LAY ) / SUM_FRAC
                  END DO        ! layers renormalized

               END DO   ! columns
            END DO   ! rows

!C Determine the time to read and/or write the hourly files
            IF ( .NOT. L_DESID_DIAG ) THEN
              NTICS = NTICS + 1
              TOT_TSTEP = TOT_TSTEP + TIME2SEC( TSTEP( 2 ) )
              LASTTIC = TOT_TSTEP .GE. LT_TSTEP
              IF ( LASTTIC ) THEN
                 MTICS = NTICS
                 NTICS = 0
                 TOT_TSTEP = 0
                 CALL NEXTIME( LDATE, LTIME, LT_TSTEP_F)
                 R_READY = .TRUE.
              END IF
            END IF

#ifdef mpas
#else
!C Write lightning NO to the diagnostic file
            IF ( LTNGDIAG .AND. .NOT. L_DESID_DIAG ) THEN 
               VDEMIS_LTDIAG = VDEMIS_LTDIAG + VDEMIS_LT   ! array assignment
               COLUMN_DIAG(:,:,1) = COLUMN_DIAG(:,:,1) + COLUMN_LTNG_NO
               IF ( LASTTIC ) THEN   ! time to write out
                  DIVFAC = 1.0 / REAL( MTICS, 4 )
                  VDEMIS_LTDIAG = VDEMIS_LTDIAG * DIVFAC   ! array assignment
                  COLUMN_DIAG = COLUMN_DIAG*DIVFAC
                  IF ( .NOT. WRITE3( CTM_LTNGDIAG_1, LTSPC, LDATE, LTIME, VDEMIS_LTDIAG ) )  THEN
                     XMSG = 'Could not write to ' // TRIM( CTM_LTNGDIAG_1 )
                     CALL M3EXIT( PNAME, LDATE, LTIME, XMSG, XSTAT2 )
                  ELSE
                     WRITE( LOGDEV,94040 )
     &                    'Timestep written to', TRIM( CTM_LTNGDIAG_1 ),
     &                    'for date and time', LDATE, LTIME
                  END IF
                  IF ( .NOT. WRITE3( CTM_LTNGDIAG_2, LTSPC, LDATE, LTIME, COLUMN_DIAG ) )  THEN
                     XMSG = 'Could not write to ' // TRIM( CTM_LTNGDIAG_2 )
                     CALL M3EXIT( PNAME, LDATE, LTIME, XMSG, XSTAT2 )
                  ELSE
                     WRITE( LOGDEV,94040 )
     &                    'Timestep written to', TRIM( CTM_LTNGDIAG_2 ),
     &                    'for date and time', LDATE, LTIME
                  END IF

                  VDEMIS_LTDIAG = 0.0   ! array assignment
                  COLUMN_DIAG = 0.0
               END IF ! LASTTIC

            END IF  ! diagnostics turned on
#endif

         ELSE  ! LTNGO is not "InLine", but instead specifies a file

!C Read in lightning NO production from an input file
            VDEMIS_LT = 0.0   ! array assignment

            call interpolate_var (LTSPC, jdate, jtime, VDEMIS_LT)

         END IF  ! end lightning NO production inline or from a file


         DEALLOCATE( COLUMN_LTNG_NO )


         RETURN

C------------------  Format  Statements   ------------------------------

94040 FORMAT( /5X, 3( A, :, 1X ), I8, ":", I6.6 )
94042 FORMAT( /5X, A, 1X, I8, ":", I6.6, 1X, 1PE13.5 )

         END SUBROUTINE GET_LTNG

      END MODULE LTNG_DEFN
