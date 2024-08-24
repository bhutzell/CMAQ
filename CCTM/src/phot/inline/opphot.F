
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
      SUBROUTINE OPPHOT ( JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C
C  FUNCTION:  Opens the photolysis diagnostic files
C
C  PRECONDITIONS REQUIRED:
C     None
C
C  REVISION  HISTORY:
C       Date   Who          What
C     -------- ----------   -----------------------------------------
C     01/2008  S.Roselle    Adapted from OPDIAM in the aerosol module
C                           for opening the photolysis diagnostic files
C     03/2011  B.Hutzell    Generalized and modified to write out surface albedo
C     03/29/11 S.Roselle    Replaced I/O API include files with UTILIO_DEFN
C     09/30/14 B.Hutzell    Added several diagnostics based on changes to cloud
C                           and aerosol description in radiation transfer solution
C-----------------------------------------------------------------------

      USE GRID_CONF               ! horizontal & vertical domain specifications
      USE RXNS_DATA               ! chemical mechanism declarations and data
      USE UTILIO_DEFN
      USE PHOT_MET_DATA, ONLY:  USE_ACM_CLOUD ! Met and Grid data
      USE PHOT_MOD                            ! photolysis in-line routines and data
      USE MIO_ASCII

      IMPLICIT none

      INCLUDE SUBST_FILES_ID  ! file name parameters

C...Arguments

      INTEGER, INTENT( IN ) :: JDATE  ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME  ! current model time, coded HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP  ! output time step

C...Local variables

      CHARACTER( 16 ), SAVE :: PNAME = 'OPPHOT'
      CHARACTER( 16 )       :: LAMBDA
      CHARACTER( 96 )       :: XMSG = ' '
      
      CHARACTER( 16 ), ALLOCATABLE :: VNAME( : )
      CHARACTER( 16 ), ALLOCATABLE :: UNITS( : )
      CHARACTER( 256), ALLOCATABLE :: VDESC( : )
      INTEGER,         ALLOCATABLE :: VTYPE( : )
      INTEGER,         ALLOCATABLE :: NDIMS( : )
      
      INTEGER :: NVARS_RJ_1 ! = 23 + 7 * NWL
      INTEGER :: NVARS_RJ_2 ! = NPHOTAB
      INTEGER :: NVARS_RJ_3 ! = 2 + 6 * N_DIAG_WVL
      INTEGER :: NVARS_RJ_MAX
      
      LOGICAL, SAVE ::  INITIALIZED = .FALSE.

      INTEGER N, L, JWL, INCR     ! loop variables
C-----------------------------------------------------------------------

      IF ( INITIALIZED ) THEN
         RETURN
      END IF
      INITIALIZED = .TRUE.
#ifdef mpas
      RETURN 
#endif
      
C...Try to open existing file for update

#ifndef phot_debug
      NVARS_RJ_1 = 21 + 7 * NWL
#else
      NVARS_RJ_1 = 21 + 9 * NWL
#endif
      IF ( USE_ACM_CLOUD ) NVARS_RJ_1 = NVARS_RJ_1 + 2      
      NVARS_RJ_2 = NPHOTAB
      NVARS_RJ_3 = 2 + 6 * N_DIAG_WVL
      NVARS_RJ_MAX = MAX( NVARS_RJ_1,NVARS_RJ_2,NVARS_RJ_3)


         
         
         ALLOCATE( NDIMS( NVARS_RJ_MAX))
         NDIMS = 0

         ALLOCATE( VNAME( NVARS_RJ_MAX),
     &             UNITS( NVARS_RJ_MAX),         
     &             VDESC( NVARS_RJ_MAX),         
     &             VTYPE( NVARS_RJ_MAX))

         VNAME = " "
         UNITS = " "
         VDESC = " "
         VTYPE = M3REAL
          
C...CSA Variables, Units and Descriptions for RJ_FILE

         N = 1
         VNAME( N ) = 'COSZENS'
         UNITS( N ) = ''
         VDESC( N ) = 'Cosine of Solar Zenith Angle'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'OZONE_COLUMN'
         UNITS( N ) = 'DU'
         VDESC( N ) = 'Observed Total Ozone Column Density'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'NO2_COLUMN'
         UNITS( N ) = 'petamolec cm-2'
         VDESC( N ) = 'Predicted nitrogen dioxide column density'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'CO_COLUMN'
         UNITS( N ) = 'petamolec cm-2'
         VDESC( N ) = 'Predicted carbon monoxide column density'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'SO2_COLUMN'
         UNITS( N ) = 'petamolec cm-2'
         VDESC( N ) = 'Predicted sulfur dioxide column density'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'HCHO_COLUMN'
         UNITS( N ) = 'petamolec cm-2'
         VDESC( N ) = 'Predicted formaldehyde column density'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'TROPO_O3_COLUMN'
         UNITS( N ) = 'DU'
         VDESC( N ) = 'Predicted Tropospheric Ozone Column density'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'JNO2'
         UNITS( N ) = 'min-1'
         VDESC( N ) = 'Photodissociation rate of NO2'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'JO3O1D'
         UNITS( N ) = 'min-1'
         VDESC( N ) = 'Photodissociation rate of ozone producing O(1D)'
         VTYPE( N ) = M3REAL


         N = N + 1
         VNAME( N ) = 'RESOLVED_CFRAC'
         UNITS( N ) = '1'
         VDESC( N ) = 'Resolved Cloud Fraction averaged over cloudy layers'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'RESOLVED_WBAR'
         UNITS( N ) = 'g m-3'
         VDESC( N ) = 'Resolved Cloud Hydrometeor Content averaged over cloudy layers'
         VTYPE( N ) = M3REAL
         
         IF( USE_ACM_CLOUD )THEN
             N = N + 1
             VNAME( N ) = 'SUBGRID_CFRAC'
             UNITS( N ) = '1'
             VDESC( N ) = 'Subgrid Cloud Fraction averaged over cloudy layers'
             VTYPE( N ) = M3REAL

             N = N + 1
             VNAME( N ) = 'SUBGRID_WBAR'
             UNITS( N ) = 'g m-3'
             VDESC( N ) = 'Subgrid Cloud Hydrometeor Content averaged over cloudy layers'
             VTYPE( N ) = M3REAL
         END IF             

         N = N + 1
         VNAME( N ) = 'TRANS_DIFFUSE'
         UNITS( N ) = '1'
         VDESC( N ) = 'broad band transmission coefficient for diffuse radiation at surface'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'TRANS_DIRECT'
         UNITS( N ) = '1'
         VDESC( N ) = 'broad band transmission coefficient for direct radiation at surface'
         VTYPE( N ) = M3REAL
 
         N = N + 1
         VNAME( N ) = 'REFLECTION'
         UNITS( N ) = '1'
         VDESC( N ) = 'broad band reflection coefficient at top of atmosphere'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'CLR_TRANS_DIF'
         UNITS( N ) = '1'
         VDESC( N ) = 'broad band diffuse transmission for clear sky at surface'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'CLR_TRANS_DIR'
         UNITS( N ) = '1'
         VDESC( N ) = 'broad band direct transmission for clear sky at surface'
         VTYPE( N ) = M3REAL
              
         N = N + 1
         VNAME( N ) = 'CLR_REFLECTION'
         UNITS( N ) = '1'
         VDESC( N ) = 'broad band reflection for clear sky at top of atmosphere'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'TROPO_O3_EXCEED'
         UNITS( N ) = '1'
         VDESC( N ) = 'Average Exceedance of modeled ozone column from max fraction of Total Column, '
     &               // ' a relative fraction from total column.'         
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'N_EXCEED_TROPO3'
         UNITS( N ) = ''
         VDESC( N ) = 'occurances predicted tropospheric ozone column exceeds observed total column '
     &               // 'per file time step'          
         VTYPE( N ) = M3REAL


         DO JWL = 1, NWL

C...assumes that lamba in nanometers is on order of 100 or less

            WRITE( LAMBDA,'(I3.3)' ) INT( WAVELENGTH( JWL ) )

            N = N + 1
            VNAME( N ) = 'ETOT_SFC_W' // TRIM( LAMBDA )
            UNITS( N ) = 'W m-2'
            VDESC( N ) = 'Total Downward Irradiance at surface at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL

            N = N + 1
            VNAME( N ) = 'AOD_W' // TRIM( LAMBDA )
            UNITS( N ) = ''
            VDESC( N ) = 'Total Aerosol Optical Depth at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL

            N = N + 1
            VNAME( N ) = 'AOD_ABS_W' // TRIM( LAMBDA )
            UNITS( N ) = ''
            VDESC( N ) = 'Absorption Aerosol Optical Depth at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL

            N = N + 1
            VNAME( N ) = 'TAU_CLOUD_W' // TRIM( LAMBDA )
            UNITS( N ) = ''
            VDESC( N ) = 'Cloud Optical Depth at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL
#ifdef phot_debug
            N = N + 1
            VNAME( N ) = 'SSA_CLOUD_W' // TRIM( LAMBDA )
            UNITS( N ) = '1'
            VDESC( N ) = 'Column Averaged Cloud Single Scattering Albedo at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL

            N = N + 1
            VNAME( N ) = 'ASY_CLOUD_W' // TRIM( LAMBDA )
            UNITS( N ) = ''
            VDESC( N ) = 'Column Averaged Cloud Asymmetry Factor at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL
#endif
            N = N + 1
            VNAME( N ) = 'TAU_TOT_W' // TRIM( LAMBDA )
            UNITS( N ) = ''
            VDESC( N ) = 'Total Optical Depth at'
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL

            N = N + 1
            VNAME( N ) = 'TAUO3_TOP_W' // TRIM( LAMBDA )
            UNITS( N ) = ''
            VDESC( N ) = 'Optical Depth of O3 above model domain at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL

            N = N + 1
            VNAME( N ) = 'ALBEDO_W' // TRIM( LAMBDA )
            UNITS( N ) = '1'
            VDESC( N ) = 'Surface Albedo at the wavelength at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL

         ENDDO

         N = N + 1
         VNAME( N ) = 'AOD_W550_ANGST' 
         UNITS( N ) = ''
         VDESC( N ) = 'Aerosol Optical Depth at'
     &                // ' 550 nm based on an Angstrom Interpolation'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'AAOD_W550_ANGST' 
         UNITS( N ) = ''
         VDESC( N ) = 'Aerosol Absorption Optical Depth at'
     &                // ' 550 nm based on an Angstrom Interpolation'
         VTYPE( N ) = M3REAL

         IF ( N .NE. NVARS_RJ_1 ) THEN
            WRITE(XMSG,'("NVARS_RJ1_1 equal ",I3," but needs to equal ",I3)')
     &      NVARS_RJ_1,N
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

!C...Write ascii table describing variables
!         WRITE(LOGDEV,'(A)')'*PHOTDIAG1 File Contents'
!         WRITE(LOGDEV,'(A)')'**Surface Values of Optical Inputs and Radiative '
!     &                   // 'Results from the In-line calculation of Photolysis Rates '
!         WRITE(LOGDEV,99950)
!         WRITE(LOGDEV,99951)
!         DO L = 1, NVARS3D
!            WRITE(LOGDEV,99952)TRIM(VNAME( L )),TRIM(UNITS( L )),TRIM(VDESC( L ))
!         END DO

         ! Store MIO Metadata
         NDIMS( 1:NVARS_RJ_1 ) = 4
         L_TSTEP( 1:NVARS_RJ_1 ) = .True.
         L_LAY  ( 1:NVARS_RJ_1 ) = .True.
         L_COL  ( 1:NVARS_RJ_1 ) = .True.
         L_ROW  ( 1:NVARS_RJ_1 ) = .True.
         L_VEXT ( 1:NVARS_RJ_1 ) = .False.
       
         CALL LOAD_MIO_FILE ( 'CTM_RJ_1_MIO', 1, 1, 1,
     &        VNAME(1:NVARS_RJ_1), VTYPE(1:NVARS_RJ_1), UNITS(1:NVARS_RJ_1),
     &        VDESC(1:NVARS_RJ_1), NDIMS(1:NVARS_RJ_1), L_TSTEP(1:NVARS_RJ_1),
     &        L_LAY(1:NVARS_RJ_1),   L_COL(1:NVARS_RJ_1),   L_ROW(1:NVARS_RJ_1),
     &        L_VEXT(1:NVARS_RJ_1) )
 

      IF ( .NOT. OPEN3( CTM_RJ_1, FSRDWR3, PNAME ) ) THEN

         XMSG = 'Could not open ' // CTM_RJ_1 // ' file for update - '
     &        // 'try to open new'
         CALL LOG_MESSAGE( LOGDEV ,  XMSG )
C...Set output file characteristics based on COORD.EXT and open
C...  the photolysis diagnostic file

         FTYPE3D = GRDDED3
         SDATE3D = JDATE
         STIME3D = JTIME
         TSTEP3D = TSTEP

         NCOLS3D = GL_NCOLS
         NROWS3D = GL_NROWS
         NLAYS3D =     1
         NTHIK3D =     1
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

         DO L = 1, NLAYS3D + 1
            VGLVS3D( L ) = VGLVS_GD( L )
         END DO

         GDNAM3D = GRID_NAME  ! from HGRD_DEFN
         FDESC3D( 1 ) = 'Surface Values of Optical Inputs and Radiative Results '
         FDESC3D( 2 ) = 'from the In-line calculation of Photolysis Rates '
         FDESC3D( 3 ) = 'for the ' // TRIM( MECHNAME ) // ' photochemical mechanism '
         DO L = 4, MXDESC3
            FDESC3D( L ) = ' '
         END DO

C...Open the 1st photolysis diagnostic file

         NVARS3D = NVARS_RJ_1
         DO N = 1, NVARS3D
            VNAME3D( N ) = VNAME( N )
            VTYPE3D( N ) = VTYPE( N )
            UNITS3D( N ) = UNITS( N )
            VDESC3D( N ) = VDESC( N )
         END DO

         IF ( .NOT. OPEN3( CTM_RJ_1, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_RJ_1 // ' file'
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

      END IF
    
C...load data from photolysis reaction list

         DO N = 1, NVARS_RJ_2
            VNAME( N ) = PHOTAB( N )
            VTYPE( N ) = M3REAL
            UNITS( N ) = 'min-1'
            VDESC( N ) = 'Photolysis rates calculated based on data file; ' // VNAME(N)
         END DO

!C...Write ascii table describing variables
!         WRITE(LOGDEV,'(A)')'*PHOTDIAG2 File Contents'
!         WRITE(LOGDEV,'(A)')'**Three dimensionals values of Photolysis rates '
!     &                   // 'used to make predictions from the In-line calculation '
!     &                   // ' of Photolysis Rates '
!         WRITE(LOGDEV,99950)
!         WRITE(LOGDEV,99951)
!         DO L = 1, NVARS3D
!            WRITE(LOGDEV,99952)TRIM(VNAME( L )),TRIM(UNITS( L )),TRIM(VDESC( L ))
!         END DO
 
         ! Store MIO Metadata
         NDIMS( 1:NVARS_RJ_2 ) = 4
         L_TSTEP( 1:NVARS_RJ_2 ) = .True.
         L_LAY  ( 1:NVARS_RJ_2 ) = .True.
         L_COL  ( 1:NVARS_RJ_2 ) = .True.
         L_ROW  ( 1:NVARS_RJ_2 ) = .True.
         L_VEXT ( 1:NVARS_RJ_2 ) = .False.
       
         CALL LOAD_MIO_FILE ( 'CTM_RJ_2_MIO', NLAYS3D, 1, NLAYS3D,
     &        VNAME(1:NVARS_RJ_2), VTYPE(1:NVARS_RJ_2), UNITS(1:NVARS_RJ_2),
     &        VDESC(1:NVARS_RJ_2), NDIMS(1:NVARS_RJ_2), L_TSTEP(1:NVARS_RJ_2),
     &        L_LAY(1:NVARS_RJ_2),   L_COL(1:NVARS_RJ_2),   L_ROW(1:NVARS_RJ_2),
     &        L_VEXT(1:NVARS_RJ_2) )
 

C...Try to open existing file for update

      IF ( .NOT. OPEN3( CTM_RJ_2, FSRDWR3, PNAME ) ) THEN

         XMSG = 'Could not open ' // CTM_RJ_2 // ' file for update - '
     &        // 'try to open new'
         CALL LOG_MESSAGE( LOGDEV ,  XMSG )

C...Set output file characteristics based on COORD.EXT and open
C...  the photolysis diagnostic file

         FTYPE3D = GRDDED3
         SDATE3D = JDATE
         STIME3D = JTIME
         TSTEP3D = TSTEP

         NCOLS3D = GL_NCOLS
         NROWS3D = GL_NROWS
         NLAYS3D = NLAYS_DIAG
         NTHIK3D =     1
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

         DO L = 1, NLAYS3D + 1
            VGLVS3D( L ) = VGLVS_GD( L )
         END DO

         NVARS3D = NVARS_RJ_2
         DO N = 1, NVARS3D
            VNAME3D( N ) = VNAME( N )
            VTYPE3D( N ) = VTYPE( N )
            UNITS3D( N ) = UNITS( N )
            VDESC3D( N ) = VDESC( N )
         END DO

         FDESC3D( 1 ) = 'Three dimensional values of Photolysis rates '
         FDESC3D( 2 ) = 'used to make predictions for the ' // TRIM( MECHNAME )
         FDESC3D( 3 ) = 'photochemical mechanism from the In-line calculation.'
         FDESC3D( 4 ) = 'Data files can be found in CMAQ repository under subdirectory,'
         FDESC3D( 5 ) = 'UTIL/inline_phot_preproc/photolysis_CSQY_data'
         DO N = 6, MXDESC3
            FDESC3D( N ) = ' '
         END DO

         IF ( .NOT. OPEN3( CTM_RJ_2, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_RJ_2 // ' file'
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

      END IF

         
         N = 0
         
         DO L = 1, N_DIAG_WVL

C...assumes that lamba in nanometers is on order of 100 or less

            JWL = DIAG_WVL( L )
            
            WRITE( LAMBDA,'(I3.3)' ) INT( WAVELENGTH( JWL ) )

            N = N + 1
            VNAME( N ) = 'AERO_SCAT_W' // TRIM( LAMBDA )
            UNITS( N ) = 'Km-1'
            VDESC( N ) = 'Aerosol Scattering of layer at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL

            N = N + 1
            VNAME( N ) = 'AERO_ASYM_W' // TRIM( LAMBDA )
            UNITS( N ) = ''
            VDESC( N ) = 'Aerosol Asymmetry Factor at '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL

            N = N + 1
            VNAME( N ) = 'EXT_W' // TRIM( LAMBDA )
            UNITS( N ) = 'Km-1'
            VDESC( N ) = 'Total Extinction of layer for '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL

            N = N + 1
            VNAME( N ) = 'GAS_EXT_W' // TRIM( LAMBDA )
            UNITS( N ) = 'Km-1' 
            VDESC( N ) = 'Total Extinction from Rayleigh scattering NO2 and O3 in layer for '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL
 
            N = N + 1
            VNAME( N ) = 'EXT_AERO_W' // TRIM( LAMBDA )
            UNITS( N ) = 'Km-1'
            VDESC( N ) = 'Aerosol Extinction in layer for '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL
            
            N = N + 1
            VNAME( N ) = 'ACTINIC_FX_W' // TRIM( LAMBDA )
            UNITS( N ) = 'W m-2'
            VDESC( N ) = 'Net Actinic Flux, '
     &                   // TRIM( LAMBDA ) // ' nm'
            VTYPE( N ) = M3REAL

        END DO
        
         N = N + 1
         VNAME( N ) = 'CFRAC_3D'
         UNITS( N ) = '1'
         VDESC( N ) = 'Resolved Cloud Fraction in grid cell'
         VTYPE( N ) = M3REAL

         N = N + 1
         VNAME( N ) = 'EXT_AERO_W550'
         UNITS( N ) = 'Km-1'
         VDESC( N ) = ' Aerosol Extinction of layer for '
     &                // '550 nm based on an Angstrom Interpolation'
         VTYPE( N ) = M3REAL
        
         IF ( N .NE. NVARS_RJ_3 ) THEN
            WRITE(XMSG,'("NVARS_RJ_3 equal ",I3," but needs to equal ",I3)')
     &      NVARS_RJ_3,N
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF
!C...Write ascii table describing variables
!         WRITE(LOGDEV,'(A)')'*PHOTDIAG3 File Contents'
!         WRITE(LOGDEV,'(A)')'**Three dimensionals values of Optical Inputs and Radiative '
!     &                    // 'Results from the In-line photolysis calculation.'
!         WRITE(LOGDEV,99950)
!         WRITE(LOGDEV,99951)
!         DO L = 1, NVARS3D
!            WRITE(LOGDEV,99952)TRIM(VNAME( L )),TRIM(UNITS( L )),TRIM(VDESC( L ))
!         END DO

         ! Store MIO Metadata
         NDIMS( 1:NVARS_RJ_3 ) = 4
         L_TSTEP( 1:NVARS_RJ_3 ) = .True.
         L_LAY  ( 1:NVARS_RJ_3 ) = .True.
         L_COL  ( 1:NVARS_RJ_3 ) = .True.
         L_ROW  ( 1:NVARS_RJ_3 ) = .True.
         L_VEXT ( 1:NVARS_RJ_3 ) = .False.
       
         CALL LOAD_MIO_FILE ( 'CTM_RJ_3_MIO', NLAYS_DIAG, 1, NLAYS_DIAG,
     &        VNAME(1:NVARS_RJ_3), VTYPE(1:NVARS_RJ_3), UNITS(1:NVARS_RJ_3),
     &        VDESC(1:NVARS_RJ_3), NDIMS(1:NVARS_RJ_3), L_TSTEP(1:NVARS_RJ_3),
     &        L_LAY(1:NVARS_RJ_3),   L_COL(1:NVARS_RJ_3),   L_ROW(1:NVARS_RJ_3),
     &        L_VEXT(1:NVARS_RJ_3) )

C...Try to open existing file for update

      IF ( .NOT. OPEN3( CTM_RJ_3, FSRDWR3, PNAME ) ) THEN

         XMSG = 'Could not open ' // CTM_RJ_3 // ' file for update - '
     &        // 'try to open new'
         CALL LOG_MESSAGE( LOGDEV ,  XMSG )

C...Set output file characteristics based on COORD.EXT and open
C...  the photolysis diagnostic file

         FTYPE3D = GRDDED3
         SDATE3D = JDATE
         STIME3D = JTIME
         TSTEP3D = TSTEP

         NCOLS3D = GL_NCOLS
         NROWS3D = GL_NROWS
         NLAYS3D = NLAYS_DIAG
         NTHIK3D =     1
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

         DO L = 1, NLAYS3D + 1
            VGLVS3D( L ) = VGLVS_GD( L )
         END DO

         NVARS3D = NVARS_RJ_3
         DO N = 1, NVARS3D
            VNAME3D( N ) = VNAME( N )
            VTYPE3D( N ) = VTYPE( N )
            UNITS3D( N ) = UNITS( N )
            VDESC3D( N ) = VDESC( N )
         END DO

         FDESC3D( 1 ) = 'Three dimensionals values of Optical Inputs and Radiative'
         FDESC3D( 2 ) = 'Results from the In-line photolysis calculation using the'
         FDESC3D( 3 ) =  TRIM( MECHNAME ) // ' photochemical mechanism.'

         DO N = 4, MXDESC3
            FDESC3D( N ) = ' '
         END DO
C...Open the 3rd photolysis diagnostic file

         IF ( .NOT. OPEN3( CTM_RJ_3, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_RJ_3 // ' file'
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

      END IF

99950    FORMAT('|Variable Name|Units|Description                                   |')         
99951    FORMAT('|:----|:----:|:---------------------------------------------|')
99952    FORMAT('|', A16, '|', A16, '|', A, '|')

      RETURN

      END SUBROUTINE OPPHOT
