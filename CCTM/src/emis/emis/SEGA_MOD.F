
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
      MODULE SEGA_MOD
C-----------------------------------------------------------------------
C Description:
C    This module estimates soil NO and HONO emissions using a BDSNP type approach.

      USE RUNTIME_VARS
      USE HGRD_DEFN
      USE DESID_VARS
      Use CONST
      Use MOSAIC_MOD, Only: Tile_Data 
#ifdef mpas
      use util_module
#endif

      IMPLICIT NONE

      REAL,    ALLOCATABLE, SAVE :: VDEMIS_SG( :,:,: ) ! soil emis

      Integer, Save :: I_Water
      Integer, Save :: I_Ag

      INTEGER, PARAMETER :: NSGSPC = 2
      CHARACTER( 16 ) :: SG_SPC( NSGSPC )        ! emitted species
      DATA  SG_SPC  /  'NO              ',
     &                 'HONO            '/
      REAL :: SG_MW( NSGSPC )                    ! emitted species molecular weight (gN/mol)
      DATA SG_MW   / 14.0,
     &               14.0 /
      REAL :: OPT_THETA( NSGSPC )                ! water filled pore space of maximum emissions
      DATA OPT_THETA / 0.30,                     ! NO
     &                 0.15 /                    ! HONO
      REAL :: EM_ADJ( NSGSPC )                   ! Emissions relative to soil NO
      DATA EM_ADJ / 1.00,                        ! NO
     &              0.61 /                       ! HONO
      REAL :: EF_LU( 11 )                        ! NO Emission Factor gN/(km2 hr)
! Global geometric mean Steinkamp and Lawrence 2011 www.atmos-chem-phys.net/11/6063/2011/
!      DATA EF_LU / 0.00,                         ! Water
!     &             0.00,                         ! Ice
!     &             0.43,                         ! Barren
!     &             0.46,                         ! Urban
!     &             1.23,                         ! Grassland
!     &             1.23,                         ! Non-growing season ag
!     &             1.07,                         ! Shrubland
!     &             1.66,                         ! ENL
!     &             0.44,                         ! EBL
!     &             0.35,                         ! DNL
!     &             0.08/                         ! DBL     
! Global mean Steinkamp and Lawrence 2011 www.atmos-chem-phys.net/11/6063/2011/
      DATA EF_LU / 0.00,                         ! Water
     &             0.00,                         ! Ice
     &             0.43,                         ! Barren
     &             0.46,                         ! Urban
     &             1.78,                         ! Grassland
     &             3.13,                         ! Non-growing season ag
     &             1.07,                         ! Shrubland
     &             4.60,                         ! ENL
     &             1.14,                         ! EBL
     &             0.95,                         ! DNL
     &             0.95/                         ! DBL     
!      REAL            :: DX1, DX2                ! CX x1- and x2-cell widths    
      REAL, SAVE      :: GRID_AREA               ! horizontal area of cells, km**2   
      REAL, SAVE      :: UNIT_CON                ! converts from gN/(km2 hr) to mol/s   
     
      CONTAINS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         FUNCTION SEGA_INIT( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )
C Revision History:
C   May 2025: J. Bash adapted from MGEMS_INIT in MGEMS_MOD.F

            USE UTILIO_DEFN
            USE HGRD_DEFN
            USE MOSAIC_MOD,   ONLY : Init_Mosaic, Tile_Data
            USE RUNTIME_VARS, ONLY : USE_SEGA_N
            USE ASX_DATA_MOD, ONLY : Grid_Data

            IMPLICIT NONE
            INCLUDE SUBST_FILES_ID                                  ! file name parameters

C Arguments:
            INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP
            LOGICAL :: SUCCESS

C Local Variables:

            CHARACTER( 16 ), SAVE :: PNAME = 'SEGA_INIT'
            CHARACTER( 96 )  :: XMSG = ' '
            INTEGER STATUS
            INTEGER l, spc

C-----------------------------------------------------------------------

            SUCCESS = .TRUE.

C flag for using SEGA soil NOx emissions
            IF ( .NOT. USE_SEGA_N ) RETURN

C Allocate emission arrays using NSGSPC value
            DESID_EMVAR( ISGSRM )%LEN = NSGSPC
            ALLOCATE( DESID_EMVAR( ISGSRM )%ARRY ( NSGSPC ) )
            ALLOCATE( DESID_EMVAR( ISGSRM )%UNITS( NSGSPC ) )

            DESID_EMVAR( ISGSRM )%ARRY  = SG_SPC
            DESID_EMVAR( ISGSRM )%UNITS = 'MOLES/S'

            ALLOCATE ( VDEMIS_SG( NSGSPC,NCOLS,NROWS), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = '*** VDEMIS_SG, memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            CALL INIT_MOSAIC( JDATE, JTIME, Grid_Data%LUFRAC )

            DO l = 1, Tile_Data%n_lufrac
               If( trim( Tile_Data%cat_lu( l ) ) .EQ. 'WATER' ) Then
                      I_Water = l
               End If
               If( trim( Tile_Data%cat_lu( l ) ) .EQ. 'AG' ) Then
                      I_ag    = l
               End If
            END DO
            
         END FUNCTION SEGA_INIT


         SUBROUTINE GET_SEGA_EMIS ( JDATE, JTIME, TSTEP, L_DESID_DIAG )
C Key Subroutines/Functions Called:  F_PULSE, F_TS, F_SW, F_FERT

C Revision History:
C
CReferences:

C GET_SG_EMIS calculates the soil gas emission rates in a grid cell
            USE RUNTIME_VARS
            USE HGRD_DEFN             ! horizontal domain specifications
            USE VGRD_DEFN !,    only: VGLVS_GD
            USE ASX_DATA_MOD, only: Met_Data, Grid_Data
            USE MOSAIC_MOD,   only: Tile_Data, zsoil1, GET_STAGE_MET
            USE UTILIO_DEFN
#ifdef parallel
            USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
            USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
            USE centralized_io_module, only: soilinp_setup, GROWAGNO, NGROWAGNO, NONAGNO, PFACTOR, 
     &                                       DRYPERIOD, FREEZEPERIOD, THETAP, MSFX2

            IMPLICIT NONE

            INTEGER, PARAMETER :: LSM_WATER = 14
C Arguments:
            INTEGER, INTENT( IN )  :: JDATE, JTIME, TSTEP( 3 )
            LOGICAL, INTENT( IN )  :: L_DESID_DIAG   ! flag determining whether or not DESID
                                                     ! is in diagnostic mode   
            INTEGER          SOILCAT                 ! soil category           
            INTEGER          R, C, L, NSpc, N_LU     ! counters

            CHARACTER( 16 ), SAVE :: SOILOUT = 'SEGA_SOILOUT' ! logical name for output NO and HONO soil data
            CHARACTER( 16 ) :: VAR        ! variable name

            INTEGER, SAVE :: EDATE     ! end scenario date
            INTEGER, SAVE :: ETIME     ! end scenario time
            INTEGER, SAVE :: NDATE     ! test date to update rainfall
            INTEGER, SAVE :: NTIME     ! test time to update rainfall


      
            REAL             TSOI1,TSOI2        ! soil temperature
            REAL             LATI               ! Latitude
            REAL             LAI                ! LAI m2/m2
            REAL             RS                 ! Stomatal Resistance s/m
            REAL             WFPS_OPT           ! Optimum Water Filled Pore Space (ratio) for NO emissions
            REAL             WSAT               ! Col, Row soil porosity
            REAL             SM_5CM             ! Col, Row soil moisture at 5 cm m3/m3 from Oswald et al. 2013 SI https://doi.org/10.1126/science.1242266
            REAL             F_Temp, THETA, PREV_THETA, EMFac 
            REAL             F_Pulse, F_Theta, F_BIOG, F_AG
            REAL             EM_BIOG, EM_AG
            REAL             AGEMIS, BIOGEMIS

            LOGICAL, SAVE :: FIRSTIME = .TRUE.
            LOGICAL, SAVE :: WRSOIL   = .TRUE.
            CHARACTER( 256 ) :: MESG            ! message buffer
            CHARACTER( 16 )  :: PNAME = 'GET_SEGA_EMIS'  ! procedure name
            REAL             :: UNIT_CONV          ! converts emissions to mol/s

            INTEGER      GXOFF, GYOFF           ! global origin offset from file
C for INTERPX
            INTEGER       :: STRTCOLSIN, ENDCOLSIN, STRTROWSIN, ENDROWSIN  ! SOILINP

            CHARACTER( 16 ), SAVE :: VNAME_RC, VNAME_RN
            INTEGER :: SPC
C-----------------------------------------------------------------------

            IF ( FIRSTIME ) THEN
               FIRSTIME = .FALSE.
C Determine last timestamp
               EDATE = STDATE; ETIME = STTIME
               CALL NEXTIME( EDATE, ETIME, RUNLEN )   ! end date & time

#ifndef mpas
                IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
                   DX1 = DG2M * XCELL_GD ! in m.
                   DX2 = DG2M * YCELL_GD
     &                 * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( GL_NROWS/2 )))! in m.
                ELSE
                   DX1 = XCELL_GD        ! in m.
                   DX2 = YCELL_GD        ! in m.
                END IF
                GRID_AREA = DX1 * DX2 * 1.0e-6                     ! km2
#endif

C If initial run, initialize some variables, otherwise get them from file
               IF ( NEW_START ) THEN
!                  write(logdev,*) 'PFACTOR X:', size(PFACTOR,1),', Y:', size(PFACTOR,2)
                  PFACTOR      = 1.0    ! array
                  DRYPERIOD    = 0.001   ! to avoid log(0)
                  FREEZEPERIOD = 0.001   ! to avoid log(0)
                  THETAP       = Min( MET_DATA%SOIM1 / Tile_Data%WSAT, 1.0 )   ! array
                  F_Pulse      = 1.0
               END IF   ! initial run
            END IF   ! FIRSTIME

            N_LU = Tile_Data%n_lufrac

            CALL GET_STAGE_MET ( JDATE, JTIME, TSTEP( 2 ) ) 

            DO R = 1, NROWS
               DO C = 1, NCOLS 
C Calculate NO emissions by going thru temperature cases
                  SOILCAT = GRID_DATA%SLTYP( C,R )
                  TSOI1   = min(MET_DATA%SOIT1( C,R ),322.15)  
                  TSOI2   = Tile_Data%SOIT2(C,R)     
                  WSAT    = Tile_Data%WSAT( C,R )
                  SM_5CM  = MET_DATA%SOIM1( c,r ) * exp( 0.04 * GRAV )**(1.0/Tile_Data%BSLP( c,r ))
                  THETA   = min(SM_5CM / WSAT,1.0)
                  LATI    = Grid_Data%LAT( C,R )
                  LAI     = MET_DATA%LAI(C,R)
                  RS      = Met_Data%RS(C,R)
                  IF( Tile_Data%LUFRAC( c,r,i_water ) .LT. 1.0 ) THEN
                     F_Pulse = PULSING( L_DESID_DIAG, THETA, TSTEP, THETAP( C,R ), PFACTOR( C,R ),
     &                                  DRYPERIOD( C,R ), FREEZEPERIOD( C,R ), zsoil1, WSAT, TSOI1 )
                     IF( TSOI1 .LE. STDTEMP ) THEN
                        BIOGEMIS = 0.0
                        AGEMIS   = 0.0
                        VDEMIS_SG( :,C,R) = 0.0
                     Else 
! Stark 1996 https://doi.org/10.1007/BF02183035 normalized to 303.15 C follwing Medinets et al 2015 http://dx.doi.org/10.1016/j.soilbio.2014.09.025
! to limit no production under high temperatures
                        F_Temp = 228.3 * exp(-79000.0/RGASUNIV*(1.0/TSOI1-1.0/303.15)) - ! Increase NO/HONO production rate due to reduced energy barrier
     &                           206.5 * exp(-83200.0/RGASUNIV*(1.0/TSOI1-1.0/303.15))   ! Decrease due to enzyme denaturation and other distructive processes
                        DO NSPC = 1, NSGSPC       
C Generalized Poisson density function from Stark 1996 https://doi.org/10.1007/BF02183035 with a value of 0 at
C 0 available water content and a maximum at 0.3. The Shape factors c = 1 and d = 4 were developed to match the 
C observations of Oikawa et al. 2015 DOI: 10.1038/ncomms9753 Fig 1b and match the limits of Oswald et al 2013 
C https://doi.org/10.1126/science.1242266 figure 4. 
                           if ( BIOGEMIS_BEIS .and. USE_SEGA_N_EF ) Then
                              F_BIOG    = NONAGNO( c,r )
                              F_AG      = GROWAGNO( c,r ) * FERT_ADJ( JDATE, LATI, TSOI2 ) +  NGROWAGNO( c,r )
                              UNIT_CONV = 1.0 / (SG_MW(NSPC) * 3600.0)
                           else
                              F_BIOG = EF_BIOG( i_ag, N_LU, Tile_Data%LUFRAC( c,r,: ), EF_LU )
                              F_AG   = EF_AG(   i_ag, Tile_Data%LUFRAC( c,r,: ), EF_LU, JDATE, LATI, TSOI2 )
                              UNIT_CONV =  GRID_AREA / (SG_MW(NSPC) * 3600.0 * msfx2(c,r))
                           end if
                           EM_BIOG  = EM_ADJ(NSPC) * F_BIOG
                           EM_AG    = EM_ADJ(NSPC) * F_AG
                           F_Theta  = THETA/OPT_THETA(NSPC)*exp(0.25*(1.0-(THETA/OPT_THETA(NSPC))**4.0))
                           BIOGEMIS = CRF( LAI,RS ) * F_Pulse * F_Temp * F_Theta * EM_BIOG
                           AGEMIS   = CRF( LAI,RS ) * F_Pulse * F_Temp * F_Theta * EM_AG
                           VDEMIS_SG( NSPC,C,R) = ( BIOGEMIS + AGEMIS ) * UNIT_CONV
                        END DO 
                     END IF
                  ELSE ! Water
                     BIOGEMIS = 0.0
                     AGEMIS   = 0.0
                     VDEMIS_SG( :,C,R) = 0.0
                  END IF
                  THETAP( C,R ) = min(THETA,THETAP( C,R ))
               END DO  ! columns
            END DO  ! rows

            IF ( SECSDIFF( JDATE,JTIME, EDATE,ETIME ) .GT. TIME2SEC( TSTEP( 2 ) ) .OR. L_DESID_DIAG ) RETURN

C Create rain data file for soil NO

C Final timestamp
            NDATE = EDATE; NTIME = ETIME
C Build description for, and create/open soil NO emissions output file
            FTYPE3D = GRDDED3
            SDATE3D = NDATE
            STIME3D = NTIME
            TSTEP3D = 0   ! make it a time-independent file
            NCOLS3D = GL_NCOLS
            NROWS3D = GL_NROWS
            NLAYS3D = 1
            NVARS3D = 4
            MXREC3D = 1
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
            DO L = 1, NLAYS3D + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
            GDNAM3D = GRID_NAME  ! from HGRD_DEFN

            VNAME3D = ' '
            VNAME3D( 1 ) = 'PFACTOR'
            VNAME3D( 2 ) = 'DRYPERIOD'
            VNAME3D( 3 ) = 'FREEZEPERIOD'
            VNAME3D( 4 ) = 'THETAP'

            UNITS3D = ' '
            UNITS3D( 1 ) = 'Ratio'
            UNITS3D( 2 ) = 'h'
            UNITS3D( 3 ) = 'h'
            UNITS3D( 4 ) = 'Ratio'

            VDESC3D( 1 ) = 'Magnitude of soil moisture pulsing'
            VDESC3D( 2 ) = 'Hours of soil available water content less than 0.3'
            VDESC3D( 3 ) = 'Hours of freezing soil temperature'
            VDESC3D( 4 ) = 'Available soil water content'

            VTYPE3D = 0
            VTYPE3D( 1 ) = M3REAL
            VTYPE3D( 2 ) = M3REAL
            VTYPE3D( 3 ) = M3REAL
            VTYPE3D( 4 ) = M3REAL

            FDESC3D = ' '
            FDESC3D( 1 ) = 'Soil Moisture data for soil NO emissions'
            FDESC3D( 2 ) = '/From/ ' // PNAME
            FDESC3D( 3 ) = '/Version/ CMAQ'

C Open NO rain data save file
            IF ( IO_PE_INCLUSIVE ) THEN
               IF ( .NOT. OPEN3( SOILOUT, FSNEW3, PNAME ) ) THEN
                  MESG = 'Could not open "' // TRIM( SOILOUT ) // '" file'
                  CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT1 )
               END IF
            END IF

#ifdef parallel_io
            IF ( IO_PE_INCLUSIVE ) THEN
               IF ( .NOT. FLUSH3 ( SOILOUT ) ) THEN
                  MESG = 'Could not sync to disk ' // TRIM( SOILOUT )
                  CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
               END IF
            END IF
            CALL SE_BARRIER
            IF ( .NOT. IO_PE_INCLUSIVE ) THEN
               IF ( .NOT. OPEN3( SOILOUT, FSREAD3, PNAME ) ) THEN
                  MESG = 'Could not open ' // TRIM( SOILOUT )
                  CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
               END IF
            END IF
#endif
!      End If
C Write soil NO rain data file

            VAR = 'PFACTOR'
            IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, PFACTOR ) ) THEN
               MESG = 'Could not write "' // TRIM( VAR ) //
     &                '" to file "' // TRIM( SOILOUT ) // '"'
               CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
            END IF

            VAR = 'DRYPERIOD'
            IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, DRYPERIOD ) ) THEN
               MESG = 'Could not write "' // TRIM( VAR ) //
     &                '" to file "' // TRIM( SOILOUT ) // '"'
               CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
            END IF

            VAR = 'FREEZEPERIOD'
            IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, FREEZEPERIOD ) ) THEN
               MESG = 'Could not write "' // TRIM( VAR ) //
     &                '" to file "' // TRIM( SOILOUT ) // '"'
               CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
            END IF

            VAR = 'THETAP'
            IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, THETAP ) ) THEN
               MESG = 'Could not write "' // TRIM( VAR ) //
     &                '" to file "' // TRIM( SOILOUT ) // '"'
               CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
            END IF

            RETURN

         END SUBROUTINE GET_SEGA_EMIS

         REAL FUNCTION PULSING( L_DESID_DIAG, THETA, TSTEP, THETAPREV, PFACTOR, 
     &                          DRYPERIOD, FREEZEPERIOD, zsoil1, WSAT, TS )!_____
! !DESCRIPTION: Function PULSING calculates the increase (or "pulse") of
!  soil NOx emission that happens after preciptiation falls on dry soil.
!  According to  Yan et al., [2005] , this pulsing process is thought to
!  be due to a release of inorganic nitrogen trapped on top of the dry soil
!  and a subsequent reactivation of water-stressed bacteria, which then
!  metabolize the excess nitrogen. This can happen in seasonally dry
!  grasslands and savannahs or over freshly fertilized fields.
!  Soil NOx emissions consist of baseline emissions plus discrete "pulsing"
!  episodes.  We follow the Yan et al., [2005] algorithm, where the pulse
!  (relative to the flux pre wetting) is determined by the antecedent dry
!  period, with a simple logarithmic relationship,
!
!  PFACTOR = 13.01 ln ( DRYPERIOD ) -  53.6
!
!  ,where PFACTOR is the magnitude of peak flux relative to prewetting flux,
!  and DRYPERIOD  is the length of the antecedent dry period in hours.
!
!  The pulse decays with
!
!  PFACTOR = PFACTOR * EXP( -0.068d0 * TSTEP(HOURS) )

         IMPLICIT NONE

         INTEGER, EXTERNAL       ::   TIME2SEC

C Function arguments:
         LOGICAL, INTENT( IN )    :: L_DESID_DIAG
         INTEGER, INTENT( IN )    :: TSTEP( 3 )        ! time step vector (HHMMSS)
         REAL,    INTENT( IN )    :: THETA
         REAL,    INTENT( IN )    :: zsoil1
         REAL,    INTENT( IN )    :: WSAT
         REAL,    INTENT( IN )    :: TS
         REAL,    INTENT( INOUT ) :: THETAPREV    
         REAL,    INTENT( INOUT ) :: DRYPERIOD
         REAL,    INTENT( INOUT ) :: FREEZEPERIOD
         REAL,    INTENT( INOUT ) :: PFACTOR
C Local Variables
         REAL MOISTDIFF
         REAL DTHOURS

         DTHOURS = FLOAT(TIME2SEC(TSTEP(2)))/3600.0
         IF (L_DESID_DIAG) then 
            DTHOURS = 0.
         END IF

         ! If soil moisture less than 0.3 and no pulse is taking place
         IF ( THETAPREV .lt. 0.30 .and. PFACTOR .eq. 1.0) THEN

            ! Get change in soil moisture since previous timestep
            MOISTDIFF = ( THETA - THETAPREV )

            ! If change in soil moisture is > 0.01 (rains) 
            ! Yan et al. (2005; doi:10.1029/2004GB002276) developed the parameterization (employed in Hudman et al. 2012) 
            ! based on a 0.07m soil layer depth and a 0.05 change in the volumetric soil water content. This adjusts the 
            ! threshold based on the LSM soil layer 1 depth assuming a bucket model (Van Den Broeke et al. 2018 https://doi.org/10.1007/s00704-017-2301-8)
            IF ( MOISTDIFF .gt. 0.005 / WSAT * 0.07 / zsoil1 .and. TS .gt. STDTEMP ) THEN

               !Initialize new pulse factor (dry period hours)
               PFACTOR = max(13.01 * LOG( max(DRYPERIOD,FREEZEPERIOD) ) - 53.6, 1.0)

               ! Reinitialize dry period
               DRYPERIOD    = 0.001
               FREEZEPERIOD = 0.001
               ! Reinitialize soil moisture
               THETAPREV    = THETA

               ! If no rain (i.e.,  change in soil moisture is < 0.01)
            ELSEIF ( TS .gt. STDTEMP ) Then
               ! Add one timestep to dry period
               DRYPERIOD    = DRYPERIOD + DTHOURS
               FREEZEPERIOD = 0.001
            ELSE
               DRYPERIOD    = DRYPERIOD    + DTHOURS
               FREEZEPERIOD = FREEZEPERIOD + DTHOURS              
            ENDIF

         ! Frost Pulsing
         ELSEIF ( TS .gt. STDTEMP .and. PFACTOR .eq. 1.0 ) THEN
            !Initialize new pulse factor (dry period hours)
            PFACTOR = max(13.01 * LOG( FREEZEPERIOD ) - 53.6, 1.0)            
            ! Reinitialize dry period
            if( THETA .Gt. 0.3 ) Then
               DRYPERIOD    = 0.001
            Else 
               DRYPERIOD    = DRYPERIOD    + DTHOURS
            End If
            FREEZEPERIOD = 0.001
         ! If box is already pulsing , then decay pulse one timestep
         ELSEIF ( PFACTOR .gt. 1.0 ) THEN

            ! Decay pulse
            PFACTOR   = max(PFACTOR * EXP( -0.068 * DTHOURS ), 1.0)

            ! Update dry period
            IF ( THETA .lt. 0.30 .and. TS .le. STDTEMP )Then
               DRYPERIOD    = DRYPERIOD    + DTHOURS
               FREEZEPERIOD = FREEZEPERIOD + DTHOURS              
            ElseIf ( THETA .lt. 0.30 .and. TS .gt. STDTEMP ) Then
               DRYPERIOD    = DRYPERIOD    + DTHOURS
               FREEZEPERIOD = 0.001
            ElseIf ( THETA .ge. 0.30 .and. TS .le. STDTEMP ) Then
               DRYPERIOD    = 0.001
               FREEZEPERIOD = FREEZEPERIOD + DTHOURS              
            Else
               DRYPERIOD    = 0.001   
               FREEZEPERIOD = 0.001            
            End If
         ELSE ! PFACTOR .eq. 1 and THETA .ge. 0.30
            PFACTOR      = 1.0 
            DRYPERIOD    = 0.001
            If( TS .le. STDTEMP ) Then
               FREEZEPERIOD = FREEZEPERIOD + DTHOURS              
            Else   
               FREEZEPERIOD = 0.001            
            End If
         ENDIF
         PULSING = max(PFACTOR,1.0)
         RETURN

         END FUNCTION PULSING!_____

         REAL FUNCTION DAY_LENGTH( DOY, LAT )
C Computes the day length as a function of day of the year and lattitude

         IMPLICIT NONE
         
C Function arguments:
         REAL, INTENT( IN ) :: DOY
         REAL, INTENT( IN ) :: LAT

! Day length function
         Real            :: hourang              ! noon hour solar angle
         Real            :: delang               ! solar declanation angle
         Real            :: daylen               ! day length (h)
         Real, Parameter :: D_OFF  = 172.0       ! Summer Solstice in northern hemisphere
         Real, Parameter :: ANGODY = 360.0/365.0 !* PIO180 

C-----------------------------------------------------------------------
         delang       = PI180 * min(max(23.45 * COS( ANGODY * PI180* ( DOY - D_OFF )),-89.0),89.0)
         hourang      = min(max(-tan( delang ) * tan( PI180 * LAT ),-0.99),0.99)
         DAY_LENGTH   = 2.0/15.0 * acos(hourang)/PI180
         return

         END FUNCTION DAY_LENGTH

         REAL FUNCTION FERT_ADJ( DATE, LAT, TS )

C Compute a fertilizer adjustment factor for the given date in yyyyddd format.
C If it is not growing season, the adjustment factor is 0; otherwise, it
C ranges from 0.0 to 1.0.

         IMPLICIT NONE
         
C Function arguments:
         INTEGER, INTENT( IN ) :: DATE
         REAL, INTENT( IN )    :: LAT
         REAL, INTENT( IN )    :: TS

C Local variables:
         Real     DOY       ! Day of the year (max 365)
         Real     DOY_YEST  ! DOY for yesterday
! Day length function
         Real            :: ddldt                ! d(day length)/dt (h/day)
         Real            :: Fdl                  ! Day length function
         Real            :: SDDL                 ! Day length Sandard Deviation (day)
! Temperature function
         Real, Parameter :: Tmin  = 283.15 ! Soil Temerature of fertilzation peak
         Real, Parameter :: SDT   = 5.0    ! Temperature standard deviation K (change from 8 to 6 for sensitivity)
         Real, Parameter :: ExdDl = -4.0   ! Asymtotic exponent for day length difference function (day/h)
         Real            :: FTs            ! Day length function
         Real            :: FMIN           ! Minimum normalized soil nutrient status


C-----------------------------------------------------------------------

         DOY      = FLOAT( MOD( DATE, 1000 ) )
         if(DOY .eq. 1.0 ) Then
            DOY_YEST = 365.0
         else 
            DOY_YEST = DOY - 1.0
         end if
         ! very near the equator the denominator for the calculation of ddldt
         ! becomes zero, so avoid division by zero for that case
         if( abs( DAY_LENGTH(81.0, LAT) - DAY_LENGTH(80.0, LAT) ) .lt. 1e-30 ) Then
            ddldt = 0
         else
            ddldt    = ( DAY_LENGTH(DOY, LAT)  - DAY_LENGTH(DOY_YEST, LAT) ) /
     &              abs( DAY_LENGTH(81.0, LAT) - DAY_LENGTH(80.0, LAT) )
         endif
         SDDL     = min(0.9,(115-abs(LAT))/100)
         Fdl      = exp(-(ddldt-1.0)**2/(2.0 * SDDL**2)) 
         FTs      = exp(-0.5*((TS-TMin)/SDT)**2)
         FERT_ADJ = Fdl*FTs
         If( abs(LAT) > 23.4 ) Then
            FMIN  = 1.17/abs(LAT)
         Else 
            FMIN  = 0.05
         End If
         if(Ts .Gt. 283.15) FERT_ADJ = max(FERT_ADJ, FMIN)

         RETURN
         
         END FUNCTION FERT_ADJ

C-----------------------------------------------------------------------

         REAL FUNCTION CRF( LAI,RS )

C Compute a vegetation adjustment factor for the given date in yyyyddd format.
C The adjustment factor ranges from 0.5 to 1.0.

         IMPLICIT NONE
         
C Function arguments:
         REAL, INTENT( IN ) :: LAI
         REAL, INTENT( IN ) :: RS

C-----------------------------------------------------------------------

C based on a 10 C germination temperature for 5 cm soil depth reported multiple agricultural extension offices, 
C e.g. https://www.agry.purdue.edu/ext/corn/news/timeless/Emergence.html. Using an asymptotic function

         If( LAI .Gt. 0.01 .And. RS .Gt. 0.0 ) Then
            CRF = 1.0 / ( 1.0 + exp( 0.007 * ( 127.0 - RS ) ) )
         Else 
            CRF = 1.0
         End If
         CRF = min(CRF, 0.4)

         RETURN
      
         END FUNCTION CRF
         
C-----------------------------------------------------------------------
         REAL FUNCTION EF_BIOG( i_ag, n_lu, frac_lu, ef_lu )

C Compute a vegetation adjustment factor for the given date in yyyyddd format.
C The adjustment factor ranges from 0.5 to 1.0.

         IMPLICIT NONE
         
C Function arguments:
         INTEGER, INTENT( IN ) :: i_ag
         INTEGER, INTENT( IN ) :: n_lu
         REAL,    INTENT( IN ) :: frac_lu(:)
         REAL,    INTENT( IN ) :: ef_lu(:)
         INTEGER               :: i

C-----------------------------------------------------------------------
         EF_BIOG = 0.0
         DO i = 1, n_lu
            If(i .EQ. i_ag) Then
               EF_BIOG = EF_BIOG + 0.0
            Else
               EF_BIOG = EF_BIOG + ef_lu( i ) * frac_lu( i ) 
            End IF
         End Do
         RETURN
      
         END FUNCTION EF_BIOG

C-----------------------------------------------------------------------
         REAL FUNCTION EF_AG( i_ag, frac_lu, ef_lu, DATE, LAT, TS )

C Compute a vegetation adjustment factor for the given date in yyyyddd format.
C The adjustment factor ranges from 0.5 to 1.0.

         IMPLICIT NONE
         
C Function arguments:
         INTEGER, INTENT( IN ) :: i_ag
         REAL,    INTENT( IN ) :: frac_lu(:)
         REAL,    INTENT( IN ) :: ef_lu(:)
         INTEGER, INTENT( IN ) :: DATE
         REAL,    INTENT( IN ) :: LAT
         REAL,    INTENT( IN ) :: TS
! Fertilizer EF matched to the FERT_ADJ seasonality and global NO emission total of 9.5 Tg to match Weng et al. 2020
! based an annual 2018 MPAS simulation
         REAL, PARAMETER       :: EF_FERT = 44.0  ! gN/(km2 hr) Mineral fertilzier
         REAL, PARAMETER       :: EF_AGO  =  3.13 ! gN/(km2 hr) Global Ag mean from Steinkamp and Lawrence 2011 www.atmos-chem-phys.net/11/6063/2011/

C-----------------------------------------------------------------------
         EF_AG = frac_lu(i_ag) * ( EF_AGO + FERT_ADJ( DATE, LAT, TS )*EF_FERT )

         RETURN
      
         END FUNCTION EF_AG


      END MODULE SEGA_MOD
