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
      MODULE STAGE_OUTPUT

C-----------------------------------------------------------------------
C Function: Collection of output routines for STAGE deposition velocities
C           and dry deposition
C-----------------------------------------------------------------------
      USE RUNTIME_VARS, Only: MOSAIC
    
      Contains 
         Pure Function Write_time( WSTEP, TSTEP ) Result( Write_now ) 

         Implicit None

         Integer, Intent( IN ) :: WSTEP
         Integer, Intent( IN ) :: TSTEP
         Logical               :: Write_now     
        
         If( WSTEP .Ge. TSTEP ) Then
            Write_now = .TRUE.
         Else 
            Write_now = .FALSE.
         End If

         End Function Write_time

         Subroutine Open_depv( DATE, TIME, TSTEP )

         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE CGRID_SPCS          ! CGRID mechanism species
         USE STAGE_DATA
         USE UTILIO_DEFN
         USE MOSAIC_MOD, Only: Tile_Data
         USE MIO_ASCII 

         IMPLICIT NONE
 
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

         INTEGER, INTENT( IN ) :: DATE    ! current model date, coded YYYYDDD
         INTEGER, INTENT( IN ) :: TIME    ! current model time, coded HHMMSS
         INTEGER, INTENT( IN ) :: TSTEP   ! output time step

C Local variables:

         CHARACTER( 16 ) :: PNAME = 'OPSTAGE_DEPV'
         CHARACTER( 16 ) :: VNAME            ! scratch string
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER      N_Vars
         INTEGER      V, N, L     ! loop induction variables

            NVARS3D = Tile_Data%n_Vd

            N = 0
            DO V = 1, N_SPC_DEPV
               If( Tile_Data%Vd_Out( V ) ) Then

                  N = N + 1 
                  VNAME = trim( Tile_Data%Vd_Name( V ) )
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = VNAME
                  UNITS3D( N ) = 'cm s-1'
                  VDESC3D( N ) = 'Dry deposition velocity'

               End If
            END DO

            ! Store MIO Metadata
            NDIMS3D( 1:NVARS3D ) = 4
            L_TSTEP( 1:NVARS3D ) = .True.
            L_LAY  ( 1:NVARS3D ) = .True. ! even if 2D
            L_COL  ( 1:NVARS3D ) = .True.
            L_ROW  ( 1:NVARS3D ) = .True.
            L_VEXT ( 1:NVARS3D ) = .False.

            CALL LOAD_MIO_FILE ( 'CTM_DEPV_DIAG', 0, 0, 0,
     &           VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &           VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &           L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),
     &           L_VEXT(1:NVARS3D) )


         IF ( MOSAIC ) THEN

            NVARS3D = Tile_Data%n_Vd + 7
            NLAYS3D = Tile_Data%N_LUFRAC   ! preempting normal use of vertical layers

            N = 0
            N = N + 1 ! 1
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'RA'
            UNITS3D( N ) = 's m-1'
            VDESC3D( N ) = 'aerodynamic resistance for land use category'

            N = N + 1 ! 2
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'LUFRAC'
            UNITS3D( N ) = 'dimensionless'
            VDESC3D( N ) = 'Fractional land use'

            N = N + 1 ! 3
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'LAI'
            UNITS3D( N ) = 'dimensionless'
            VDESC3D( N ) = 'leaf area index for land use category'

            N = N + 1 ! 4
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'USTAR'
            UNITS3D( N ) = 'm s-1'
            VDESC3D( N ) = 'friction velocity for land use category'

            N = N + 1 ! 5
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'Z0'
            UNITS3D( N ) = 'm'
            VDESC3D( N ) = 'surface roughness for land use category'

            N = N + 1 ! 6
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'RST'
            UNITS3D( N ) = 's m-1'
            VDESC3D( N ) = 'Stomatal resistance to water vapor'

            N = N + 1 ! 7
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'VEG'
            UNITS3D( N ) = 'ratio'
            VDESC3D( N ) = 'Vegetation coverage'

            DO V = 1, N_SPC_DEPV
               If( Tile_Data%Vd_Out( V ) ) Then

                  N = N + 1 
                  VNAME = trim( Tile_Data%Vd_Name( V ) )
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = VNAME
                  UNITS3D( N ) = 'cm s-1'
                  VDESC3D( N ) = 'Dry deposition velocity'

               End If
            END DO
 
            ! Store MIO Metadata
            NDIMS3D( 1:NVARS3D ) = 4
            L_TSTEP( 1:NVARS3D ) = .True.
            L_LAY  ( 1:NVARS3D ) = .True.
            L_COL  ( 1:NVARS3D ) = .True.
            L_ROW  ( 1:NVARS3D ) = .True.
            L_VEXT ( 1:NVARS3D ) = .False.

            CALL LOAD_MIO_FILE ( 'CTM_DEPV_MOS', 0, 0, 0,
     &           VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &           VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &           L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),
     &           L_VEXT(1:NVARS3D) )


         END IF
         Return
         End Subroutine Open_depv

         Subroutine Write_depv( DATE, TIME )

         USE CGRID_SPCS, ONLY : N_SPC_DEPV
         USE HGRD_DEFN
         USE UTILIO_DEFN
         USE MOSAIC_MOD,   Only: Tile_Data
         USE ASX_DATA_MOD, Only: MOSAIC_DATA
         use mio_module

         Implicit None    

         Integer, Intent( IN ) :: DATE
         Integer, Intent( IN ) :: TIME

         Integer               :: N

         CHARACTER( 16 )  :: PNAME = 'WRITE_DEPV'
         CHARACTER( 120 ) :: XMSG = ' '
         character( 20 )  :: time_stamp
         logical, save    :: firstime = .true.
         
         INCLUDE SUBST_FILES_ID  ! file name parameters

         if ( firstime ) then
            firstime = .false.
            call mio_setfile( INIT_CONC_1 )
            mio_nlays = 1
            call mio_fcreate( CTM_DEPV_DIAG, mio_new_file )
            if (mype .eq. 0) then
               call mio_set_global_attr( CTM_DEPV_DIAG, 'NLAYS', 1 )
               call mio_set_global_attr( CTM_DEPV_DIAG, 'VGLVLS', INIT_CONC_1, 1, 2 )
            end if
            if ( MOSAIC ) then
               call mio_fcreate( CTM_DEPV_MOS, mio_new_file )
               if (mype .eq. 0) then
                  call mio_set_global_attr( CTM_DEPV_DIAG, 'NLAYS', 1 )
                  call mio_set_global_attr( CTM_DEPV_DIAG, 'VGLVLS', INIT_CONC_1, 1, 2 )
               end if
            end if
         end if

         call mio_time_format_conversion( date, time, time_stamp )

         DO N = 1, N_SPC_DEPV
            If( Tile_Data%Vd_Out( N ) ) Then
               call mio_fwrite( CTM_DEPV_DIAG, Tile_Data%Vd_Name( N ),
     &                          PNAME, Tile_Data%Grd_Vd(:,:,N)*100.0, TIME_STAMP )
            End If
         END DO

         IF ( MOSAIC ) THEN


!-----------------------------------------------------------------------------------------!
!                               STAGE Diagnostic Output                                   !
!-----------------------------------------------------------------------------------------!

               call mio_fwrite( CTM_DEPV_MOS, 'LUFRAC', PNAME, 
     &                          Tile_Data%LUFRAC, TIME_STAMP )

               call mio_fwrite( CTM_DEPV_MOS, 'RA', PNAME, 
     &                          MOSAIC_DATA%RA, TIME_STAMP )

               call mio_fwrite( CTM_DEPV_MOS, 'RST', PNAME, 
     &                          Mosaic_Data%RSTW, TIME_STAMP )

               call mio_fwrite( CTM_DEPV_MOS, 'LAI', PNAME, 
     &                          MOSAIC_Data%LAI, TIME_STAMP )

               call mio_fwrite( CTM_DEPV_MOS, 'USTAR', PNAME, 
     &                          MOSAIC_Data%USTAR, TIME_STAMP )

               call mio_fwrite( CTM_DEPV_MOS, 'Z0', PNAME, 
     &                          MOSAIC_Data%Z0, TIME_STAMP )

               call mio_fwrite( CTM_DEPV_MOS, 'VEG', PNAME, 
     &                          MOSAIC_Data%VEG, TIME_STAMP )

               DO N = 1, N_SPC_DEPV
                  If( Tile_Data%Vd_Out( N ) ) Then
                     call mio_fwrite( CTM_DEPV_MOS, Tile_Data%Vd_Name( N ), 
     &                                 PNAME, Tile_Data%Lu_Vd(:,:,N,:)*100.0, TIME_STAMP )
                  End If
               END DO
         End If

         End Subroutine Write_depv

      End Module STAGE_OUTPUT
      
