
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
      MODULE STAGE_EMIS_MAP

C-----------------------------------------------------------------------
C Function: index mapping between CGRID, diffusion, dry dep, and emissions
C           species

C Module depends on prior call to DEPV_INIT to get NDEPV_SPC

C Revision History:
C   10 Apr 2008 J.Young: initial implementation using subroutine vdiff_map.F
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   11 May 11 J.Bash: Modified for NH3 bidi component fluxes
C   04 Jan 12 J.Young: Initalize char strings with blank padding

C-----------------------------------------------------------------------
      USE VDIFF_DATA

C component flux output variables 
      INTEGER,         SAVE :: N_BD_EMIS, NH3_E, HG_E, HONO_E, HCHO_E
      CHARACTER( 16 ), SAVE :: BD_EMIS_SPC( 4 ) = ' '

      CONTAINS

C-----------------------------------------------------------------------
         FUNCTION STAGE_EMIS_MAP_INIT( ) RESULT ( SUCCESS )

         USE CGRID_SPCS   ! CGRID species number and offsets
         USE UTILIO_DEFN

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_EMISPRM   ! Emissions processing control parameters

C Arguments:
         LOGICAL SUCCESS

C External Functions:
         INTEGER, EXTERNAL :: FINDEX

C Local variables:
         
         CHARACTER( 80 )  :: XMSG       ! Message text
         CHARACTER( 16 )  :: PNAME = 'STAGE_EMIS_MAP'

         INTEGER STRT, FINI
         INTEGER S, V, N, I, INDX
         INTEGER ALLOCSTAT

         LOGICAL LERROR                    ! Error flag

!-----------------------------------------------------------------------
         SUCCESS = .TRUE.

         NH3_E  = 0
         HG_E   = 0
         HONO_E = 0
         HCHO_E = 0
         IF ( ABFLUX ) THEN          
            NH3_E = 1
            N_BD_EMIS = NH3_E
            BD_EMIS_SPC( NH3_E ) = 'NH3_Emis'
         END IF
         IF ( HGBIDI ) THEN
            HG_E = NH3_E + 1 
            N_BD_EMIS = HG_E
            BD_EMIS_SPC( HG_E ) = 'HG_Emis'             
         END IF   
         IF ( SFC_HONO ) THEN
            HONO_E = max(NH3_E,HG_E) + 1   
            N_BD_EMIS = HONO_E
            BD_EMIS_SPC( HONO_E ) = 'HONO_Het '                      
         END IF    
         HCHO_E = max(NH3_E,HG_E,HONO_E) + 1   
         N_BD_EMIS = HCHO_E
         BD_EMIS_SPC( HCHO_E ) = 'HCHO_Emis '  

         RETURN

         END FUNCTION STAGE_EMIS_MAP_INIT

      END MODULE STAGE_EMIS_MAP
