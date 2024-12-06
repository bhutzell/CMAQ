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

C*************************************************************************
C
C  MODULE:  defines a set of species variables
C
C*************************************************************************
      MODULE SPECIES

      CHARACTER(LEN=16) :: SPECVARS(2048) = ''
      INTEGER        :: NSPECVAR       = 0
 
      CONTAINS
 
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C   ROUTINE TO GET SPECIES DEFINITIONS FROM ENVIRONMENT VARIABLES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         SUBROUTINE LoadSpecies

         USE M3UTILIO
         USE GET_ENV_VARS
 
         IMPLICIT NONE

         ! local variables
         Integer k, n
         Integer status
         CHARACTER(LEN=16)  varname
         CHARACTER(LEN=16)  field   

         NSPECVAR = 0
         CALL GET_ENVLIST ( 'SPECIES', NSPECVAR, SPECVARS, STATUS )
         IF ( STATUS .EQ. 0 ) THEN
            IF ( TRIM( SPECVARS( 1 ) ) .EQ. 'ALL' ) THEN ! clear SPECVARS and set VNAME3D
               SPECVARS = ''
               NSPECVAR = NVARS3D
               DO N = 1,NVARS3D
                  SPECVARS( N ) = VNAME3D( N )
               END DO 
            END IF
         ELSE
            WRITE(6,*)'Environment Variable SPECIES not found'
            STOP
         END IF
         RETURN
          
         END SUBROUTINE LOADSPECIES
 
      END MODULE SPECIES

