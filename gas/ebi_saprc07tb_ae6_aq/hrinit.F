
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE HRINIT
C***********************************************************************
C
C  FUNCTION: To initialize species tolerances, arrays, and indices
C
C  PRECONDITIONS: For the SAPRC07TB family of mechanisms
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Aug 18, 2016
C   21 Jun 10 J.Young:   convert for Namelist redesign
C   18 Jul 14 B.Hutzell: revised to use the RXNS_DATA and RXNS_FUNCTION
C                        modules instead of include files
C***********************************************************************

      USE UTILIO_DEFN
      USE HRDATA
      USE RXNS_DATA
      USE RXNS_FUNCTION

      IMPLICIT NONE

C.....INCLUDES:
      Include SUBST_CONST          ! common constants

C.....ARGUMENTS: NONE

C.....PARAMETERS: NONE


C.....SAVED VARIABLES:
      CHARACTER( 16 ), SAVE  ::  PNAME = 'HRINIT'   ! Program name


C.....LOCAL VARIABLES:
      CHARACTER( 132 ) ::  MSG       ! Log message

      INTEGER N               ! Loop index
      INTEGER IOS             ! Allocate error status

C***********************************************************************

      N_SPEC = NUMB_MECH_SPC
      N_RXNS = NRXNS
      N_SPCL_RKS =   9
      ALLOCATE( RKI( NRXNS ),
     &          RKI_SAV( MAX_CELLS_EBI, NRXNS ),
     &          FORWARD_CONV( N_SPEC),
     &          REVERSE_CONV( N_SPEC),
     &          RXRAT( NRXNS ),
     &          RTOL( N_SPEC),
     &          YC(   N_SPEC),
     &          YC0(  N_SPEC),
     &          YCP(  N_SPEC),
     &          PROD( N_SPEC),
     &          LOSS( N_SPEC),
     &          STAT = IOS )

       IF ( IOS .NE. 0 ) THEN
          MSG = 'Error allocating RKI, RXRAT, RTOL, YC, YC0, YCP, FORWARD_CONV, '
     &       // 'REVERSE_CONV, PROD, LOSS, or PNEG'
          CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
       END IF

       ALLOCATE( ATMPRES( MAX_CELLS_EBI ),
     &           H2O    ( MAX_CELLS_EBI ),
     &           TEMP   ( MAX_CELLS_EBI ),
     &           HET    ( MAX_CELLS_EBI, NHETERO ),
     &           RJIN   ( MAX_CELLS_EBI, NPHOTAB ),
     &           LAND   ( MAX_CELLS_EBI ) ,
     &           STAT = IOS )

       IF ( IOS .NE. 0 ) THEN
          MSG = 'Error allocating ATMPRES, H2O, TEMP, HET, RJIN, LAND '
          CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
       END IF

       IF( NSPECIAL .GT. 0 )THEN
          ALLOCATE( SYC( MAX_CELLS_EBI, N_SPEC ), STAT = IOS )
          IF ( IOS .NE. 0 ) THEN
               MSG = 'Error allocating SYC'
               CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
          END IF
       END IF

       IF( .NOT. MAP_CHEMISTRY_SPECIES() )THEN
          MSG = 'Detected above error(s) when mapping Chemistry species from CGRID species'
          CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
       END IF

       DO N = 1, NUMB_MECH_SPC
          FORWARD_CONV( N ) = 1.0E-3 * MWAIR / SPECIES_MOLWT( N )
          REVERSE_CONV( N ) = 1.0E+3 / MWAIR * SPECIES_MOLWT( N )
       END DO

! initialize all reaction rates to zero so unused array elements are defined

       RXRAT = 0.0D0

! initialize all production and loss rates to zero so unused array elements are defined

       PROD  = 0.0D0
       LOSS  = 0.0D0

       NING1 = 4
       NING2 = 4



c..Allocate and define ebi species

      N_EBISP  = 181
      ALLOCATE( EBISP( N_EBISP ), STAT = IOS ) 
      IF ( IOS .NE. 0 ) THEN
         MSG = 'Error allocating EBISP' 
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      EBISP(   1 ) = AXYL1J
      EBISP(   2 ) = AXYL2J
      EBISP(   3 ) = ATOL1J
      EBISP(   4 ) = ATOL2J
      EBISP(   5 ) = ABNZ1J
      EBISP(   6 ) = ABNZ2J
      EBISP(   7 ) = ATRP1J
      EBISP(   8 ) = ATRP2J
      EBISP(   9 ) = AISO1J
      EBISP(  10 ) = AISO2J
      EBISP(  11 ) = ASQTJ
      EBISP(  12 ) = APAH1J
      EBISP(  13 ) = APAH2J
      EBISP(  14 ) = AALK1J
      EBISP(  15 ) = AALK2J
      EBISP(  16 ) = AISO3J
      EBISP(  17 ) = SO2
      EBISP(  18 ) = SULF
      EBISP(  19 ) = SULRXN
      EBISP(  20 ) = BACL
      EBISP(  21 ) = ISOPRXN
      EBISP(  22 ) = BENZENE
      EBISP(  23 ) = SOAALK
      EBISP(  24 ) = ALKRXN
      EBISP(  25 ) = CLACET
      EBISP(  26 ) = CLCHO
      EBISP(  27 ) = BNZNRXN
      EBISP(  28 ) = BNZHRXN
      EBISP(  29 ) = XYLNRXN
      EBISP(  30 ) = XYLHRXN
      EBISP(  31 ) = TOLNRXN
      EBISP(  32 ) = TOLHRXN
      EBISP(  33 ) = PAHNRXN
      EBISP(  34 ) = PAHHRXN
      EBISP(  35 ) = ACLI
      EBISP(  36 ) = ACLJ
      EBISP(  37 ) = ACLK
      EBISP(  38 ) = PCVOC
      EBISP(  39 ) = PCSOARXN
      EBISP(  40 ) = HO2H
      EBISP(  41 ) = PAN2
      EBISP(  42 ) = PBZN
      EBISP(  43 ) = MAPAN
      EBISP(  44 ) = TBUO
      EBISP(  45 ) = ISOPOOH
      EBISP(  46 ) = IEPOX
      EBISP(  47 ) = TOLUENE
      EBISP(  48 ) = MXYL
      EBISP(  49 ) = OXYL
      EBISP(  50 ) = PXYL
      EBISP(  51 ) = TMBENZ124
      EBISP(  52 ) = ETOH
      EBISP(  53 ) = ALK1
      EBISP(  54 ) = ALK2
      EBISP(  55 ) = ALK3
      EBISP(  56 ) = ALK4
      EBISP(  57 ) = ALK5
      EBISP(  58 ) = ARO1
      EBISP(  59 ) = NAPHTHAL
      EBISP(  60 ) = CLNO
      EBISP(  61 ) = CLONO
      EBISP(  62 ) = HOCL
      EBISP(  63 ) = H2NO3PK
      EBISP(  64 ) = VIVPO1
      EBISP(  65 ) = COOH
      EBISP(  66 ) = CCOOOH
      EBISP(  67 ) = NPHE
      EBISP(  68 ) = xTBUO
      EBISP(  69 ) = ROOH
      EBISP(  70 ) = R6OOH
      EBISP(  71 ) = RAOOH
      EBISP(  72 ) = yISOPOOH
      EBISP(  73 ) = ARO2MN
      EBISP(  74 ) = ACETYLENE
      EBISP(  75 ) = CL2
      EBISP(  76 ) = CLONO2
      EBISP(  77 ) = CLCCHO
      EBISP(  78 ) = HCHO_PRIMARY
      EBISP(  79 ) = CCHO_PRIMARY
      EBISP(  80 ) = H2NO3PIJ
      EBISP(  81 ) = BALD
      EBISP(  82 ) = BENZRO2
      EBISP(  83 ) = PAHRO2
      EBISP(  84 ) = SESQRXN
      EBISP(  85 ) = AOLGBJ
      EBISP(  86 ) = VSVPO3
      EBISP(  87 ) = RCOOOH
      EBISP(  88 ) = ETHENE
      EBISP(  89 ) = PROPENE
      EBISP(  90 ) = BUTADIENE13
      EBISP(  91 ) = ISOPRENE
      EBISP(  92 ) = APIN
      EBISP(  93 ) = TRPRXN
      EBISP(  94 ) = TOLRO2
      EBISP(  95 ) = OLE1
      EBISP(  96 ) = OLE2
      EBISP(  97 ) = TERP
      EBISP(  98 ) = SESQ
      EBISP(  99 ) = ACRO_PRIMARY
      EBISP( 100 ) = VLVPO1
      EBISP( 101 ) = VSVPO1
      EBISP( 102 ) = VSVPO2
      EBISP( 103 ) = VSVOO1
      EBISP( 104 ) = MEOH
      EBISP( 105 ) = RNO3
      EBISP( 106 ) = ACETONE
      EBISP( 107 ) = xMEO2
      EBISP( 108 ) = HCOCO3
      EBISP( 109 ) = xACROLEIN
      EBISP( 110 ) = xCLACET
      EBISP( 111 ) = VSVOO2
      EBISP( 112 ) = VSVOO3
      EBISP( 113 ) = xOH
      EBISP( 114 ) = AFG1
      EBISP( 115 ) = AFG2
      EBISP( 116 ) = ACROLEIN
      EBISP( 117 ) = yRAOOH
      EBISP( 118 ) = IEPOXOO
      EBISP( 119 ) = CLNO2
      EBISP( 120 ) = CLO
      EBISP( 121 ) = CCOOH
      EBISP( 122 ) = xMACO3
      EBISP( 123 ) = IPRD
      EBISP( 124 ) = XYLRO2
      EBISP( 125 ) = MACR
      EBISP( 126 ) = MVK
      EBISP( 127 ) = xAFG3
      EBISP( 128 ) = AOLGAJ
      EBISP( 129 ) = xNO2
      EBISP( 130 ) = xHOCCHO
      EBISP( 131 ) = HOCCHO
      EBISP( 132 ) = xBALD
      EBISP( 133 ) = xMACR
      EBISP( 134 ) = xCL
      EBISP( 135 ) = VLVOO1
      EBISP( 136 ) = VLVOO2
      EBISP( 137 ) = RCOOH
      EBISP( 138 ) = xMVK
      EBISP( 139 ) = CCHO
      EBISP( 140 ) = xBACL
      EBISP( 141 ) = AFG3
      EBISP( 142 ) = xCLCCHO
      EBISP( 143 ) = xRNO3
      EBISP( 144 ) = BZCO3
      EBISP( 145 ) = CRES
      EBISP( 146 ) = xIPRD
      EBISP( 147 ) = xACETONE
      EBISP( 148 ) = BZO
      EBISP( 149 ) = xMECO3
      EBISP( 150 ) = xRCO3
      EBISP( 151 ) = xPROD2
      EBISP( 152 ) = HNO3
      EBISP( 153 ) = xCO
      EBISP( 154 ) = MGLY
      EBISP( 155 ) = xMEK
      EBISP( 156 ) = RCHO
      EBISP( 157 ) = xAFG1
      EBISP( 158 ) = xAFG2
      EBISP( 159 ) = GLY
      EBISP( 160 ) = RCO3
      EBISP( 161 ) = MACO3
      EBISP( 162 ) = MEK
      EBISP( 163 ) = HCOOH
      EBISP( 164 ) = xGLY
      EBISP( 165 ) = xMGLY
      EBISP( 166 ) = PRD2
      EBISP( 167 ) = xRCHO
      EBISP( 168 ) = MEO2
      EBISP( 169 ) = xCCHO
      EBISP( 170 ) = yR6OOH
      EBISP( 171 ) = xHCHO
      EBISP( 172 ) = HCHO
      EBISP( 173 ) = HCL
      EBISP( 174 ) = CO
      EBISP( 175 ) = yROOH
      EBISP( 176 ) = zRNO3
      EBISP( 177 ) = CO2
      EBISP( 178 ) = RO2XC
      EBISP( 179 ) = xHO2
      EBISP( 180 ) = CL
      EBISP( 181 ) = RO2C


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( AXYL1J       ) = 1.0D-03
      RTOL( AXYL2J       ) = 1.0D-03
      RTOL( ATOL1J       ) = 1.0D-03
      RTOL( ATOL2J       ) = 1.0D-03
      RTOL( ABNZ1J       ) = 1.0D-03
      RTOL( ABNZ2J       ) = 1.0D-03
      RTOL( ATRP1J       ) = 1.0D-03
      RTOL( ATRP2J       ) = 1.0D-03
      RTOL( AISO1J       ) = 1.0D-03
      RTOL( AISO2J       ) = 1.0D-03
      RTOL( ASQTJ        ) = 1.0D-03
      RTOL( APAH1J       ) = 1.0D-03
      RTOL( APAH2J       ) = 1.0D-03
      RTOL( AALK1J       ) = 1.0D-03
      RTOL( AALK2J       ) = 1.0D-03
      RTOL( AISO3J       ) = 1.0D-03
      RTOL( O1D          ) = 1.0D+00
      RTOL( SO2          ) = 1.0D-03
      RTOL( SULF         ) = 1.0D+00
      RTOL( SULRXN       ) = 1.0D+00
      RTOL( BACL         ) = 1.0D-03
      RTOL( ISOPRXN      ) = 1.0D+00
      RTOL( BENZENE      ) = 1.0D-03
      RTOL( SOAALK       ) = 1.0D-03
      RTOL( ALKRXN       ) = 1.0D+00
      RTOL( CLACET       ) = 1.0D-03
      RTOL( CLCHO        ) = 1.0D-03
      RTOL( BNZNRXN      ) = 1.0D+00
      RTOL( BNZHRXN      ) = 1.0D+00
      RTOL( XYLNRXN      ) = 1.0D+00
      RTOL( XYLHRXN      ) = 1.0D+00
      RTOL( TOLNRXN      ) = 1.0D+00
      RTOL( TOLHRXN      ) = 1.0D+00
      RTOL( PAHNRXN      ) = 1.0D+00
      RTOL( PAHHRXN      ) = 1.0D+00
      RTOL( ACLI         ) = 1.0D-03
      RTOL( ACLJ         ) = 1.0D-03
      RTOL( ACLK         ) = 1.0D-03
      RTOL( PCVOC        ) = 1.0D-03
      RTOL( PCSOARXN     ) = 1.0D-03
      RTOL( N2O5         ) = 1.0D-03
      RTOL( HO2H         ) = 1.0D-03
      RTOL( PAN          ) = 1.0D-03
      RTOL( PAN2         ) = 1.0D-03
      RTOL( PBZN         ) = 1.0D-03
      RTOL( MAPAN        ) = 1.0D-03
      RTOL( TBUO         ) = 1.0D-03
      RTOL( ISOPOOH      ) = 1.0D-03
      RTOL( IEPOX        ) = 1.0D-03
      RTOL( TOLUENE      ) = 1.0D-03
      RTOL( MXYL         ) = 1.0D-03
      RTOL( OXYL         ) = 1.0D-03
      RTOL( PXYL         ) = 1.0D-03
      RTOL( TMBENZ124    ) = 1.0D-03
      RTOL( ETOH         ) = 1.0D-03
      RTOL( ALK1         ) = 1.0D-03
      RTOL( ALK2         ) = 1.0D-03
      RTOL( ALK3         ) = 1.0D-03
      RTOL( ALK4         ) = 1.0D-03
      RTOL( ALK5         ) = 1.0D-03
      RTOL( ARO1         ) = 1.0D-03
      RTOL( NAPHTHAL     ) = 1.0D-03
      RTOL( CLNO         ) = 1.0D-03
      RTOL( CLONO        ) = 1.0D-03
      RTOL( HOCL         ) = 1.0D-03
      RTOL( H2NO3PK      ) = 1.0D-03
      RTOL( VIVPO1       ) = 1.0D-03
      RTOL( HNO4         ) = 1.0D-03
      RTOL( COOH         ) = 1.0D-03
      RTOL( CCOOOH       ) = 1.0D-03
      RTOL( NPHE         ) = 1.0D-03
      RTOL( xTBUO        ) = 1.0D-03
      RTOL( ROOH         ) = 1.0D-03
      RTOL( R6OOH        ) = 1.0D-03
      RTOL( RAOOH        ) = 1.0D-03
      RTOL( yISOPOOH     ) = 1.0D-03
      RTOL( ARO2MN       ) = 1.0D-03
      RTOL( ACETYLENE    ) = 1.0D-03
      RTOL( CL2          ) = 1.0D-03
      RTOL( CLONO2       ) = 1.0D-03
      RTOL( CLCCHO       ) = 1.0D-03
      RTOL( HCHO_PRIMARY ) = 1.0D-03
      RTOL( CCHO_PRIMARY ) = 1.0D-03
      RTOL( H2NO3PIJ     ) = 1.0D-03
      RTOL( HONO         ) = 1.0D-03
      RTOL( BALD         ) = 1.0D-03
      RTOL( BENZRO2      ) = 1.0D-03
      RTOL( PAHRO2       ) = 1.0D+00
      RTOL( SESQRXN      ) = 1.0D+00
      RTOL( AOLGBJ       ) = 1.0D-03
      RTOL( VSVPO3       ) = 1.0D-03
      RTOL( RCOOOH       ) = 1.0D-03
      RTOL( ETHENE       ) = 1.0D-03
      RTOL( PROPENE      ) = 1.0D-03
      RTOL( BUTADIENE13  ) = 1.0D-03
      RTOL( ISOPRENE     ) = 1.0D-03
      RTOL( APIN         ) = 1.0D-03
      RTOL( TRPRXN       ) = 1.0D+00
      RTOL( TOLRO2       ) = 1.0D-03
      RTOL( OLE1         ) = 1.0D-03
      RTOL( OLE2         ) = 1.0D-03
      RTOL( TERP         ) = 1.0D-03
      RTOL( SESQ         ) = 1.0D-03
      RTOL( ACRO_PRIMARY ) = 1.0D-03
      RTOL( VLVPO1       ) = 1.0D-03
      RTOL( VSVPO1       ) = 1.0D-03
      RTOL( VSVPO2       ) = 1.0D-03
      RTOL( VSVOO1       ) = 1.0D-03
      RTOL( MEOH         ) = 1.0D-03
      RTOL( RNO3         ) = 1.0D-03
      RTOL( ACETONE      ) = 1.0D-03
      RTOL( xMEO2        ) = 1.0D-03
      RTOL( HCOCO3       ) = 1.0D-03
      RTOL( xACROLEIN    ) = 1.0D-03
      RTOL( xCLACET      ) = 1.0D-03
      RTOL( VSVOO2       ) = 1.0D-03
      RTOL( VSVOO3       ) = 1.0D-03
      RTOL( xOH          ) = 1.0D-03
      RTOL( AFG1         ) = 1.0D-03
      RTOL( AFG2         ) = 1.0D-03
      RTOL( ACROLEIN     ) = 1.0D-03
      RTOL( yRAOOH       ) = 1.0D-03
      RTOL( IEPOXOO      ) = 1.0D-03
      RTOL( CLNO2        ) = 1.0D-03
      RTOL( CLO          ) = 1.0D-03
      RTOL( CCOOH        ) = 1.0D-03
      RTOL( xMACO3       ) = 1.0D-03
      RTOL( IPRD         ) = 1.0D-03
      RTOL( XYLRO2       ) = 1.0D-03
      RTOL( MACR         ) = 1.0D-03
      RTOL( MVK          ) = 1.0D-03
      RTOL( xAFG3        ) = 1.0D-03
      RTOL( AOLGAJ       ) = 1.0D-03
      RTOL( xNO2         ) = 1.0D-03
      RTOL( xHOCCHO      ) = 1.0D-03
      RTOL( HOCCHO       ) = 1.0D-03
      RTOL( xBALD        ) = 1.0D-03
      RTOL( xMACR        ) = 1.0D-03
      RTOL( xCL          ) = 1.0D-03
      RTOL( VLVOO1       ) = 1.0D-03
      RTOL( VLVOO2       ) = 1.0D-03
      RTOL( RCOOH        ) = 1.0D-03
      RTOL( xMVK         ) = 1.0D-03
      RTOL( CCHO         ) = 1.0D-03
      RTOL( xBACL        ) = 1.0D-03
      RTOL( AFG3         ) = 1.0D-03
      RTOL( xCLCCHO      ) = 1.0D-03
      RTOL( xRNO3        ) = 1.0D-03
      RTOL( BZCO3        ) = 1.0D-03
      RTOL( CRES         ) = 1.0D-03
      RTOL( xIPRD        ) = 1.0D-03
      RTOL( xACETONE     ) = 1.0D-03
      RTOL( BZO          ) = 1.0D-03
      RTOL( xMECO3       ) = 1.0D-03
      RTOL( xRCO3        ) = 1.0D-03
      RTOL( xPROD2       ) = 1.0D-03
      RTOL( O3P          ) = 1.0D+00
      RTOL( HNO3         ) = 1.0D-03
      RTOL( xCO          ) = 1.0D-03
      RTOL( MGLY         ) = 1.0D-03
      RTOL( xMEK         ) = 1.0D-03
      RTOL( RCHO         ) = 1.0D-03
      RTOL( xAFG1        ) = 1.0D-03
      RTOL( xAFG2        ) = 1.0D-03
      RTOL( GLY          ) = 1.0D-03
      RTOL( RCO3         ) = 1.0D-03
      RTOL( MACO3        ) = 1.0D-03
      RTOL( MEK          ) = 1.0D-03
      RTOL( HCOOH        ) = 1.0D-03
      RTOL( NO           ) = 1.0D-03
      RTOL( xGLY         ) = 1.0D-03
      RTOL( xMGLY        ) = 1.0D-03
      RTOL( PRD2         ) = 1.0D-03
      RTOL( xRCHO        ) = 1.0D-03
      RTOL( O3           ) = 1.0D-03
      RTOL( MEO2         ) = 1.0D-03
      RTOL( MECO3        ) = 1.0D-03
      RTOL( xCCHO        ) = 1.0D-03
      RTOL( yR6OOH       ) = 1.0D-03
      RTOL( NO2          ) = 1.0D-03
      RTOL( xHCHO        ) = 1.0D-03
      RTOL( HCHO         ) = 1.0D-03
      RTOL( HCL          ) = 1.0D-03
      RTOL( CO           ) = 1.0D-03
      RTOL( yROOH        ) = 1.0D-03
      RTOL( zRNO3        ) = 1.0D-03
      RTOL( CO2          ) = 1.0D-03
      RTOL( NO3          ) = 1.0D-03
      RTOL( RO2XC        ) = 1.0D-03
      RTOL( xHO2         ) = 1.0D-03
      RTOL( CL           ) = 1.0D-03
      RTOL( RO2C         ) = 1.0D-03
      RTOL( HO2          ) = 1.0D-03
      RTOL( OH           ) = 1.0D-03


      RETURN

      END
