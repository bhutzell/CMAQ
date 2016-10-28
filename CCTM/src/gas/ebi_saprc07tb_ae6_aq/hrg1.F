
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

       SUBROUTINE HRG1( DTC )

C**********************************************************************
C
C  FUNCTION: To solve for the concentration of NO2, NO, O3, and O3P
C            algebraically.
C
C  PRECONDITIONS: For the SAPRC07TB family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Aug 18, 2016
C
C   18 Jul 14 B.Hutzell: revised to use real(8) variables
C**********************************************************************
      USE HRDATA

      IMPLICIT NONE


C..INCLUDES: None


C..ARGUMENTS:
      REAL( 8 ), INTENT( IN ) :: DTC                      ! Time step


C..PARAMETERS: None


C..EXTERNAL FUNCTIONS: NONE


C..SAVED LOCAL VARIABLES:
!     CHARACTER( 16 ), SAVE  :: PNAME = 'HRG1'   ! Prgram Name


C..SCRATCH LOCAL VARIABLES:
      REAL( 8 ) :: O1D_S               ! sum of O1D loss frequencies
      REAL( 8 ) :: O3P_S               ! stoich coeff for O3P from O1D



      REAL( 8 ) :: R1_2                ! production term for NO from NO2
      REAL( 8 ) :: R2_1                ! production term for NO2 from NO
      REAL( 8 ) :: P1, P2, P3, P12     ! production terms for NO, NO2, O3, & O3P
      REAL( 8 ) :: L1, L2, L3, L12     ! loss terms for NO, NO2, O3, O3P
      REAL( 8 ) :: L1_INV, L2_INV,
     &             L3_INV, L12_INV     ! inverse of loss terms

      REAL( 8 ) :: T1, T2, T3, T4, T5  ! intermerdiate terms
      REAL( 8 ) :: F1, F2, F3          ! intermerdiate terms
      REAL( 8 ) :: A, B, C             ! coefficients for quadratic equation
      REAL( 8 ) :: Q, XX, S1, S2       ! intermerdiate terms

      REAL( 8 ) :: RK1, RK2, RK3       ! rate constants

      REAL( 8 ) :: PO3                 ! temp variable for O3

C**********************************************************************


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O1D Section
c    1) sum of the rate constants for all O1D loss reactions
c    2) get fractional yield of O3P from O1D loss
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      O1D_S =                 RKI(  70 )                        ! O1D=0.2000D+01*OH
     &      +                 RKI(  71 )                        ! O1D=O3P

      O3P_S =                 RKI(  71 )                        ! O1D=O3P

      O3P_S  = O3P_S / O1D_S


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO Section
c    R1_2 = production of NO from NO2 ( rates of form k[NO2][x] )
c           except NO2+NO3=NO+NO2 (it is treated as if it were NO3=NO )
c    P1 =   remaining NO production terms
c    L1 =   loss of NO (except rxns producing NO2 - they are in R2_1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R1_2 =                 RKI(   1 )                        ! NO2=NO+O3P
     &     +                 RKI(  59 ) * YC( O3P          )   ! NO2+O3P=NO
      R1_2  = R1_2 * DTC


      P1 =                 RXRAT(   2 )      ! NO3=NO
     &   +                 RXRAT(   6 )      ! HONO=NO+OH
     &   +                 RXRAT(  43 )      ! CLNO=NO+CL
     &   +                 RXRAT(  69 )      ! NO2+NO3=NO+NO2
      P1    = YC0( NO ) + P1 * DTC


      L1 =                 RKI(  72 ) * YC( OH           )   ! NO+OH=HONO
     &   +                 RKI( 103 ) * YC( RO2XC        )   ! NO+RO2XC=
     &   +                 RKI( 344 ) * YC( CL           )   ! NO+CL=CLNO
      L1    = 1.0D0 + L1 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO2 Section
c    R2_1 = production of NO2 from NO ( rates of form k[NO][x] )
c            a)  NO+O3=NO2 not included
c            b)  NO+NO3=2NO2 ( 1/2 of NO2 formation rate included )
c            c)  NO3+NO2=NO+NO2 is not included for NO2
c    P2 =  remaining NO2 production terms 
c            a)  NO+O3=NO2 not included
c            b)  NO+NO3=2NO2 (1/2 of NO2 formation rate included )
c    L2 = loss of NO2 (except rxns producing NO2 - they are in R1_2)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R2_1 =                 RKI(  58 ) * YC( O3P          )   ! NO+O3P=NO2
     &     +                 RKI(  63 ) * YC( NO3          )   ! NO+NO3=0.2000D+...
     &     +    2.0000D+00 * RKI(  64 ) * YC( NO           )   ! NO+NO=0.2000D+01*NO2
     &     +                 RKI(  79 ) * YC( HO2          )   ! NO+HO2=NO2+OH
     &     +                 RKI(  92 ) * YC( MEO2         )   ! NO+MEO2=NO2+HCHO+HO2
     &     +                 RKI(  98 ) * YC( RO2C         )   ! NO+RO2C=NO2
     &     +                 RKI( 111 ) * YC( MECO3        )   ! NO+MECO3=NO2+CO2+...
     &     +                 RKI( 120 ) * YC( RCO3         )   ! NO+RCO3=NO2+RO2C+...
     &     +                 RKI( 130 ) * YC( BZCO3        )   ! NO+BZCO3=NO2+CO2+...
     &     +                 RKI( 141 ) * YC( MACO3        )   ! NO+MACO3=NO2+CO2+...
     &     +                 RKI( 225 ) * YC( HCOCO3       )   ! NO+HCOCO3=NO2+CO+...
     &     +                 RKI( 302 ) * YC( IEPOXOO      )   ! NO+IEPOXOO=NO2+...
     &     +                 RKI( 351 ) * YC( CLO          )   ! NO+CLO=NO2+CL
      R2_1  = R2_1 * DTC


      P2 =                 RXRAT(   3 )      ! NO3=NO2+O3P
     &   +                 RXRAT(   7 )      ! HNO3=NO2+OH
     &   +    6.1000D-01 * RXRAT(   8 )      ! HNO4=0.6100D+00*NO2+...
     &   +    6.0000D-01 * RXRAT(  10 )      ! PAN=0.6000D+00*NO2+...
     &   +    6.0000D-01 * RXRAT(  11 )      ! PAN2=0.6000D+00*NO2+...
     &   +    6.0000D-01 * RXRAT(  12 )      ! PBZN=0.6000D+00*NO2+...
     &   +    6.0000D-01 * RXRAT(  13 )      ! MAPAN=0.6000D+00*NO2+...
     &   +                 RXRAT(  37 )      ! RNO3=NO2+0.5540D+...
     &   +                 RXRAT(  44 )      ! CLONO=NO2+CL
     &   +                 RXRAT(  45 )      ! CLNO2=NO2+CL
     &   +                 RXRAT(  46 )      ! CLONO2=NO2+CLO
     &   +                 RXRAT(  63 )      ! NO+NO3=0.2000D+01*NO2
     &   +                 RXRAT(  66 )      ! N2O5=NO2+NO3
     &   +                 RXRAT(  73 )      ! OH+HONO=NO2
     &   +                 RXRAT(  75 )      ! OH+NO3=NO2+HO2
     &   +                 RXRAT(  81 )      ! HNO4=NO2+HO2
     &   +                 RXRAT(  82 )      ! HNO4+OH=NO2
     &   +    8.0000D-01 * RXRAT(  86 )      ! NO3+HO2=0.8000D+...
     &   +    2.0000D+00 * RXRAT(  87 )      ! NO3+NO3=0.2000D+01*NO2
     &   +                 RXRAT(  95 )      ! MEO2+NO3=NO2+HO2+HCHO
     &   +                 RXRAT( 100 )      ! RO2C+NO3=NO2
     &   +                 RXRAT( 105 )      ! RO2XC+NO3=NO2
     &   +                 RXRAT( 110 )      ! PAN=NO2+MECO3
     &   +                 RXRAT( 113 )      ! MECO3+NO3=NO2+CO2+MEO2
     &   +                 RXRAT( 119 )      ! PAN2=NO2+RCO3
     &   +                 RXRAT( 122 )      ! RCO3+NO3=NO2+RO2C+...
     &   +                 RXRAT( 129 )      ! PBZN=NO2+BZCO3
     &   +                 RXRAT( 132 )      ! BZCO3+NO3=NO2+CO2+BZO+...
     &   +                 RXRAT( 140 )      ! MAPAN=NO2+MACO3
     &   +                 RXRAT( 143 )      ! MACO3+NO3=NO2+CO2+...
     &   +                 RXRAT( 160 )      ! xNO2=NO2
     &   +    1.9000D-02 * RXRAT( 216 )      ! RNO3+OH=0.1900D-...
     &   +                 RXRAT( 350 )      ! CL+NO3=NO2+CLO
     &   +                 RXRAT( 353 )      ! CLONO2=NO2+CLO
     &   +    3.8000D-02 * RXRAT( 365 )      ! RNO3+CL=0.3800D-...
      P2 = YC0( NO2 ) + P2 * DTC


      L2 =                 RKI(  60 ) * YC( O3P          )   ! NO2+O3P=NO3
     &   +                 RKI(  62 ) * YC( O3           )   ! NO2+O3=NO3
     &   +                 RKI(  65 ) * YC( NO3          )   ! NO2+NO3=N2O5
     &   +                 RKI(  74 ) * YC( OH           )   ! NO2+OH=HNO3
     &   +                 RKI(  80 ) * YC( HO2          )   ! NO2+HO2=HNO4
     &   +                 RKI( 109 ) * YC( MECO3        )   ! NO2+MECO3=PAN
     &   +                 RKI( 118 ) * YC( RCO3         )   ! NO2+RCO3=PAN2
     &   +                 RKI( 128 ) * YC( BZCO3        )   ! NO2+BZCO3=PBZN
     &   +                 RKI( 139 ) * YC( MACO3        )   ! NO2+MACO3=MAPAN
     &   +                 RKI( 151 ) * YC( TBUO         )   ! NO2+TBUO=RNO3
     &   +                 RKI( 153 ) * YC( BZO          )   ! NO2+BZO=NPHE
     &   +                 RKI( 226 ) * YC( HCOCO3       )   ! NO2+HCOCO3=HO2+...
     &   +                 RKI( 345 ) * YC( CL           )   ! NO2+CL=CLONO
     &   +                 RKI( 346 ) * YC( CL           )   ! NO2+CL=CLNO2
     &   +                 RKI( 352 ) * YC( CLO          )   ! NO2+CLO=CLONO2
     &   +                 RKI( 430 )                        ! NO2=0.5000D+...
      L2     = 1.0D0 + L2 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3 Section
c    P3 = production of O3 except O+O2=O3
c    L3 =   loss terms for O3 except NO+O3=NO2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P3 =    1.5000D-01 * RXRAT( 112 )      ! MECO3+HO2=0.1500D+...
     &   +    1.5000D-01 * RXRAT( 121 )      ! RCO3+HO2=0.1500D+...
     &   +    1.5000D-01 * RXRAT( 131 )      ! BZCO3+HO2=0.1500D+...
     &   +    1.5000D-01 * RXRAT( 142 )      ! MACO3+HO2=0.1500D+...
     &   +    1.5000D-01 * RXRAT( 227 )      ! HCOCO3+HO2=0.1500D+...
      P3 = YC0( O3 ) + P3 * DTC


      L3 =                 RKI(   4 )                        ! O3=O1D
     &   +                 RKI(   5 )                        ! O3=O3P
     &   +                 RKI(  55 )                        ! O3=
     &   +                 RKI(  57 ) * YC( O3P          )   ! O3+O3P=
     &   +                 RKI(  62 ) * YC( NO2          )   ! O3+NO2=NO3
     &   +                 RKI(  78 ) * YC( OH           )   ! O3+OH=HO2
     &   +                 RKI(  83 ) * YC( HO2          )   ! O3+HO2=OH
     &   +                 RKI( 200 ) * YC( AFG1         )   ! O3+AFG1=0.8260D+...
     &   +                 RKI( 202 ) * YC( AFG2         )   ! O3+AFG2=0.8260D+...
     &   +                 RKI( 204 ) * YC( AFG3         )   ! O3+AFG3=0.4710D+...
     &   +                 RKI( 206 ) * YC( MACR         )   ! O3+MACR=0.2080D+...
     &   +                 RKI( 210 ) * YC( MVK          )   ! O3+MVK=0.1640D+...
     &   +                 RKI( 213 ) * YC( IPRD         )   ! O3+IPRD=0.2850D+...
     &   +                 RKI( 220 ) * YC( ACROLEIN     )   ! O3+ACROLEIN=...
     &   +                 RKI( 283 ) * YC( ETHENE       )   ! O3+ETHENE=...
     &   +                 RKI( 287 ) * YC( PROPENE      )   ! O3+PROPENE=...
     &   +                 RKI( 291 ) * YC( BUTADIENE13  )   ! O3+BUTADIENE13=...
     &   +                 RKI( 295 ) * YC( ISOPRENE     )   ! O3+ISOPRENE=...
     &   +                 RKI( 307 ) * YC( APIN         )   ! O3+APIN=0.9000D-...
     &   +                 RKI( 311 ) * YC( ACETYLENE    )   ! O3+ACETYLENE=...
     &   +                 RKI( 326 ) * YC( OLE1         )   ! O3+OLE1=0.9500D-...
     &   +                 RKI( 330 ) * YC( OLE2         )   ! O3+OLE2=0.9400D-...
     &   +                 RKI( 337 ) * YC( TERP         )   ! O3+TERP=0.7800D-...
     &   +                 RKI( 341 ) * YC( SESQ         )   ! O3+SESQ=0.7800D-...
     &   +                 RKI( 349 ) * YC( CL           )   ! O3+CL=CLO
      L3    = 1.0D0 + L3 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3P Section 
c    P12 = production of O3P except NO2+hv=O3P (J1)
c    L12 = loss terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P12 =                 RXRAT(   3 )      ! NO3=O3P+NO2
     &    +   O3P_S *       RXRAT(   4 )      ! O3=O1D
     &    +                 RXRAT(   5 )      ! O3=O3P
      P12 = YC0( O3P ) + P12 * DTC


      L12 =                 RKI(  56 )                        ! O3P=O3
     &    +                 RKI(  57 ) * YC( O3           )   ! O3P+O3=
     &    +                 RKI(  58 ) * YC( NO           )   ! O3P+NO=NO2
     &    +                 RKI(  59 ) * YC( NO2          )   ! O3P+NO2=NO
     &    +                 RKI(  60 ) * YC( NO2          )   ! O3P+NO2=NO3
     &    +                 RKI( 208 ) * YC( MACR         )   ! O3P+MACR=RCHO
     &    +                 RKI( 211 ) * YC( MVK          )   ! O3P+MVK=0.4500D+...
     &    +                 RKI( 222 ) * YC( ACROLEIN     )   ! O3P+ACROLEIN=RCHO
     &    +                 RKI( 285 ) * YC( ETHENE       )   ! O3P+ETHENE=0.8000D+...
     &    +                 RKI( 289 ) * YC( PROPENE      )   ! O3P+PROPENE=0.4500D+...
     &    +                 RKI( 293 ) * YC( BUTADIENE13  )   ! O3P+BUTADIENE13=...
     &    +                 RKI( 297 ) * YC( ISOPRENE     )   ! O3P+ISOPRENE=0.2500D+...
     &    +                 RKI( 309 ) * YC( APIN         )   ! O3P+APIN=PRD2+TRPRXN
     &    +                 RKI( 328 ) * YC( OLE1         )   ! O3P+OLE1=0.4500D+...
     &    +                 RKI( 332 ) * YC( OLE2         )   ! O3P+OLE2=0.7900D-...
     &    +                 RKI( 339 ) * YC( TERP         )   ! O3P+TERP=0.2370D+...
     &    +                 RKI( 343 ) * YC( SESQ         )   ! O3P+SESQ=0.2370D+...
      L12   = 1.0D0 + L12 * DTC

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Solution section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..compute reciprocal of loss terms
      L1_INV  = 1.0D0 / L1
      L2_INV  = 1.0D0 / L2
      L3_INV  = 1.0D0 / L3
      L12_INV = 1.0D0 / L12

c..compute specific k*delta t terms
      RK1 = RKI(   1 ) * DTC            ! J1    (NO2+hv=NO+O3P)
      RK2 = RKI(  56 ) * DTC            ! J2    (O3P+O2=O3)
      RK3 = RKI(  61 ) * DTC            ! k1_3  (NO+O3=NO2)

c..compute terms that are used to calulate a,b & c
      T1 = RK1  * L2_INV                ! J1   / ( 1.0 + Lno2 * dt )
      T2 = R1_2 * L2_INV                ! r1,2 / ( 1.0 + Lno2 * dt)
      T3 = R2_1 * L1_INV                ! r2,1 / ( 1.0 + Lno  * dt)
      T4 = RK2  * L12_INV               ! J2   / ( 1.0 + Lo3p * dt )
      T5 = T3   * P1 - T2 * P2          ! T3 * Pno - T2 * Pno2

      F1 = 1.0D0 + T2 + T3                ! factor in calculating a & b
      F2 = T1 * T4                      ! factor in calculating a & b
      F3 = L3 * L1 + RK3 * P1           ! (1 + Lo3 * dt) (1 + lno * dt )
                                        ! + k1,3 * dt * Pno

      PO3 = P3 + P12 * T4

      A = RK3 * ( F1  - F2 )

      B = F1 * F3 +  RK3 * ( F2 * ( P2 - P1 ) + PO3 +  T5 )

      C = RK3 * P1 * ( PO3 + P2 * F2 ) + F3 * T5

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B - 4.0D0 * A * C ) )

      XX = MAX( Q / A , C / Q  )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Species solutions
c       [NO]   = ( P1 + x ) / ( 1 + L1 )
c       [NO2]  = ( P2 - x ) / ( 1 + L2 )
c       [O3 ]  = ( P3 + Ko3p->O3 ) / (1 + K1,3 * [NO] + L3 )
c       [O3P]  = ( P12 + J1 * [NO2] ) / ( 1 + L12 )
c       [O1D2] = ( Ko3->o1d * [O3] ) / ( O1D_DNM )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      YCP( NO ) = MAX( 0.0D0, ( P1 + XX ) * L1_INV )

      YCP( NO2 ) = MAX( 0.0D0, ( P2 - XX ) * L2_INV )

      S1 = P12 + RK1 * YCP( NO2 )

      S2 = T4 * S1

      YCP( O3 ) = ( P3 + S2 ) / ( L3 + RK3 * YCP( NO ) )

      YCP( O3P ) = S1 * L12_INV

      YCP( O1D ) = RKI( 4 ) * YCP( O3 ) / O1D_S

      RETURN

      END


