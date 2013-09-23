
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
C $Header: /project/work/rep/arc/CCTM/src/cloud/cloud_acm_ae6/acmcld.f,v 1.4 2011/10/21 16:10:26 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE ACMCLD ( F, C, SIGMAF, CBELOW, CLBASE, CLTOP, FRAC,
     &                    NSP, NLAYS, TCLIFE, DTCLD )
C-----------------------------------------------------------------------
C
C  FUNCTION:  Subroutine to compute convective mixing in the CBL
C             according to the Asymmetrical Convective Model (ACM).
C             Ref: Pleim snd Chang (1992)
C
C  SUMMARY:
C   ACM is based on the Blackadar non-local convective model which is
C   used in HIRPBL where upward mixing similar to Blackadar but
C   downward mixing is to the next lower level representing more
C   realistic gradual subsidence.
C
C  REVISION  HISTORY:
C      Date   Who             What
C    -------- ---             -----------------------------------------
C     06/2005 J.Pleim         Initial version
C     07/2005 J.Young         Clean up for CMAQ-F
C     07/2013 S.Napelenok     ddm-3d implementation for cmaq5.0.1
C-----------------------------------------------------------------------

#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX, S_CCR, S_CBELOW
#endif 

      IMPLICIT NONE

C Arguments

      INTEGER NSP                ! no. of species
      INTEGER NLAYS              ! no. of model layers
      REAL    F( NLAYS )         ! entrainment fraction
      REAL    C( NSP, NLAYS )    ! species concentration
      REAL    SIGMAF( 0:NLAYS )  ! full layer sigma (mono decr)
      REAL    CBELOW( NSP )      ! spec conc in layer below cld base
      INTEGER CLBASE, CLTOP
      REAL    FRAC               ! grid cell fractional cloud cover
      REAL    TCLIFE             ! cloud lifetime (s)
      REAL    DTCLD              ! cloud integration time step

C Parameters

      REAL, PARAMETER :: HALF = 0.5
      REAL, PARAMETER :: CRANKP = 0.5

C Local variables

      INTEGER NLP, K, NL, S          ! index variables
      INTEGER KB

      REAL DTLIM, F1
c     REAL TOT1, TOT2
      REAL DTS, DELC, M1UP
      REAL( 8 ) :: AI( NLAYS ), BI( NLAYS ), EI( NLAYS )
      REAL( 8 ) :: DI( NLAYS ), UI( NLAYS )
      REAL( 8 ) :: ALPHA, BETA, GAMA
      REAL VCI( NLAYS,NSP )
      REAL MBARKS( NLAYS ), MDWN( NLAYS )
      REAL DSIGH( NLAYS ), DSIGHI( NLAYS )

#ifdef sens
      REAL( 8 ) :: S_VCI( NPMAX,NLAYS,NSP )
      REAL( 8 ) :: S_UI( NPMAX,NLAYS )
      REAL( 8 ) :: S_DI( NPMAX,NLAYS )
      REAL( 8 ) :: S_DELC( NPMAX )
      REAL( 8 ) :: S_F1( NPMAX )
      REAL( 8 ) :: S_BETA( NPMAX )
#endif

C-----------------------------------------------------------------------

      DTLIM = DTCLD
      MDWN ( CLTOP + 1 ) = 0.0
      DSIGH( CLTOP + 1 ) = 1.0
c     SIGMAF( 0 ) = 1
      M1UP = 0.0
      KB  = CLBASE - 1
c     CLBASE = CLBASE
      DSIGH ( KB ) = SIGMAF( KB ) - 1.0
      DSIGHI( KB ) = 1.0 / DSIGH( KB )

C Compute ACM mixing rate

      DO K = CLTOP, CLBASE, -1
        DSIGH ( K ) = SIGMAF( K ) - SIGMAF( K - 1 )
        DSIGHI( K ) = 1.0 / DSIGH( K )
        MBARKS( K ) = ( 1.0 - F( K ) ) * FRAC / TCLIFE
        MDWN  ( K ) = MBARKS( K )
     &              + MDWN( K + 1 ) * DSIGH( K + 1 ) * DSIGHI( K )
        M1UP  = M1UP + MBARKS( K ) * DSIGH( K )
        DTLIM = MIN( HALF / ( M1UP * DSIGHI( K ) ), DTLIM )
      END DO
      DTLIM = MIN( HALF / ( M1UP * DSIGHI( KB ) ), DTLIM )

#ifdef sens
      S_VCI = 0.0D0
      S_DI  = 0.0D0
#endif

      DO S = 1, NSP
#ifdef sens
        DO NP = 1, NPMAX
          S_VCI( NP, KB, S ) = S_CBELOW( NP, S )
          S_VCI( NP,CLTOP+1,S ) = 0.0D0
        END DO
#endif
        VCI( KB, S ) = CBELOW( S )
        VCI( CLTOP+1,S ) = 9999.0
        DO K = CLBASE, CLTOP
#ifdef sens
          DO NP = 1, NPMAX
            S_VCI( NP,K,S ) = S_CCR( NP,S,K )
            S_UI( NP,K )  = 0.0D0
          END DO
#endif
          VCI( K,S ) = C( S,K )
          UI( K )  = 0.0           ! init variable for use below
        END DO
      END DO

      NLP = INT( DTCLD / DTLIM + 1.0 )
      DTS = ( DTCLD / NLP )
      DO 2000 NL = 1, NLP      ! loop over sub timestep
        DO 1000 S = 1, NSP     ! loop over species
                                                                              
C Compute tendency of CBL concentrations - Semi-Implicit solution

          DO K = CLBASE, CLTOP
#ifdef sens
            DO NP = 1, NPMAX
              S_DELC( NP ) = REAL( DTS, 8 )
     &                     * ( REAL ( MBARKS( K ), 8 ) * S_VCI( NP,KB,S )
     &                     -   REAL ( MDWN( K ), 8 ) * S_VCI( NP,K,S )
     &                     + REAL( (DSIGH( K+1 ) * DSIGHI( K ) * MDWN( K+1 )), 8 ) * S_VCI( NP,K+1,S ) )
              S_DI( NP,K ) = S_VCI( NP,K,S ) + ( 1.0D0 - REAL( CRANKP, 8 ) ) * S_DELC( NP )
            END DO
#endif
            DELC = DTS
     &           * ( MBARKS( K ) * VCI( KB,S )
     &           -   MDWN( K ) * VCI( K,S )
     &           +   DSIGH( K+1 ) * DSIGHI( K ) * MDWN( K+1 ) * VCI( K+1,S ) )
            DI( K ) = VCI( K,S ) + ( 1.0 - CRANKP ) * DELC
            EI( K ) = -CRANKP * MDWN( K ) * DTS * DSIGH( K ) * DSIGHI( K-1 )
            BI( K ) = 1.0 + CRANKP * MDWN( K ) * DTS
            AI( K ) = -CRANKP * MBARKS( K ) * DTS
          END DO
#ifdef sens
          DO NP = 1, NPMAX
            S_F1( NP ) = REAL (M1UP, 8) * S_VCI( NP,KB,S )
     &                 - REAL (MDWN( CLBASE ), 8) * S_VCI( NP,CLBASE,S ) * REAL(DSIGH( CLBASE ), 8)
            S_DI( NP,KB ) = S_VCI( NP,KB,S ) - ( 1.0D0 - REAL(CRANKP,8) ) * S_F1( NP ) * REAL( (DSIGHI( KB ) * DTS), 8)
          END DO
#endif
          BI( KB ) = 1.0 + CRANKP * M1UP * DTS * DSIGHI( KB )
          F1 = M1UP * VCI( KB,S )
     &       - MDWN( CLBASE ) * VCI( CLBASE,S ) * DSIGH( CLBASE )
          DI( KB ) = VCI( KB,S ) - ( 1.0 - CRANKP ) * F1 * DSIGHI( KB ) * DTS

C Define arrays A,B,E which make up matrix and D which is RHS

          BETA = DI( KB )
#ifdef sens
          DO NP = 1, NPMAX
            S_BETA( NP ) = S_DI( NP,KB )
          END DO
#endif
          GAMA = BI( KB )
          ALPHA = 1.0
          DO K = CLBASE, CLTOP
            ALPHA = -ALPHA * EI( K ) / BI( K )
            BETA  = ALPHA * DI( K ) + BETA
            GAMA  = ALPHA * AI( K ) + GAMA
#ifdef sens
            DO NP = 1, NPMAX
              S_BETA( NP )  = ALPHA * S_DI( NP,K ) + S_BETA( NP )
            END DO
#endif
          END DO

#ifdef sens
          DO NP = 1, NPMAX
            S_UI( NP,KB )   = S_BETA( NP ) / GAMA
            S_UI( NP,CLTOP ) = ( S_DI( NP,CLTOP ) - AI( CLTOP ) * S_UI( NP,KB ) ) / BI( CLTOP )
          END DO
#endif
          UI( KB )   = BETA / GAMA
          UI( CLTOP ) = ( DI( CLTOP ) - AI( CLTOP ) * UI( KB ) ) / BI( CLTOP )

c         BETA = DI( KB )
c         GAMA = BI( KB )
c         ALPHA = 1.0

C Back substitution:
          DO K = CLTOP - 1, CLBASE, -1
#ifdef sens
            DO NP = 1 , NPMAX
              S_UI( NP,K ) = ( S_DI( NP,K ) - AI( K ) * S_UI( NP,KB ) - EI( K+1 ) * S_UI( NP,K+1 ) )
     &                     / BI( K )
            END DO
#endif
            UI( K ) = ( DI( K ) - AI( K ) * UI( KB ) - EI( K+1 ) * UI( K+1 ) )
     &              / BI( K )
          END DO

C Update concentrations
          DO K = KB, CLTOP
            VCI( K,S ) = UI( K )
#ifdef sens
            DO NP = 1 , NPMAX
              S_VCI( NP,K,S ) = S_UI( NP,K )
            END DO
#endif
          END DO


1000    CONTINUE                 ! end loop for species
2000  CONTINUE               ! end timestep loop

      DO S = 1, NSP
c       TOT1 = 0.0
c       TOT2 = 0.0

c       DO K = 1, CLTOP
c         TOT1 = TOT1 + C( S,K ) * ( SIGMAF( K-1 ) - SIGMAF( K ) )
c       END DO

        CBELOW( S ) = VCI( KB,S )

        DO K = CLBASE, CLTOP
          C( S,K ) = VCI( K,S )
        END DO
#ifdef sens
        DO NP = 1, NPMAX
          S_CBELOW( NP,S ) = S_VCI( NP,KB,S )
          DO K = CLBASE, CLTOP
            S_CCR( NP,S,K ) = S_VCI( NP,K,S )
          END DO
        END DO
#endif
      END DO

      RETURN
      END
