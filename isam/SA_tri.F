
!***********************************************************************
!   Portions of Models-3/CMAQ software were developed or based on      *
!   information from various groups: Federal Government employees,     *
!   contractors working on a United States Government contract, and    *
!   non-Federal sources (including research institutions).  These      *
!   research institutions have given the Government permission to      *
!   use, prepare derivative works, and distribute copies of their      *
!   work in Models-3/CMAQ to the public and to permit others to do     *
!   so.  EPA therefore grants similar permissions for use of the       *
!   Models-3/CMAQ software, but users are requested to provide copies  *
!   of derivative works to the Government without restrictions as to   *
!   use by others.  Users are responsible for acquiring their own      *
!   copies of commercial software associated with Models-3/CMAQ and    *
!   for complying with vendor requirements.  Software copyrights by    *
!   the MCNC Environmental Modeling Center are used with their         *
!   permissions subject to the above restrictions.                     *
!***********************************************************************

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /amber/home/krt/cmq471/models/CCTM/src/vdiff/acm2_inline/tri.F,v 1.1.1.1 2010/06/14 16:03:07 sjr Exp $ 

! what(1) key, module and SID; SCCS file; date and time of last delta:
! @(#)tridiag.F	1.1 /project/mod3/CMAQ/src/vdiff/eddy/SCCS/s.tridiag.F 03 Jun 1997 11:45:09

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SA_TRI ( L, D, U, B, X )
C-----------------------------------------------------------------------

C  FUNCTION:
C    Solves tridiagonal system by Thomas algorithm.  Algorithm fails
C    ( M3ERR ) if first pivot is zero.  In that case, rewrite the
C    equation as a set of order KMAX-1, with X(2) trivially eliminated.
C The associated tri-diagonal system is stored in 3 arrays
C   D : diagonal
C   L : sub-diagonal
C   U : super-diagonal
C   B : right hand side function
C   X : return solution from tridiagonal solver

C     [ D(1) U(1) 0    0    0 ...       0     ]
C     [ L(2) D(2) U(2) 0    0 ...       .     ]
C     [ 0    L(3) D(3) U(3) 0 ...       .     ]
C     [ .       .     .     .           .     ] X(i) = B(i)
C     [ .             .     .     .     0     ]
C     [ .                   .     .     .     ]
C     [ 0                           L(n) D(n) ]

C   where n = NLAYS

C  PRECONDITIONS REQUIRED:
C    Dimensionality set up in terms of NLAYS from SUBST_VGRD_ID

C  SUBROUTINES AND FUNCTIONS CALLED:

C  REVISION HISTORY:
C    NO.   DATE     WHO      WHAT
C    __    ____     ___      ____
C    5     Apr 06   JEP   adapted tridiag for use with ACM2
C    4     Aug 96    yoj  cleaner
C    3     8/16/94   XKX  configuration management include statements
C    2     3/15/92   CJC  For use in Models-3 LCM.
C    1     10/19/89  JKV  converted for use on IBM
C    0      3/89     BDX  Initial version
C-----------------------------------------------------------------------

      USE VGRD_DEFN           ! vertical layer specifications
      USE SA_DEFN             ! Mc06

      IMPLICIT NONE


C Arguments:

      REAL, INTENT( IN )  :: L( : )      ! subdiagonal
      REAL, INTENT( IN )  :: D( : )      ! diagonal
      REAL, INTENT( IN )  :: U( : )      ! superdiagonal
      REAL, INTENT( IN )  :: B( :,: )    ! R.H. side
      REAL, INTENT( OUT ) :: X( :,: )    ! solution

C Local Variables:

      REAL        GAM( NLAYS )
      REAL        BET
      INTEGER     V, K

C Decomposition and forward substitution:
      BET = 1.0 / D( 1 )
      DO V = 1, N_SPCTAG
         X( V,1 ) = BET * B(V,1 )
      END DO

      DO K = 2, NLAYS
         GAM( K ) = BET * U( K-1 )
         BET = 1.0 / ( D( K ) - L( K ) * GAM( K ) )
	 DO V = 1, N_SPCTAG
            X( V, K ) = BET * ( B( V,K ) - L( K ) * X( V,K-1 ) )
	 END DO
      END DO

C Back-substitution:

      DO K = NLAYS - 1, 1, -1
         DO V = 1, N_SPCTAG
            X( V,K ) = X( V,K ) - GAM( K+1 ) * X( V,K+1 )
         END DO
      END DO

      RETURN
      END
