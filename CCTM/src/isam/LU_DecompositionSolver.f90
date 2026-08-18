Module LU_DEcompositionSolver

 Implicit None

    LOGICAL :: USE_LU_SOLVERS
 
    INTERFACE ludcmp_f90
      Module Procedure dludcmp_f90, &
  &                    sludcmp_f90    
    End INTERFACE
    INTERFACE lubksb_f90
      Module Procedure dlubksb_f90, &
   &                   slubksb_f90
    End INTERFACE

! smallest number possible for each precision KIND
    REAL(4), PARAMETER, PRIVATE :: SMALL   = TINY(1.0)  
    REAL(8), PARAMETER, PRIVATE :: SMALLER = TINY(1.0D0)
    
 Contains
 
   subroutine lu_solve_nr(a,b,n)

    Implicit None

    real(8), intent(inout) :: a(:,:)
    real(8), intent(inout) :: b(:)
    integer, intent(in   ) :: n

    integer                :: i,j
    integer, allocatable, save   :: ipiv(:)
    real(8), allocatable, save   :: copy(:,:)
    
    integer :: d, code
    logical, save :: first_call = .true.
    
    if( first_call )then
!        n = size(a,1)
        allocate(ipiv(n))
        allocate(copy(n,n))
        first_call = .false.
    end if

    call LUDCMP_F77(a, n, ipiv, d)
    call LUBKSB_F77(a, n, ipiv, b)
    !call DP_MPROVE(copy, a, ipiv, b)
   
  end subroutine lu_solve_nr

   subroutine lu_solve_nrf90(a,b,n)

    Implicit None

    real(8), intent(inout) :: a(:,:)
    real(8), intent(inout) :: b(:)
    integer, intent(in   ) :: n
 
    integer                :: i,j
    integer, allocatable   :: ipiv(:)
    
    integer :: d, code

!    n = size(a,1)
    allocate(ipiv(n))


    call LUDCMP_F90(a, n, ipiv, d)
    call LUBKSB_F90(a, n, ipiv, b)
   
  end subroutine lu_solve_nrf90


 Subroutine LUDCMP_F77(A,N,INDX,D)
!  ***************************************************************
!  * Given an N x N matrix A, this routine replaces it by the LU *
!  * decomposition of a rowwise permutation of itself. A and N   *
!  * are input. INDX is an output vector which records the row   *
!  * permutation effected by the partial pivoting; D is output   *
!  * as -1 or 1, depending on whether the number of row inter-   *
!  * changes was even or odd, respectively. This routine is used *
!  * in combination with LUBKSB to solve linear equations or to  *
!  * invert a matrix. Return code is 1, if matrix is singular.   *
!  *************************************************************** 
!*                 F90 version by J-P Moreau, Paris    *
!*                        (www.jpmoreau.fr)            *
!* --------------------------------------------------- *
!* Reference:                                          *
!*                                                     *
!* "Numerical Recipes By W.H. Press, B. P. Flannery,   *
!*  S.A. Teukolsky and W.T. Vetterling, Cambridge      *
!*  University Press, 1986" [BIBLI 08].                *
!*                                                     * 
   IMPLICIT NONE
 
     REAL( 8 ), INTENT( INOUT ) :: A( :,: )
     INTEGER,   INTENT( IN    ) :: N
     INTEGER,   INTENT(   OUT ) :: INDX( : )
     INTEGER,   INTENT(   OUT ) :: D
 
     INTEGER,   PARAMETER :: NMAX = 100
     
     INTEGER    :: I, J, K, IMAX
     REAL( 8 )  :: AMAX, DUM, TOTAL
     REAL( 8 )  :: VV( N )


     D    = 1
!     N    = SIZE( A,DIM=1 )

     DO I=1,N
        AMAX=SMALLER ! 0.0D0
        DO J=1,N
           IF (DABS(A(I,J)).GT.AMAX) AMAX=DABS(A(I,J))
        END DO ! j loop
        IF(AMAX.LT.SMALLER) THEN
           WRITE(6,*)'singular matrix in ludcmp'
           STOP
        END IF
        VV(I) = 1.0D0 / AMAX
     END DO ! i loop

     DO J=1,N
        DO I=1,J-1
           TOTAL = A(I,J)
           DO K=1,I-1
              TOTAL = TOTAL - A(I,K)*A(K,J) 
           END DO ! k loop
           A(I,J) = TOTAL
        END DO ! i loop
        AMAX = 0.0D0
        DO I=J,N
           TOTAL = A(I,J)
           DO K=1,J-1
              TOTAL = TOTAL - A(I,K)*A(K,J) 
           END DO ! k loop
           A(I,J) = TOTAL
           DUM = VV(I)*DABS(TOTAL)
           IF(DUM.GE.AMAX) THEN
              IMAX = I
              AMAX = DUM
           END IF
        END DO ! i loop  
   
        IF(J.NE.IMAX) THEN
           DO K=1,N
              DUM = A(IMAX,K)
              A(IMAX,K) = A(J,K)
              A(J,K) = DUM
           END DO ! k loop
           D = -D
           VV(IMAX) = VV(J)
        END IF

        INDX(J) = IMAX
        IF(DABS(A(J,J)) .LT. SMALLER) A(J,J) = SMALLER

        IF(J.NE.N) THEN
           DUM = 1.0D0 / A(J,J)
           DO I=J+1,N
              A(I,J) = A(I,J)*DUM
           END DO ! i loop
        END IF 
     END DO ! j loop

 RETURN
 END Subroutine LUDCMP_F77
 Subroutine LUBKSB_F77(A,N,INDX,B)

!  ******************************************************************
!  * Solves the set of N linear equations A . X = B.  Here A is     *
!  * input, not as the matrix A but rather as its LU decomposition, *
!  * determined by the routine LUDCMP. INDX is input as the permuta-*
!  * tion vector returned by LUDCMP. B is input as the right-hand   *
!  * side vector B, and returns with the solution vector X. A, N and*
!  * INDX are not modified by this routine and can be used for suc- *
!  * cessive calls with different right-hand sides. This routine is *
!  * also efficient for plain matrix inversion.                     *
!  ******************************************************************
!*                 F90 version by J-P Moreau, Paris    *
!*                        (www.jpmoreau.fr)            *
!* --------------------------------------------------- *
!* Reference:                                          *
!*                                                     *
!* "Numerical Recipes By W.H. Press, B. P. Flannery,   *
!*  S.A. Teukolsky and W.T. Vetterling, Cambridge      *
!*  University Press, 1986" [BIBLI 08].                *
!*                                                     *

   IMPLICIT NONE

     REAL( 8 ), INTENT( IN    ) :: A( :,: )
     INTEGER,   INTENT( IN    ) :: N
     INTEGER,   INTENT( IN    ) :: INDX( : )
     REAL( 8 ), INTENT( INOUT ) :: B( : )


     INTEGER    :: I, J, II, LL
     REAL( 8 )  :: TOTAL

      II = 0
      TOTAL = 0.0D0
      DO I = 1, N
         LL = INDX(I)
         TOTAL = B(LL)
         B(LL) = B(I)
         IF (II .NE. 0) THEN
            DO J = II, I-1
               TOTAL = TOTAL - A(I,J)*B(J)
            END DO ! j loop
         ELSE IF ( TOTAL .NE. 0.0D0 ) THEN
            II = I
         END IF
         B(I) = TOTAL
      END DO ! i loop

      DO I = N, 1, -1
         TOTAL = B(I)
         IF(I .LT. N) THEN
            DO J=I+1,N
               TOTAL = TOTAL - A(I,J)*B(J)
            END DO ! j loop
         END IF
         B(I) = TOTAL / A(I,I)
      END DO ! i loop
      RETURN
 End Subroutine LUBKSB_F77

 Subroutine SLUDCMP_F77(A,N,INDX,D)
!  ***************************************************************
!  * Given an N x N matrix A, this routine replaces it by the LU *
!  * decomposition of a rowwise permutation of itself. A and N   *
!  * are input. INDX is an output vector which records the row   *
!  * permutation effected by the partial pivoting; D is output   *
!  * as -1 or 1, depending on whether the number of row inter-   *
!  * changes was even or odd, respectively. This routine is used *
!  * in combination with LUBKSB to solve linear equations or to  *
!  * invert a matrix. Return code is 1, if matrix is singular.   *
!  *************************************************************** 
!*                 F90 version by J-P Moreau, Paris    *
!*                        (www.jpmoreau.fr)            *
!* --------------------------------------------------- *
!* Reference:                                          *
!*                                                     *
!* "Numerical Recipes By W.H. Press, B. P. Flannery,   *
!*  S.A. Teukolsky and W.T. Vetterling, Cambridge      *
!*  University Press, 1986" [BIBLI 08].                *
!*                                                     * 
   IMPLICIT NONE
 
     REAL,    INTENT( INOUT ) :: A( :,: )
     INTEGER, INTENT( IN    ) :: N
     INTEGER, INTENT(   OUT ) :: INDX( : )
     INTEGER, INTENT(   OUT ) :: D
 
     INTEGER,   PARAMETER :: NMAX = 100
     
     INTEGER :: I, J, K, IMAX
     REAL    :: AMAX, DUM, TOTAL
     REAL    :: VV( N )


     D    = 1

     DO I=1,N
        AMAX=SMALL ! 0.0
        DO J=1,N
           IF (ABS(A(I,J)).GT.AMAX) AMAX=ABS(A(I,J))
        END DO ! j loop
        IF(AMAX.LT.SMALL) THEN
           WRITE(6,*)'singular matrix in ludcmp'
           STOP
        END IF
        VV(I) = 1.0 / AMAX
     END DO ! i loop

     DO J=1,N
        DO I=1,J-1
           TOTAL = A(I,J)
           DO K=1,I-1
              TOTAL = TOTAL - A(I,K)*A(K,J) 
           END DO ! k loop
           A(I,J) = TOTAL
        END DO ! i loop
        AMAX = 0.0D0
        DO I=J,N
           TOTAL = A(I,J)
           DO K=1,J-1
              TOTAL = TOTAL - A(I,K)*A(K,J) 
           END DO ! k loop
           A(I,J) = TOTAL
           DUM = VV(I)*ABS(TOTAL)
           IF(DUM.GE.AMAX) THEN
              IMAX = I
              AMAX = DUM
           END IF
        END DO ! i loop  
   
        IF(J.NE.IMAX) THEN
           DO K=1,N
              DUM = A(IMAX,K)
              A(IMAX,K) = A(J,K)
              A(J,K) = DUM
           END DO ! k loop
           D = -D
           VV(IMAX) = VV(J)
        END IF

        INDX(J) = IMAX
        IF(ABS(A(J,J)) .LT. SMALL) A(J,J) = SMALL

        IF(J.NE.N) THEN
           DUM = 1.0 / A(J,J)
           DO I=J+1,N
              A(I,J) = A(I,J)*DUM
           END DO ! i loop
        END IF 
     END DO ! j loop

 RETURN
 END subroutine SLUDCMP_F77
 Subroutine SLUBKSB_F77(A,N,INDX,B)

!  ******************************************************************
!  * Solves the set of N linear equations A . X = B.  Here A is     *
!  * input, not as the matrix A but rather as its LU decomposition, *
!  * determined by the routine LUDCMP. INDX is input as the permuta-*
!  * tion vector returned by LUDCMP. B is input as the right-hand   *
!  * side vector B, and returns with the solution vector X. A, N and*
!  * INDX are not modified by this routine and can be used for suc- *
!  * cessive calls with different right-hand sides. This routine is *
!  * also efficient for plain matrix inversion.                     *
!  ******************************************************************
!*                 F90 version by J-P Moreau, Paris    *
!*                        (www.jpmoreau.fr)            *
!* --------------------------------------------------- *
!* Reference:                                          *
!*                                                     *
!* "Numerical Recipes By W.H. Press, B. P. Flannery,   *
!*  S.A. Teukolsky and W.T. Vetterling, Cambridge      *
!*  University Press, 1986" [BIBLI 08].                *
!*                                                     * 

   IMPLICIT NONE
 
     REAL,    INTENT( IN    ) :: A( :,: )
     INTEGER, INTENT( IN    ) :: N
     INTEGER, INTENT( IN    ) :: INDX( : )
     REAL,    INTENT( INOUT ) :: B( : )
 
     
     INTEGER :: I, J, II, LL
     REAL    :: TOTAL

      II = 0
      TOTAL = 0.0
      DO I = 1, N
         LL = INDX(I)
         TOTAL = B(LL)
         B(LL) = B(I)
         IF (II .NE. 0) THEN
            DO J = II, I-1
               TOTAL = TOTAL - A(I,J)*B(J)
            END DO ! j loop
         ELSE IF ( TOTAL .NE. 0.0 ) THEN
            II = I
         END IF
         B(I) = TOTAL
      END DO ! i loop
     
      DO I = N, 1, -1
         TOTAL = B(I)
         IF(I .LT. N) THEN
            DO J=I+1,N
               TOTAL = TOTAL - A(I,J)*B(J)
            END DO ! j loop
         END IF
         B(I) = TOTAL / A(I,I)
      END DO ! i loop
      RETURN
 End Subroutine SLUBKSB_F77
      SUBROUTINE dp_mprove(a,alud,indx,x)
! Improves a solution vector x of the linear set of equations A·X = B. The N ×N matrix a
! and the N-dimensional vectors b and x are input. Also input is alud, the LU decomposition
! of a as returned by ludcmp, and the N-dimensional vector indx also returned by that
! routine. On output, only x is modified, to an improved set of values.
        IMPLICIT NONE
           REAL,    INTENT(IN   ) :: a(:,:)
           REAL(8), INTENT(IN   ) :: alud(:,:)
           INTEGER, INTENT(IN   ) :: indx(:)
           REAL,    INTENT(INOUT) :: x(:)
           INTEGER :: ndum
           REAL(8) :: r(size(indx))
           REAL(8) :: b(size(indx))
           ndum=size(indx)
           b = x           
           call slubksb_f77(a,ndum,indx,x) ! Solve for x with residue error
           !r=matmul(real(a,16),real(x,16))-real(b,16) ! Calculate accumulating the residual error
           r=matmul(real(a,8),real(x,8))-real(b,8) ! Calculate accumulating the residual error
           call lubksb_f77(alud,ndum,indx,r) ! Solve for the error term,
           x=x-r
           where( x .lt. SMALL )x=SMALL   ! subtract it from the old solution.
     END SUBROUTINE dp_mprove

SUBROUTINE dludcmp_f90(a,n,indx,d)
! Adapted from: NUMERICAL RECIPES IN FORTRAN 90: 
! The Art of PARALLEL Scientific Computing (ISBN 0-521-57439-0)
! Copyright (C) 1986-1996 by Cambridge University Press.

   IMPLICIT NONE
   REAL(8),  INTENT(INOUT) :: a( :,: )
   INTEGER,  INTENT(IN   ) :: n
   INTEGER,  INTENT(  OUT) :: indx( : )
   INTEGER,  INTENT(  OUT) :: d
! Given an NxN input matrix a , this routine replaces it by the LU decomposition of a
! rowwise permutation of itself. On output, a is arranged as in equation (2.3.14); indx is an
! output vector of length N that records the row permutation effected by the partial pivoting;
! d is output as ±1 depending on whether the number of row interchanges was even or odd,
! respectively. This routine is used in combination with lubksb to solve linear equations or
! invert a matrix.

  REAL(8) ::  vv( n )  ! stores the implicit scaling of each row.
  REAL(8) :: dum( n )  ! storage for column swaps
  INTEGER :: jmax( 1 )
  INTEGER :: j,imax
!  INTEGER :: j,n,imax
  
!  n=size(a,1)
  d=1                      ! No row interchanges yet.
  vv=maxval(abs(a),dim=2)  ! Loop over rows to get the implicit scaling
  if (any(vv .eq. 0.0d0) )then !  a row of zeros.
    write(6,*)'singular matrix in ludcmp'
    do j = 1, size( vv )
       write(6,*)j,vv(j)
    end do
    stop
  end if    
  vv=1.0d0/vv ! Save the scaling.
  do j=1,n
     jmax=(j-1)+maxloc(vv(j:n)*abs(a(j:n,j))) ! Find the pivot row
     imax=jmax(1)
     if (j .ne. imax) then ! Need to interchange rows
        dum(:)    = a(imax,:) ! swapping columns
        a(imax,:) = a(j,:)
        a(j,:)    = dum( : )  
        d=-d                  ! change the parity of d.
        vv(imax)=vv(j)        ! Also interchange the scale factor.
    end if
    indx(j)=imax
! If the pivot element is zero the matrix is singular (at least to the precision of the al-
! gorithm). For some applications on singular matrices, it is desirable to substitute TINY
! for zero.
    if (a(j,j) .lt. SMALLER) a(j,j) = SMALLER 
    if( j .ne. n )then
       a(j+1:n,j)=a(j+1:n,j)/a(j,j) ! Divide by the pivot element.
       a(j+1:n,j+1:n)=a(j+1:n,j+1:n)-douterprod( a(j+1:n,j), a(j,j+1:n) ) ! Reduce remaining submatrix.
    end if
  end do
END SUBROUTINE dludcmp_f90
FUNCTION douterprod(a,b)
! Adapted from: NUMERICAL RECIPES IN FORTRAN 90: 
! The Art of PARALLEL Scientific Computing (ISBN 0-521-57439-0)
! Copyright (C) 1986-1996 by Cambridge University Press.
  Implicit None
  REAL(8), INTENT(IN) :: a(:)
  REAL(8), INTENT(IN) :: b(:)
  
  REAL(8) :: douterprod(size(a),size(b))
  
  douterprod = spread(a,dim=2,ncopies=size(b))  &
             * spread(b,dim=1,ncopies=size(a))

END FUNCTION douterprod
 SUBROUTINE dlubksb_f90(a,n,indx,b)
! Adapted from: NUMERICAL RECIPES IN FORTRAN 90: 
! The Art of PARALLEL Scientific Computing (ISBN 0-521-57439-0)
! Copyright (C) 1986-1996 by Cambridge University Press.

! Solves the set of N linear equations A · X = B. Here the N × N matrix a is input, not
! as the original matrix A, but rather as its LU decomposition, determined by the routine
! ludcmp. indx is input as the permutation vector of length N returned by ludcmp . b is
! input as the right-hand-side vector B, also of length N , and returns with the solution vector
! X. a and indx are not modified by this routine and can be left in place for successive calls
! with different right-hand sides b . This routine takes into account the possibility that b will
! begin with many zero elements, so it is efficient for use in matrix inversion.
   implicit none
 
     real( 8 ), intent( in    ) :: a( :,: )
     integer,   intent( in   )  :: n
     integer,   intent( in    ) :: indx( : )
     real( 8 ), intent( inout ) :: b( : )
      
     integer    :: i, j, ii, ll
!     integer    :: i, j, n, ii, ll
     real( 8 )  :: total

     ii = 0                 ! ii set to positive value at  
!     n  = size( a,dim=1 )   ! index of the first nonvanishing element of b.
     do i = 1, n            ! and do the forward substitution. 
        ll    = indx(i)     ! Unscrambling the permutation as the loop proceeds.  
        total = b(ll)
        b(ll) = b(i)
        if (ii .ne. 0) then
           total = total - dot_product(a(i,ii:i-1),b(ii:i-1))
        else if ( total .ne. 0.0d0 ) then
           ii = i  ! nonzero element encountered so
        end if     ! use to do the dot product above
        b(i) = total
     end do ! i loop

     do i = n, 1, -1 ! do the backsubstitution
        b(i) = (b(i)-dot_product(a(i,i+1:n),b(i+1:n)))/a(i,i)
     end do ! i loop

     return
 END Subroutine dlubksb_f90
SUBROUTINE sludcmp_f90(a,n,indx,d)
! Adapted from: NUMERICAL RECIPES IN FORTRAN 90: 
! The Art of PARALLEL Scientific Computing (ISBN 0-521-57439-0)
! Copyright (C) 1986-1996 by Cambridge University Press.

   IMPLICIT NONE
   REAL(4),  INTENT(INOUT) :: a( :,: )
   INTEGER,  INTENT(IN   ) :: n
   INTEGER,  INTENT(  OUT) :: indx( : )
   INTEGER,  INTENT(  OUT) :: d
! Given an NxN input real4 matrix a , this routine replaces it by the LU decomposition of a
! rowwise permutation of itself. On output, a is arranged as in equation (2.3.14); indx is an
! output vector of length N that records the row permutation effected by the partial pivoting;
! d is output as ±1 depending on whether the number of row interchanges was even or odd,
! respectively. This routine is used in combination with lubksb to solve linear equations or
! invert a matrix.

  REAL(8) ::  vv( n )           ! stores the implicit scaling of each row.
  REAL(8) :: dum( n )           ! storage for column swaps
!  REAL(8) ::  vv( size(a,1) )  ! stores the implicit scaling of each row.
!  REAL(8) :: dum( size(a,1) )  ! storage for column swaps
  INTEGER :: jmax( 1 )
  INTEGER :: j,imax
!  INTEGER :: j,n,imax
  
!  n=size(a,1)
  d=1                      ! No row interchanges yet.
  vv=maxval(abs(a),dim=2)  ! Loop over rows to get the implicit scaling
  if (any(vv .eq. 0.0d0) )then !  a row of zeros.
    write(6,*)'singular matrix in ludcmp'
    do j = 1, size( vv )
       write(6,*)j,vv(j)
    end do
    stop
  end if    
  vv=1.0d0/vv ! Save the scaling.
  do j=1,n
     jmax=(j-1)+maxloc(vv(j:n)*abs(a(j:n,j))) ! Find the pivot row
     imax=jmax(1)
     if (j .ne. imax) then ! Need to interchange rows
        dum(:)    = a(imax,:) ! swapping columns
        a(imax,:) = a(j,:)
        a(j,:)    = dum( : )  
        d=-d                  ! change the parity of d.
        vv(imax)=vv(j)        ! Also interchange the scale factor.
    end if
    indx(j)=imax
! If the pivot element is zero the matrix is singular (at least to the precision of the al-
! gorithm). For some applications on singular matrices, it is desirable to substitute TINY
! for zero.
    if (a(j,j) .lt. small) a(j,j)= small
    if( j .ne. n )then
       a(j+1:n,j)=a(j+1:n,j)/a(j,j) ! Divide by the pivot element.
       a(j+1:n,j+1:n)=a(j+1:n,j+1:n)-souterprod( a(j+1:n,j), a(j,j+1:n) ) ! Reduce remaining submatrix.
    end if
  end do
END SUBROUTINE sludcmp_f90
FUNCTION souterprod(a,b)
! Adapted from: NUMERICAL RECIPES IN FORTRAN 90: 
! The Art of PARALLEL Scientific Computing (ISBN 0-521-57439-0)
! Copyright (C) 1986-1996 by Cambridge University Press.
  Implicit None
  REAL(4), INTENT(IN) :: a(:)
  REAL(4), INTENT(IN) :: b(:)
  
  REAL(4) :: souterprod(size(a),size(b))
  
  souterprod = spread(a,dim=2,ncopies=size(b))  &
             * spread(b,dim=1,ncopies=size(a))

END FUNCTION souterprod
 SUBROUTINE slubksb_f90(a,n,indx,b)
! Adapted from: NUMERICAL RECIPES IN FORTRAN 90: 
! The Art of PARALLEL Scientific Computing (ISBN 0-521-57439-0)
! Copyright (C) 1986-1996 by Cambridge University Press.

! Solves the set of N linear equations A · X = B. Here the N × N matrix a is input, not
! as the original matrix A, but rather as its LU decomposition, determined by the routine
! ludcmp. indx is input as the permutation vector of length N returned by ludcmp . b is
! input as the right-hand-side vector B, also of length N , and returns with the solution vector
! X. a and indx are not modified by this routine and can be left in place for successive calls
! with different right-hand sides b . This routine takes into account the possibility that b will
! begin with many zero elements, so it is efficient for use in matrix inversion.
   implicit none
 
     real( 4 ), intent( in    ) :: a( :,: )
     integer,   intent( in   )  :: n
     integer,   intent( in    ) :: indx( : )
     real( 4 ), intent( inout ) :: b( : )
      
     integer    :: i, j, ii, ll
!     integer    :: i, j, n, ii, ll
     real( 8 )  :: total

     ii = 0                 ! ii set to positive value at  
!     n  = size( a,dim=1 )   ! index of the first nonvanishing element of b.
     do i = 1, n            ! and do the forward substitution. 
        ll    = indx(i)     ! Unscrambling the permutation as the loop proceeds.  
        total = b(ll)
        b(ll) = b(i)
        if (ii .ne. 0) then
           total = total - dot_product(a(i,ii:i-1),b(ii:i-1))
        else if ( total .ne. 0.0d0 ) then
           ii = i  ! nonzero element encountered so
        end if     ! use to do the dot product above
        b(i) = total
     end do ! i loop

     do i = n, 1, -1 ! do the backsubstitution
        b(i) = (b(i)-dot_product(a(i,i+1:n),b(i+1:n)))/a(i,i)
     end do ! i loop

     return
 END Subroutine slubksb_f90


END MODULE LU_DEcompositionSolver
