!Computes the solution of a square linear system using Gaussian elimination
!and back-substitution.
!@author: Jackson Reid


SUBROUTINE solvegausselim(A,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(in) :: A(n,n), b(n)
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: A_aug(n,n+1)
    INTEGER :: i

    !Create augmented coefficient matrix
    A_aug(:,1:n) = A(:,:)
    A_aug(:,n+1) = b(:)

    !Reduce augmented coefficient matrix to row echelon form
    CALL rowechelon(A_aug,n,n+1)

    !Use back-substitution to determine the system solution
    CALL backsub(A_aug(:,1:n),n,A_aug(:,n+1),x)

END SUBROUTINE
