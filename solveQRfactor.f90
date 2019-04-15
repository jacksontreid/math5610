!Computes the solution of a square linear system using QR-decomposition,
!matrix-vector multiplication, and back-substitution.
!@author: Jackson Reid


SUBROUTINE solveQRfactor(A,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(in) :: b(n), A(n,n)
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: Q(n,n), R(n,n), y(n)
    INTEGER :: i

    !Perform QR-decomposition on the matrix
    CALL QRdecomphouse(A,n,Q,R)

    !Use matrix-vector multiplication to modify the right-hand side
    CALL multmat(TRANSPOSE(Q),b,n,n,1,y)

    !Use back-substitution to determine the system solution
    CALL backsub(R,n,y,x)

END SUBROUTINE
