!Calculates an approximate solution of a least squares problem, using
!modified Gram-Schmidt QR factorization.
!@author Jackson Reid

SUBROUTINE lsQRgsmod(A,m,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: m, n
    REAL*8, INTENT(in) :: A(m,n), b(m)
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: y(n), Q(m,n), R(n,n)
    INTEGER :: i

    !Compute QR factorization of A
    CALL QRdecompmod(A,m,n,Q,R)

    !Multiply the system by Q^T
    CALL multmat(TRANSPOSE(Q),b,n,m,1,y)

    !Solve the upper-triangular system
    CALL backsub(R,n,y,x)

END SUBROUTINE
