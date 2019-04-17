!Calculates an approximate solution of a least squares problem, using
!Householder QR factorization.
!@author Jackson Reid

SUBROUTINE lsQRhouse(A,m,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: m, n
    REAL*8, INTENT(in) :: A(m,n), b(m)
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: y(m), Q(m,m), R(m,n)
    INTEGER :: i

    !Compute QR factorization of A
    CALL QRdecomphouse(A,m,n,Q,R)

    !Multiply the system by Q^T
    CALL multmat(TRANSPOSE(Q),b,m,m,1,y)

    !Solve the upper-triangular system
    CALL backsub(R(1:n,:),n,y(1:n),x)

END SUBROUTINE
