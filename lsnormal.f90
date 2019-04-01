!Calculates an approximate solution of a least squares problem, using the
!normal equations.
!@author Jackson Reid

SUBROUTINE lsnormal(A,r,c,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, c
    REAL*8, INTENT(in) :: A(r,c), b(r)
    REAL*8, INTENT(out) :: x(c)
    REAL*8 :: Asq(c,c), y(c)
    INTEGER :: error

    !Multiply the system by A^T
    CALL multmat(TRANSPOSE(A),A,c,r,c,Asq)
    CALL multmat(TRANSPOSE(A),b,c,r,1,x)

    !Compute Cholesky factorization of Asq
    CALL choldecomp(Asq,c,error)

    !Solve the lower-triangular system
    CALL forsub(Asq,c,x,y)

    !Solve the upper-triangular system
    CALL backsub(TRANSPOSE(Asq),c,y,x)

END SUBROUTINE
