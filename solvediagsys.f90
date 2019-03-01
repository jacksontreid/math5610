!Computes the solution of a square linear system with a diagonal
!coefficient matrix.
!@author: Jackson Reid


SUBROUTINE solvediagsys(A,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(in) :: A(n,n), b(n)
    REAL*8, INTENT(out) :: x(n)
    INTEGER :: i

    DO i = 1, n
        x(i) = b(i)/A(i,i)
    END DO

END SUBROUTINE
