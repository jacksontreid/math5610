!Computes the solution of a square linear system with an upper triangular
!coefficient matrix, using back-substitution.
!@author: Jackson Reid


SUBROUTINE backsub(A,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(in) :: A(n,n), b(n)
    REAL*8, INTENT(out) :: x(n)
    INTEGER :: i, j

    x(n) = b(n)/A(n,n)

    DO i = n-1,1,-1
        x(i) = b(i)
        DO j = i+1,n
            x(i) = x(i) - x(j)*A(i,j)
        END DO
        x(i) = x(i)/A(i,i)
    END DO

END SUBROUTINE
