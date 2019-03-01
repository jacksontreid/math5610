!Computes the solution of a square linear system with an lower triangular
!coefficient matrix, using forward-substitution.
!@author: Jackson Reid


SUBROUTINE forsub(A,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(in) :: A(n,n), b(n)
    REAL*8, INTENT(out) :: x(n)
    INTEGER :: i, j

    x(1) = b(1)/A(1,1)

    DO i = 2,n
        x(i) = b(i)
        DO j = 1,i-1
            x(i) = x(i) - x(j)*A(i,j)
        END DO
        x(i) = x(i)/A(i,i)
    END DO

END SUBROUTINE
