!Calculates the LU decomposition of a matrix.
!@author: Jackson Reid


SUBROUTINE LUdecomp(A,n)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(inout) :: A(n,n)
    REAL*8 :: factor
    INTEGER :: i, j, k

    DO k = 1,n-1
        DO i = k+1,n
            factor = A(i,k)/A(k,k)
            DO j = k+1,n
                A(i,j) = A(i,j) - A(k,j)*factor
            END DO
            A(i,k) = factor
        END DO
    END DO

END SUBROUTINE
