!Performs elementary row operations to reduce a matrix to row echelon form.
!@author: Jackson Reid


SUBROUTINE rowechelon(A,r,c)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, c
    REAL*8, INTENT(inout) :: A(r,c)
    REAL*8 :: factor
    INTEGER :: i, j, k

    DO k = 1,r-1
        DO i = k+1,r
            factor = A(i,k)/A(k,k)
            DO j = k,c
                A(i,j) = A(i,j) - A(k,j)*factor
            END DO
        END DO
    END DO

END SUBROUTINE
