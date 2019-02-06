!Generates a diagonally dominant matrix of size (n,n) populated with random
!values, [0,1), on the off-diagonals.
!@author Jackson Reid

SUBROUTINE randdommat(n,mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(out) :: mat(n,n)
    INTEGER :: i, j

    !Fill matrix with random numbers
    CALL RANDOM_NUMBER(mat)

    !Ensure diagonal dominance
    DO i = 1,n
        DO j = 1,n
            mat(i,i) = mat(i,i) + mat(i,j)
        END DO
    END DO

END SUBROUTINE

