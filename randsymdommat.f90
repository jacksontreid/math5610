!Generates a diagonally dominant, symmetric matrix of size (n,n) populated with
!random values, [0,1).
!@author Jackson Reid

SUBROUTINE randsymdommat(n,mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(out) :: mat(n,n)
    INTEGER :: i, j

    !Fill matrix
    DO i = 1, n

        !Fill upper triangle with random numbers
        CALL RANDOM_NUMBER(mat(i,i:n))

        !Copy values into lower triangle
        mat(i+1:n,i) = mat(i,i+1:n)

    END DO

    !Ensure diagonal dominance
    DO i = 1,n
        DO j = 1,n
            mat(i,i) = mat(i,i) + mat(i,j)
        END DO
    END DO

END SUBROUTINE

