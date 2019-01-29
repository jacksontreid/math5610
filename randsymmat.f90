!Generates a symmetric matrix of size (n,n) populated with random values, [0,1).
!@author Jackson Reid

SUBROUTINE randsymmat(n,mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(out) :: mat(n,n)
    INTEGER :: i

    !Fill matrix
    DO i = 1, n

        !Fill upper triangle with random numbers
        CALL RANDOM_NUMBER(mat(i,i:n))

        !Copy values into lower triangle
        mat(i+1:n,i) = mat(i,i+1:n)

    END DO

END SUBROUTINE

