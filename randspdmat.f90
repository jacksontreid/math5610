!Generates a random symmetric-positive-definite matrix of size (n,n).
!@author Jackson Reid

SUBROUTINE randspdmat(n,mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(out) :: mat(n,n)
    REAL*8 :: tempmat(n,n)
    INTEGER :: error

    error = 1

    DO WHILE (error == 1)
        !Fill matrix with random numbers
        CALL RANDOM_NUMBER(tempmat)

        !Multiply random matrix by its transpose
        CALL multmat(tempmat,TRANSPOSE(tempmat),n,n,n,mat)

        !Check that matrix is positive definate
        tempmat = mat
        CALL choldecomp(tempmat,n,error)
    END DO

END SUBROUTINE

