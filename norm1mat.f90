!Computes the induced 1-norm of a square matrix.
!@author: Jackson Reid

SUBROUTINE norm1mat(mat, n, norm)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, DIMENSION(n,n), INTENT(in) :: mat
    REAL*8, INTENT(out) :: norm
    INTEGER :: i, j
    REAL*8 :: num

    norm = 0.0d0

    DO i = 1, n
        !Calculate the 1-norm of each column
        num = 0.0d0
        DO j = 1, n
            num = num + DABS(mat(j,i))
        END DO

        !Determine maximum column 1-norm
        IF (num > norm) THEN
            norm = num
        END IF
    END DO

END SUBROUTINE
