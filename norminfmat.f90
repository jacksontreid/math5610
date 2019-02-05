!Computes the induced infinity-norm of a square matrix.
!@author: Jackson Reid

SUBROUTINE norminfmat(mat, n, norm)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, DIMENSION(n,n), INTENT(in) :: mat
    REAL*8, INTENT(out) :: norm
    INTEGER :: i, j
    REAL*8 :: num

    norm = 0.0d0

    DO i = 1, n
        !Calculate the 1-norm of each row
        num = 0.0d0
        DO j = 1, n
            num = num + DABS(mat(i,j))
        END DO

        !Determine maximum row 1-norm
        IF (num > norm) THEN
            norm = num
        END IF
    END DO

END SUBROUTINE
