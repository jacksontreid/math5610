!Computes the infinity-norm of the absolute error between two vectors.
!@author: Jackson Reid

SUBROUTINE norminfabserr(approx, exact, length, error)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, DIMENSION(length), INTENT(in) :: approx, exact
    REAL*8, INTENT(out) :: error
    INTEGER :: i
    REAL*8 :: num

    error = 0.0d0

    DO i = 1, length
        num = DABS(approx(i) - exact(i))
        IF (num > error) THEN
            error = num
        END IF
    END DO

END SUBROUTINE
