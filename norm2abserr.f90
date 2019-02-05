!Computes the 2-norm of the absolute error between two vectors.
!@author: Jackson Reid

SUBROUTINE norm2abserr(approx, exact, length, error)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, DIMENSION(length), INTENT(in) :: approx, exact
    REAL*8, INTENT(out) :: error
    INTEGER :: i

    error = 0.0d0

    DO i = 1, length
        error = error + DABS(approx(i) - exact(i))**2
    END DO

    error = DSQRT(error)

END SUBROUTINE
