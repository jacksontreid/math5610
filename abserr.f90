!Calculates the absolute error between two values.
!@author: Jackson Reid


SUBROUTINE abserr(approx, exact, error)
    IMPLICIT NONE

    REAL*8, INTENT(in) :: approx, exact
    REAL*8, INTENT(out) :: error

    error = DABS(approx - exact)

END SUBROUTINE
