!Calculates the relative error between two values.
!@author: Jackson Reid


SUBROUTINE relerr(approx, exact, error)
    IMPLICIT NONE

    REAL*8, INTENT(in) :: approx, exact
    REAL*8, INTENT(out) :: error

    error = DABS((approx - exact)/exact)

END SUBROUTINE
