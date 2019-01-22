!Generates a vector of size (r) populated with random values, [0,1).
!@author Jackson Reid

SUBROUTINE randvec(r,vec)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r
    REAL*8, INTENT(out) :: vec(r)

    !Fill vector with random numbers
    CALL RANDOM_NUMBER(vec)

END SUBROUTINE

