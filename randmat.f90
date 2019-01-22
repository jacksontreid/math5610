!Generates a matrix of size (r,c) populated with random values, [0,1).
!@author Jackson Reid

SUBROUTINE randmat(r,c,mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r,c
    REAL*8, INTENT(out) :: mat(r,c)

    !Fill matrix with random numbers
    CALL RANDOM_NUMBER(mat)

END SUBROUTINE

