!Computes the 1-norm of a vector.
!@author: Jackson Reid


SUBROUTINE norm1vec(vec, length, norm)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, DIMENSION(length), INTENT(in) :: vec
    REAL*8, INTENT(out) :: norm
    INTEGER :: i

    norm = 0.0d0

    DO i = 1, length
        norm = norm + DABS(vec(i))
    END DO

END SUBROUTINE
