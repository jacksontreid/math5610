!Computes the infinity-norm of a vector.
!@author: Jackson Reid


SUBROUTINE norminfvec(vec, length, norm)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, DIMENSION(length), INTENT(in) :: vec
    REAL*8, INTENT(out) :: norm
    INTEGER :: i
    REAL*8 :: num

    norm = 0.

    DO i = 1, length
        num = vec(i)
        IF (num > norm) THEN
            norm = num
        END IF
    END DO

END SUBROUTINE
