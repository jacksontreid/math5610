!Computes the infinity-norm of a vector.
!@author: Jackson Reid


SUBROUTINE norminfvec(vec, length, norm)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, DIMENSION(length), INTENT(in) :: vec
    REAL*8, INTENT(out) :: norm
    INTEGER :: i
    REAL*8 :: num

    norm = 0.0d0

    DO i = 1, length
        num = DABS(vec(i))
        IF (num > norm) THEN
            norm = num
        END IF
    END DO

END SUBROUTINE
