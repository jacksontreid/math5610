!Adds two vectors of the same length.
!@author: Jackson Reid


SUBROUTINE addvec(vec1, vec2, length, new_vec)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, DIMENSION(length), INTENT(in) :: vec1, vec2
    REAL*8, DIMENSION(length), INTENT(out) :: new_vec
    INTEGER :: i

    DO i = 1, length
        new_vec(i) = vec1(i) + vec2(i)
    END DO

END SUBROUTINE
