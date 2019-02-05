!Computes the dot product of two vectors of the same length.
!@author: Jackson Reid


SUBROUTINE dotvec(vec1, vec2, length, dot)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, DIMENSION(length), INTENT(in) :: vec1, vec2
    REAL*8, INTENT(out) :: dot
    INTEGER :: i

    dot = 0.0d0

    DO i = 1, length
        dot = dot + vec1(i)*vec2(i)
    END DO

END SUBROUTINE
