!Computes the outer product of two vectors.
!@author: Jackson Reid


SUBROUTINE outervec(vec1, r, vec2, c, outer)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, c
    REAL*8, INTENT(in) :: vec1(r), vec2(c)
    REAL*8, INTENT(out) :: outer(r,c)
    INTEGER :: i, j

    DO i = 1, r
        DO j = 1, c

            outer(i,j) = vec1(i)*vec2(j)

        END DO
    END DO

END SUBROUTINE
