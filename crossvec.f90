!Computes the cross product of two vectors of length three.
!@author: Jackson Reid


SUBROUTINE crossvec(vec1, vec2, new_vec)
    IMPLICIT NONE

    REAL*8, DIMENSION(3), INTENT(in) :: vec1, vec2
    REAL*8, DIMENSION(3), INTENT(out) :: new_vec

    new_vec(1) = vec1(2)*vec2(3) - vec1(3)*vec2(2)
    new_vec(2) = vec1(3)*vec2(1) - vec1(1)*vec2(3)
    new_vec(3) = vec1(1)*vec2(2) - vec1(2)*vec2(1)

END SUBROUTINE
