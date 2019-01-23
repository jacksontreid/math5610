!Scales a vector by a constant.
!@author: Jackson Reid


SUBROUTINE scalevec(vec, length, const)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, INTENT(in) :: const
    REAL*8, DIMENSION(length), INTENT(inout) :: vec
    INTEGER :: i

    DO i = 1, length
        vec(i) = const*vec(i)
    END DO

END SUBROUTINE
