!Scales a matrix by a constant.
!@author: Jackson Reid


SUBROUTINE scalemat(mat,r,c,const)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, c
    REAL*8, INTENT(in) :: const
    REAL*8, DIMENSION(r,c), INTENT(inout) :: mat
    INTEGER :: i, j

    DO i = 1, r
        DO j = 1,c
            mat(i,j) = const*mat(i,j)
        END DO
    END DO

END SUBROUTINE
