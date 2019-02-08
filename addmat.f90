!Adds two matrices of the same size.
!@author: Jackson Reid


SUBROUTINE addmat(mat1, mat2, r, c, new_mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, c
    REAL*8, DIMENSION(r,c), INTENT(in) :: mat1, mat2
    REAL*8, DIMENSION(r,c), INTENT(out) :: new_mat
    INTEGER :: i, j

    DO i = 1, r
        DO j = 1, c
            new_mat(i,j) = mat1(i,j) + mat2(i,j)
        END DO
    END DO

END SUBROUTINE
