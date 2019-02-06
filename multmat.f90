!Multiplies two matrices, of size (r,n) and (n,c), to generate a matrix (r,c).
!@author Jackson Reid

SUBROUTINE multmat(mat1,mat2,r,n,c,new_mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, n, c
    REAL*8, INTENT(in) :: mat1(r,n), mat2(n,c)
    REAL*8, INTENT(out) :: new_mat(r,c)
    INTEGER :: i, j, k
    REAL*8 :: num

    DO i = 1,r
        DO j = 1,c

            num = 0.0d0

            DO k = 1,n
                 num = num + mat1(i,k)*mat2(k,j)
            END DO

            new_mat(i,j) = num

        END DO
    END DO

END SUBROUTINE

