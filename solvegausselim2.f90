!Computes the solution of a square linear system using Gaussian elimination
!and back-substitution, without calling external routines.
!@author: Jackson Reid


SUBROUTINE solvegausselim2(A,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(in) :: A(n,n), b(n)
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: A_aug(n,n+1), factor
    INTEGER :: i,j,k

    !Create augmented coefficient matrix
    A_aug(:,1:n) = A(:,:)
    A_aug(:,n+1) = b(:)

    !Reduce augmented coefficient matrix to row echelon form
    DO k = 1,n-1
        DO i = k+1,n
            factor = A_aug(i,k)/A_aug(k,k)
            DO j = k,n+1
                A_aug(i,j) = A_aug(i,j) - A_aug(k,j)*factor
            END DO
        END DO
    END DO

    !Use back-substitution to determine the system solution
    x(n) = A_aug(n,n+1)/A_aug(n,n)
    DO i = n-1,1,-1
        x(i) = A_aug(i,n+1)
        DO j = i+1,n
            x(i) = x(i) - x(j)*A_aug(i,j)
        END DO
        x(i) = x(i)/A_aug(i,i)
    END DO

END SUBROUTINE
