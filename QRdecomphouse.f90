!Calculates the QR decomposition of a matrix using Householder transformations.
!@author: Jackson Reid


SUBROUTINE QRdecomphouse(A,n,Q,R)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(in) :: A(n,n)
    REAL*8, INTENT(out) :: Q(n,n), R(n,n)
    REAL*8 :: factor
    INTEGER :: i, j

    R = 0.0d0

    DO j = 1,n
        Q(:,j) = A(:,j)
        DO i = 1,j-1
            CALL dotvec(Q(:,j),Q(:,i),n,R(i,j))
            Q(:,j) = Q(:,j) - R(i,j)*Q(:,i)
        END DO
        CALL norm2vec(Q(:,j),n,R(j,j))
        Q(:,j) = Q(:,j)/R(j,j)
    END DO

END SUBROUTINE
