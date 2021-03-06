!Calculates the QR decomposition of a matrix using classical Gram-Schmidt.
!@author: Jackson Reid


SUBROUTINE QRdecomp(A,m,n,Q,R)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: m, n
    REAL*8, INTENT(in) :: A(m,n)
    REAL*8, INTENT(out) :: Q(m,n), R(n,n)
    REAL*8 :: factor
    INTEGER :: i, j

    R = 0.0d0

    DO j = 1,n
        Q(:,j) = A(:,j)
        DO i = 1,j-1
            CALL dotvec(A(:,j),Q(:,i),m,R(i,j))
            Q(:,j) = Q(:,j) - R(i,j)*Q(:,i)
        END DO
        CALL norm2vec(Q(:,j),m,R(j,j))
        Q(:,j) = Q(:,j)/R(j,j)
    END DO

END SUBROUTINE
