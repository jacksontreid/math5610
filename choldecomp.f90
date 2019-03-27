!Calculates the Cholesky decomposition of a matrix.
!@author: Jackson Reid


SUBROUTINE choldecomp(A,n)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(inout) :: A(n,n)
    INTEGER :: i, j, k

    DO k = 1,n-1
        !Check for decomposition failure
        IF (A(k,k) <= 0.0d0) THEN
            WRITE(*,*) "Cholesky Decomposition FAILURE!"
            RETURN
        END IF
        A(k,k) = SQRT(A(k,k))

        DO i = k+1,n
            A(i,k) = A(i,k)/A(k,k)
        END DO

        DO j = k+1,n
            DO i = j,n
                A(i,j) = A(i,j) - A(i,k)*A(j,k)
            END DO
        END DO
    END DO

    !Check for decomposition failure
    IF (A(k,k) < 0.0d0) THEN
        WRITE(*,*) "Cholesky Decomposition FAILURE!"
        RETURN
    END IF
    A(n,n) = SQRT(A(n,n))

END SUBROUTINE
