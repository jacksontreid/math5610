!Computes the solution of a square linear system using Gauss-Seidel iteration.
!@author: Jackson Reid


SUBROUTINE solvegaussseidel(A,n,b,x0,tol,maxiter,text,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n, maxiter
    REAL*8, INTENT(in) :: A(n,n), b(n), x0(n), tol
    LOGICAL, INTENT(in) :: text
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: A_U(n,n), L(n,n), xold(n), y(n), error
    INTEGER :: i, iter

    !Split the coefficient matrix (A = [L + D] + [U])
    L(:,:) = 0.0d0
    A_U(:,:) = 0.0d0
    DO i = 1,n
        L(i,:i) = A(i,:i)
        A_U(i,i+1:) = A(i,i+1:)
    END DO

    error = 10.0d0*tol
    iter = 0
    xold = x0

    DO WHILE (error > tol .AND. iter < maxiter)

        CALL multmat(A_U,xold,n,n,1,y)

        y = b - y

        CALL forsub(L,n,y,x)

        iter = iter + 1

        CALL norm2abserr(x,xold,n,error)

        xold = x

        IF (text) THEN
            WRITE(*,*) iter, error
        END IF

    END DO

END SUBROUTINE
