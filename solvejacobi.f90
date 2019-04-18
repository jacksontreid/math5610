!Computes the solution of a square linear system using Jacobi iteration.
!@author: Jackson Reid


SUBROUTINE solvejacobi(A,n,b,x0,tol,maxiter,text,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n, maxiter
    REAL*8, INTENT(in) :: A(n,n), b(n), x0(n), tol
    LOGICAL, INTENT(in) :: text
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: A_LU(n,n), D(n), xold(n), y(n), error
    INTEGER :: i, iter

    !Split the coefficient matrix (A = [L + U] + [D])
    A_LU = A
    DO i = 1,n
        D(i) = 1.0d0/A(i,i)
        A_LU(i,i) = 0.0d0
    END DO

    error = 10.0d0*tol
    iter = 0
    xold = x0

    DO WHILE (error > tol .AND. iter < maxiter)

        CALL multmat(A_LU,xold,n,n,1,y)

        x = D*(b - y)

        iter = iter + 1

        CALL norm2abserr(x,xold,n,error)

        xold = x

        IF (text) THEN
            WRITE(*,*) iter, error
        END IF

    END DO

END SUBROUTINE
