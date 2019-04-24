!Computes the solution of a square linear system using conjugate gradient.
!@author: Jackson Reid


SUBROUTINE solveconjgrad(A,n,b,x0,tol,maxiter,text,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n, maxiter
    REAL*8, INTENT(in) :: A(n,n), b(n), x0(n), tol
    LOGICAL, INTENT(in) :: text
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: x1(n), p(n), r(n), r1(n), s(n), d1, d2, alpha, bb, error
    INTEGER :: iter

    error = 10.0d0*tol
    iter = 0
    x = x0

    CALL dotvec(b,b,n,bb)

    CALL multmat(A,x,n,n,1,s)
    r = b - s
    CALL dotvec(r,r,n,d1)
    p = r

    DO WHILE (error > tol .AND. iter < maxiter)

        CALL multmat(A,p,n,n,1,s)

        CALL dotvec(p,s,n,d2)

        alpha = d1/d2

        x1 = x + alpha*p

        r1 = r - alpha*s

        CALL dotvec(r1,r1,n,d2)

        p = r1 + d2*p/d1

        r = r1

        x = x1

        d1 = d2

        iter = iter + 1

        error = DSQRT(d1/bb)

        IF (text) THEN
            WRITE(*,*) iter, error
        END IF

    END DO

END SUBROUTINE
