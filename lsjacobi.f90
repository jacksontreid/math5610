!Calculates an approximate solution of a least squares problem, using the
!normal equations with a Jacobi solver.
!@author Jackson Reid

SUBROUTINE lsjacobi(A,r,c,b,x0,tol,maxiter,text,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, c, maxiter
    REAL*8, INTENT(in) :: A(r,c), b(r), x0(c), tol
    LOGICAL, INTENT(in) :: text
    REAL*8, INTENT(out) :: x(c)
    REAL*8 :: Asq(c,c), A_LU(c,c), D(c), ATb(r), alpha(c), xold(c), y(c), error
    INTEGER :: i, iter

    !Multiply the system by A^T
    CALL multmat(TRANSPOSE(A),A,c,r,c,Asq)
    CALL multmat(TRANSPOSE(A),b,c,r,1,ATb)

    !Generate conditioning array
    alpha = 0.0d0
    DO i = 1,c
        alpha = alpha + Asq(:,i)
    END DO
    alpha = alpha

    !Split the coefficient matrix (A = [L - aI + U] + [D + aI])
    A_LU = Asq
    DO i = 1,c
        D(i) = 1.0d0/(Asq(i,i) + alpha(i))
        A_LU(i,i) = -alpha(i)
    END DO

    error = 10.0d0*tol
    iter = 0
    xold = x0

    !Perform Jacobi Iteration
    DO WHILE (error > tol .AND. iter < maxiter)

        CALL multmat(A_LU,xold,c,c,1,y)

        x = D*(ATb - y)

        iter = iter + 1

        CALL norm2abserr(x,xold,c,error)

        xold = x

        IF (text) THEN
            WRITE(*,*) iter, error
        END IF

    END DO


END SUBROUTINE


