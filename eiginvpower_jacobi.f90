!Computes the smallest (or other), real eigenvalue of a matrix using the inverse
!Power Method with Jacobi Iteration.
!@author: Jackson Reid


SUBROUTINE eiginvpower_jacobi(A,n,v0,alpha,tol,maxiter,l,v)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n, maxiter
    REAL*8, INTENT(in) :: A(n,n), v0(n), alpha, tol
    REAL*8, INTENT(out) :: l, v(n)
    REAL*8 :: A_LU(n,n), v1(n), vguess(n), vnorm, lold, error
    INTEGER :: i, iter

    error = 10.0d0*tol
    iter = 0
    lold = 0.0d0

    A_LU = A
    DO i = 1,n
        A_LU(i,i) = A_LU(i,i) - alpha
    END DO
    v1 = v0

    DO WHILE (error > tol .AND. iter < maxiter)

        vguess = v1
        CALL solvejacobi(A_LU,n,v1,vguess,tol,maxiter,.FALSE.,v)

        CALL norm2vec(v,n,vnorm)

        v1 = v/vnorm

        CALL multmat(A,v1,n,n,1,v)

        CALL dotvec(v1,v,n,l)

        error = ABS(l - lold)

        lold = l

        iter = iter + 1

    END DO

    IF (iter == maxiter) THEN
        WRITE(*,*)
        WRITE(*,*) "Iteration maximum reached! Error =", error
        WRITE(*,*)
    END IF

    v = v1/MAXVAL(ABS(v1))

END SUBROUTINE
