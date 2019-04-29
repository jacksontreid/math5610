!Approximates the condition number of a symmetric, positive-definite matrix
!using Rayleigh Quotient Iteration.
!@author: Jackson Reid


SUBROUTINE matcond2(A,n,guess,tol,maxiter,l1,ln,cond)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n, guess, maxiter
    REAL*8, INTENT(in) :: A(n,n), tol
    REAL*8, INTENT(out) :: l1, ln, cond
    REAL*8 :: v0(n), v(n), l
    INTEGER :: i

    DO i = 1, guess
        !Generate guess vector
        CALL randvec(n,v0)

        !Calculate the eigenvalue
        CALL eigrayleigh(A,n,v0,tol,maxiter,l,v)

        IF (ISNAN(l)) THEN
            CONTINUE
        ELSE IF (i == 1) THEN
            l1 = l
            ln = l
        ELSE IF (ABS(l) > ABS(l1)) THEN
            l1 = l
        ELSE IF (ABS(l) < ABS(ln)) THEN
            ln = l
        END IF

    END DO

    cond = l1/ln

END SUBROUTINE
