!Approximates the condition number of a symmetric, positive-definite matrix
!using the power and inverse power methods.
!@author: Jackson Reid


SUBROUTINE matcond(A,n,v0,tol,maxiter,l1,ln,cond)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n, maxiter
    REAL*8, INTENT(in) :: A(n,n), v0(n), tol
    REAL*8, INTENT(out) :: l1, ln, cond
    REAL*8 :: v1(n), vn(n)

    !Calculate the largest eigenvalue
    CALL eigpower(A,n,v0,tol,maxiter,l1,v1)

    !Calculate the smallest eigenvalue
    CALL eiginvpower(A,n,v0,0.0d0,tol,maxiter,ln,vn)

    cond = l1/ln

END SUBROUTINE
