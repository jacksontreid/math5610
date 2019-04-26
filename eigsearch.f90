!Performs a blind search for the real eigenvalues 1 < l < n (for a spd matrix).
!@author: Jackson Reid


SUBROUTINE eigsearch(A,n,v0,tol,maxiter,search)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n, maxiter, search
    REAL*8, INTENT(in) :: A(n,n), v0(n), tol
    REAL*8 :: interval, alpha, l1, ln, l, v1(n), vn(n), v(n)
    INTEGER :: i, j, iter

    !Calculate the largest eigenvalue
    CALL eigpower(A,n,v0,tol,maxiter,l1,v1)
    WRITE(*,*) "l1:", l1

    !Calculate the smallest eigenvalue
    CALL eiginvpower(A,n,v0,0.0d0,tol,maxiter,ln,vn)
    WRITE(*,*) "ln:", ln
    WRITE(*,*)
    WRITE(*,*) "Search location (alpha)", "       Eigenvalue"
    WRITE(*,*) "-----------------------", "       ----------"

    interval = l1 - ln

    i = 1
    iter = 1
    WRITE(*,*) "Search:", iter
    alpha = interval/DBLE(i+1) + ln

    CALL eiginvpower(A,n,v0,alpha,tol,maxiter,l,v)
    WRITE(*,*) alpha, l
    WRITE(*,*)

    DO i = 2, 2*search-1, 2

        iter = iter + 1
        WRITE(*,*) "Search:", iter

        DO j = 1, i

            alpha = DBLE(j)*interval/DBLE(i+1) + ln

            CALL eiginvpower(A,n,v0,alpha,tol,maxiter,l,v)
            WRITE(*,*) alpha, l

        END DO

        WRITE(*,*)

    END DO


END SUBROUTINE
