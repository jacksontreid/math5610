# MATH 5610 Software Manual

### Subroutine: [_eigsearch_](../eigsearch.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will perform a blind search for the real eigenvalues 1 < l < n (for a spd matrix), by subdividing the interval between the largest and smallest eigenvalues.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing non-zero values of _A_

​	_n_ : INTEGER -- the number describing the size of _A_ and _b_

​        _v0_ : REAL*8 -- an array of size (n) containing the initial guess of the eigenvector

​        _tol_ : REAL*8 -- the error tolerance for the eigenvalue evaluation, norm2(l(k+1) - l(k))

​        _maxiter_ : INTEGER -- the maximum number of iterations per eigenvalue evaluation

​        _search_ : INTEGER -- the number of searches to perform, each with increasing search locations

**Example Usage:** 

```
    A = RESHAPE((/2.0d0, 1.0d0, 0.0d0, &
                & 1.0d0, 2.0d0, 1.0d0, &
                & 0.0d0, 1.0d0, 2.0d0/),(/3,3/),ORDER=(/2,1/))
	v0 = (/ 1.0d0, 1.0d0, 1.0d0 /)
	
    CALL matcond(A,3,v0,10.d-15,100,4)
```
Output from the lines above:
```
 l1:   3.4142135623730940     
 ln:  0.58578643762690485     

 Search location (alpha)       Eigenvalue
 -----------------------       ----------
 Search:           1
   1.9999999999999993        2.0000000000000004     

 Search:           2
   1.5285954792089678       0.58578643762690663     
   2.4714045207910309        3.4142135623730940     

 Search:           3
   1.1514718625761426       0.58578643762690508     
   1.7171572875253804        2.0000000000000000     
   2.2828427124746185        2.0000000000000004     
   2.8485281374238562        3.4142135623730940     

 Search:           4
  0.98984745544778896       0.58578643762690508     
   1.3939084732686733       0.58578643762690685     
   1.7979694910895574        2.0000000000000000     
   2.2020305089104415        1.9999999999999998     
   2.6060915267313254        3.4142135623730949     
   3.0101525445522102        3.4142135623730958  
```
**Implementation:**

```
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
```



**Last Modified:** April/2019

