# MATH 5610 Software Manual

### Subroutine: [_matcond_](../matcond.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will approximate the condition number of a symmetric, positive-definite matrix using the power and inverse power methods.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing non-zero values of _A_

​	_n_ : INTEGER -- the number describing the size of _A_ 

​        _v0_ : REAL*8 -- an array of size (n) containing the initial guess of the eigenvector

​        _tol_ : REAL*8 -- the error tolerance for the iterations, norm2(l(k+1) - l(k))

​        _maxiter_ : INTEGER -- the maximum number of iterations

**Outputs:** 

​        _l1_ : REAL*8 --  the largest eigenvalue of the matrix

​        _ln_ : REAL*8 --  the smallest eigenvalue of the matrix

​        _cond_ : REAL*8 --  the condition number of the matrix

**Example Usage:** 

```
    A = RESHAPE((/2.0d0, 1.0d0, 0.0d0, &
                & 1.0d0, 2.0d0, 1.0d0, &
                & 0.0d0, 1.0d0, 2.0d0/),(/3,3/),ORDER=(/2,1/))
	v0 = (/ 1.0d0, 1.0d0, 1.0d0 /)
	
    CALL matcond(A,3,v0,10.d-15,100,l1,ln,cond)
    WRITE(*,*) l1, ln, cond
```
Output from the lines above:
```
   3.4142135623730940       0.58578643762690485        5.8284271247461890 
```
**Implementation:**

```
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
```



**Last Modified:** April/2019

