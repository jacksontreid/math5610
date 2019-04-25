# MATH 5610 Software Manual

### Subroutine: [_eigpower_](../eigpower.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the largest eigenvalue of a matrix using the power method.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing non-zero values of _A_

​	_n_ : INTEGER -- the number describing the size of _A_ and _b_

​        _v0_ : REAL*8 -- an array of size (n) containing the initial guess of the eigenvector

​        _tol_ : REAL*8 -- the error tolerance for the iterations, norm2(l(k+1) - l(k))

​        _maxiter_ : INTEGER -- the maximum number of iterations

**Outputs:** 

​        _l_ : REAL*8 --  the largest eigenvalue of the matrix

​        _v_ : REAL*8 -- an array of size (n) containing the eigenvalue, normalized by the largest entry's magnitude

**Example Usage:** 

```
    A = RESHAPE((/1.0d0, 2.0d0, 0.0d0, &
               & -2.0d0, 1.0d0, 2.0d0, &
                & 1.0d0, 3.0d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
	v0 = (/ 1.0d0, 1.0d0, 1.0d0 /)
	
    CALL eigpower(A,3,v0,10.d-15,100,l,v)
    WRITE(*,*) l
    WRITE(*,*) v
```
Output from the lines above:
```
   3.0000000000000040     
  0.49999999999999933       0.50000000000000189        1.0000000000000000 
```
**Implementation:**

```
SUBROUTINE eigpower(A,n,v0,tol,maxiter,l,v)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n, maxiter
    REAL*8, INTENT(in) :: A(n,n), v0(n), tol
    REAL*8, INTENT(out) :: l, v(n)
    REAL*8 :: v1(n), vnorm, lold, error
    INTEGER :: i, iter

    error = 10.0d0*tol
    iter = 0
    lold = 0.0d0

    CALL multmat(A,v0,n,n,1,v)

    DO WHILE (error > tol .AND. iter < maxiter)

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
```



**Last Modified:** April/2019

