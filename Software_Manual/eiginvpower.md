# MATH 5610 Software Manual

### Subroutine: [_eiginvpower_](../eiginvpower.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the smallest real eigenvalue (based on a shift) of a matrix using the inverse power method.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing non-zero values of _A_

​	_n_ : INTEGER -- the number describing the size of _A_ and _b_

​        _v0_ : REAL*8 -- an array of size (n) containing the initial guess of the eigenvector

​        _alpha_ : REAL*8 -- the value specifying the shift amount

​        _tol_ : REAL*8 -- the error tolerance for the iterations, norm2(l(k+1) - l(k))

​        _maxiter_ : INTEGER -- the maximum number of iterations

**Outputs:** 

​        _l_ : REAL*8 --  the largest eigenvalue of the matrix

​        _v_ : REAL*8 -- an array of size (n) containing the eigenvalue, normalized by the largest entry's magnitude

**Example Usage:** 

```
    A = RESHAPE((/1.0d0, 2.0d0, 0.0d0, &
                & 2.0d0, 1.0d0, 2.0d0, &
                & 0.0d0, 2.0d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
	v0 = (/ 1.0d0, 1.0d0, 1.0d0 /)
	
    CALL eigpower(A,3,v0,1.1d0,10.d-15,100,l,v)
    WRITE(*,*) l
    WRITE(*,*) v
```
Output from the lines above:
```
   1.0000000000000000     
   1.0000000000000000        1.7414728274865761E-010 -0.99999999972887066 
```
**Implementation:**

```
SUBROUTINE eiginvpower(A,n,v0,alpha,tol,maxiter,l,v)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n, maxiter
    REAL*8, INTENT(in) :: A(n,n), v0(n), alpha, tol
    REAL*8, INTENT(out) :: l, v(n)
    REAL*8 :: A_LU(n,n), v1(n), vnorm, lold, error
    INTEGER :: i, iter

    error = 10.0d0*tol
    iter = 0
    lold = 0.0d0

    A_LU = A
    DO i = 1,n
        A_LU(i,i) = A_LU(i,i) - alpha
    END DO
    CALL LUdecomp(A_LU,n)
    v1 = v0

    DO WHILE (error > tol .AND. iter < maxiter)

        CALL solveLUfactor(A_LU,n,v1,v,.FALSE.)

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

