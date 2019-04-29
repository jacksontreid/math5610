# MATH 5610 Software Manual

### Subroutine: [_matcond2_](../matcond2.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will approximate the condition number of a symmetric, positive-definite matrix using Rayleigh Quotient Iteration. A specified number of random initial vectors are generated, and the resulting eigenvalues are used to predict the condition number.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing non-zero values of _A_

​	_n_ : INTEGER -- the number describing the size of _A_ 

​        _guess_ : REAL*8 -- the number of initial vectors to be generated

​        _tol_ : REAL*8 -- the error tolerance for the iterations, norm2(l(k+1) - l(k))

​        _maxiter_ : INTEGER -- the maximum number of iterations

**Outputs:** 

​        _l1_ : REAL*8 --  the largest found eigenvalue of the matrix

​        _ln_ : REAL*8 --  the smallest found eigenvalue of the matrix

​        _cond_ : REAL*8 --  the condition number of the matrix

**Example Usage:** 

```
    A = RESHAPE((/2.0d0, 1.0d0, 0.0d0, &
                & 1.0d0, 2.0d0, 1.0d0, &
                & 0.0d0, 1.0d0, 2.0d0/),(/3,3/),ORDER=(/2,1/))
	v0 = (/ 1.0d0, 1.0d0, 1.0d0 /)
	
    CALL matcond(A,3,10,10.d-15,100,l1,ln,cond)
    WRITE(*,*) l1, ln, cond
```
Output from the lines above:
```
   3.4142135623730958       0.58578643762690485        5.8284271247461925 
```
**Implementation:**

```
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
```



**Last Modified:** April/2019

