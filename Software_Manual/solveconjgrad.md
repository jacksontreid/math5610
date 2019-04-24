# MATH 5610 Software Manual

### Subroutine: [_solveconjgrad_](../solveconjgrad.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the solution of a square linear system, _A_ _x_ = _b_, using the conjugate gradient method.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing non-zero values of _A_

​	_n_ : INTEGER -- the number describing the size of _A_ and _b_

​        _b_ : REAL*8 -- an array of size (n) containing the values on the right-hand-side of the linear system

​        _x0_ : REAL*8 -- an array of size (n) containing the initial guess of the solution

​        _tol_ : REAL*8 -- the error tolerance for the iterations, SQRT(<r,r>/<b,b>)

​        _maxiter_ : INTEGER -- the maximum number of iterations

​        _text_ : LOGICAL -- flag to toggle writing of error/iteration count to the screen

**Outputs:** 

​        _x_ : REAL*8 -- an array of size (n) containing the solution to the linear system

**Example Usage:** 

```
    A = RESHAPE((/7.0d0, 3.0d0, 1.0d0, &
                & 3.0d0, 10.0d0, 2.0d0, &
                & 1.0d0, 2.0d0, 15.0d0/),(/3,3/),ORDER=(/2,1/)
    b = (/ 28.0d0, 31.0d0, 22.0d0 /)
    
    x0 = (/ 0.0d0, 0.0d0, 0.0d0 /)
	
    CALL solveconjgrad(A,3,b,x0,10.d-15,100,.TRUE.,x)
    WRITE(*,*) x
```
Output from the lines above:
```
           1  0.20683266161888783     
           2   5.7727182008145833E-002
           3   1.0516474991443452E-017
   3.0000000000000000        1.9999999999999996       0.99999999999999989
```
**Implementation:**

```
SUBROUTINE solveconjgrad(A,n,b,x0,tol,maxiter,text,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n, maxiter
    REAL*8, INTENT(in) :: A(n,n), b(n), x0(n), tol
    LOGICAL, INTENT(in) :: text
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: x1(n), p(n), r(n), r1(n), s(n), d1, d2, alpha, bb, error
    INTEGER :: iter

    error = 10.0d0*tol
    iter = 0
    x = x0

    CALL dotvec(b,b,n,bb)

    CALL multmat(A,x,n,n,1,s)
    r = b - s
    CALL dotvec(r,r,n,d1)
    p = r

    DO WHILE (error > tol .AND. iter < maxiter)

        CALL multmat(A,p,n,n,1,s)

        CALL dotvec(p,s,n,d2)

        alpha = d1/d2

        x1 = x + alpha*p

        r1 = r - alpha*s

        CALL dotvec(r1,r1,n,d2)

        p = r1 + d2*p/d1

        r = r1

        x = x1

        d1 = d2

        iter = iter + 1

        error = DSQRT(d1/bb)

        IF (text) THEN
            WRITE(*,*) iter, error
        END IF

    END DO

END SUBROUTINE
```



**Last Modified:** April/2019

