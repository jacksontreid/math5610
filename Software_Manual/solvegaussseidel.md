# MATH 5610 Software Manual

### Subroutine: [_solvegaussseidel_](../solvegaussseidel.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the solution of a square linear system, _A_ _x_ = _b_, using Gauss-Seidel iteration.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing non-zero values of _A_

​	_n_ : INTEGER -- the number describing the size of _A_ and _b_

​        _b_ : REAL*8 -- an array of size (n) containing the values on the right-hand-side of the linear system

​        _x0_ : REAL*8 -- an array of size (n) containing the initial guess of the solution

​        _tol_ : REAL*8 -- the error tolerance for the iterations, norm2(x(k+1) - x(k))

​        _maxiter_ : INTEGER -- the maximum number of iterations

​        _text_ : LOGICAL -- flag to toggle writing of error/iteration count to the screen

**Outputs:** 

​        _x_ : REAL*8 -- an array of size (n) containing the solution to the linear system

**Example Usage:** 

```
    A = RESHAPE((/3.0d0, -1.0d0, 1.0d0, &
                & 1.0d0, -4.0d0, 1.0d0, &
                & 1.0d0, 2.0d0, -6.0d0/),(/3,3/),ORDER=(/2,1/))
    b = (/ 2.0d0, -0.0d0, 1.0d0 /)
    
	x0 = (/ 0.1d0, 0.1d0, 0.1d0 /)
	
    CALL solvegaussseidel(A,3,b,x0,10.d-16,100,.TRUE.,x)
    WRITE(*,*) x
```
Output from the lines above:
```
           1  0.58130600088650952     
           2   6.2058626702080261E-002
           3   5.1715522251732995E-003
           4   4.3096268543111655E-004
           5   3.5913557119215168E-005
           6   2.9927964265470456E-006
           7   2.4939970217447685E-007
           8   2.0783308404144884E-008
           9   1.7319423845393031E-009
          10   1.4432859555223617E-010
          11   1.2027384226079242E-011
          12   1.0022913251752320E-012
          13   8.3432677891542419E-014
          14   6.8786604816109442E-015
          15   5.6284492623895397E-016
  0.72307692307692306       0.18461538461538463        1.5384615384615385E-002
```
**Implementation:**

```
SUBROUTINE solvegaussseidel(A,n,b,x0,tol,maxiter,text,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n, maxiter
    REAL*8, INTENT(in) :: A(n,n), b(n), x0(n), tol
    LOGICAL, INTENT(in) :: text
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: A_U(n,n), L(n,n), xold(n), y(n), error
    INTEGER :: i, iter

    !Split the coefficient matrix (A = [L + D] + [U])
    L(:,:) = 0.0d0
    A_U(:,:) = 0.0d0
    DO i = 1,n
        L(i,:i) = A(i,:i)
        A_U(i,i+1:) = A(i,i+1:)
    END DO

    error = 10.0d0*tol
    iter = 0
    xold = x0

    DO WHILE (error > tol .AND. iter < maxiter)

        CALL multmat(A_U,xold,n,n,1,y)

        y = b - y

        CALL forsub(L,n,y,x)

        iter = iter + 1

        CALL norm2abserr(x,xold,n,error)

        xold = x

        IF (text) THEN
            WRITE(*,*) iter, error
        END IF

    END DO

END SUBROUTINE
```



**Last Modified:** April/2019

