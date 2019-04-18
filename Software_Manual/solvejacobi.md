# MATH 5610 Software Manual

### Subroutine: [_solvejacobi_](../solvejacobi.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the solution of a square linear system, _A_ _x_ = _b_, using Jacobi iteration.

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
	
    CALL solvejacobi(A,3,b,x0,10.d-16,100,.TRUE.,x)
    WRITE(*,*) x
```
Output from the lines above:
```
           1  0.60873274559165580     
           2  0.12958415207553656     
           3   5.0972201197560150E-002
           4   1.5720681360411996E-002
           5   4.0661101290844614E-003
           6   1.0563070123870849E-003
           7   2.3439100404806272E-004
           8   6.2002332645135199E-005
           9   1.1950974007381066E-005
          10   3.9275528971733843E-006
          11   7.4262459178560002E-007
          12   3.4325093007584623E-007
          13   9.3491034623016626E-008
          14   4.1814883550270918E-008
          15   1.4339488090380537E-008
          16   5.8334701036459848E-009
          17   2.1606480022549640E-009
          18   8.4560772761037712E-010
          19   3.2088248360435129E-010
          20   1.2393921471965539E-010
          21   4.7394909322266116E-011
          22   1.8227250325944243E-011
          23   6.9873942318568981E-012
          24   2.6834827209085342E-012
          25   1.0294880634080934E-012
          26   3.9522509868314944E-013
          27   1.5166741917866818E-013
          28   5.8239311564721519E-014
          29   2.2367773929940780E-014
          30   8.6289590504347317E-015
          31   3.3939318407308729E-015
          32   1.2449514688564291E-015
          33   4.4754520913118096E-016
  0.72307692307692317       0.18461538461538454        1.5384615384615403E-002
```
**Implementation:**

```
SUBROUTINE solvejacobi(A,n,b,x0,tol,maxiter,text,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n, maxiter
    REAL*8, INTENT(in) :: A(n,n), b(n), x0(n), tol
    LOGICAL, INTENT(in) :: text
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: A_LU(n,n), D(n), xold(n), y(n), error
    INTEGER :: i, iter

    !Split the coefficient matrix (A = [L + U] + [D])
    A_LU = A
    DO i = 1,n
        D(i) = 1.0d0/A(i,i)
        A_LU(i,i) = 0.0d0
    END DO

    error = 10.0d0*tol
    iter = 0
    xold = x0

    DO WHILE (error > tol .AND. iter < maxiter)

        CALL multmat(A_LU,xold,n,n,1,y)

        x = D*(b - y)

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

