# MATH 5610 Software Manual

### Subroutine: [_solvesteepest_](../solvesteepest.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the solution of a square linear system, _A_ _x_ = _b_, using the steepest descent method.

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
	
    CALL solvesteepest(A,3,b,x0,10.d-15,100,.TRUE.,x)
    WRITE(*,*) x
```
Output from the lines above:
```
           1  0.20683266161888783     
           2   7.7469506912762179E-002
           3   4.0462240414762082E-002
           4   2.0691648507544073E-002
           5   1.0819187535817176E-002
           6   5.5327405460426473E-003
           7   2.8929428963524640E-003
           8   1.4793996690647862E-003
           9   7.7354409227599569E-004
          10   3.9557672415968152E-004
          11   2.0683797922508023E-004
          12   1.0577327274638729E-004
          13   5.5306413787011866E-005
          14   2.8282718734900614E-005
          15   1.4788383726431614E-005
          16   7.5625170543361072E-006
          17   3.9542663909180446E-006
          18   2.0221416736203114E-006
          19   1.0573314149535518E-006
          20   5.4070052587153090E-007
          21   2.8271988038421713E-007
          22   1.4457793066216427E-007
          23   7.5596477730662486E-008
          24   3.8658697438589182E-008
          25   2.0213744564110896E-008
          26   1.0336950327090881E-008
          27   5.4049537963776508E-009
          28   2.7639974739056752E-009
          29   1.4452307660424891E-009
          30   7.3906537170204081E-010
          31   3.8644030010313516E-010
          32   1.9761871304362699E-010
          33   1.0333028402981756E-010
          34   5.2841273912051095E-011
          35   2.7629487904944696E-011
          36   1.4129229897535033E-011
          37   7.3878496421165124E-012
          38   3.7780152278249415E-012
          39   1.9754373487592990E-012
          40   1.0102036108965338E-012
          41   5.2821225497426345E-013
          42   2.7011837537137677E-013
          43   1.4123869151316361E-013
          44   7.2226960907928355E-014
          45   3.7765818177244962E-014
          46   1.9312769354632374E-014
          47   1.0098203313245771E-014
          48   5.1640420067056291E-015
   2.9999999999999689        2.0000000000000129       0.99999999999998901 
```
**Implementation:**

```
SUBROUTINE solvesteepest(A,n,b,x0,tol,maxiter,text,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n, maxiter
    REAL*8, INTENT(in) :: A(n,n), b(n), x0(n), tol
    LOGICAL, INTENT(in) :: text
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: x1(n), r(n), r1(n), s(n), d1, d2, alpha, bb, error
    INTEGER :: iter

    error = 10.0d0*tol
    iter = 0
    x = x0

    CALL dotvec(b,b,n,bb)

    CALL multmat(A,x,n,n,1,s)
    r = b - s
    CALL dotvec(r,r,n,d1)

    DO WHILE (error > tol .AND. iter < maxiter)

        CALL multmat(A,r,n,n,1,s)

        CALL dotvec(r,s,n,d2)

        alpha = d1/d2

        x1 = x + alpha*r

        r1 = r - alpha*s

        CALL dotvec(r1,r1,n,d1)

        r = r1

        x = x1

        iter = iter + 1

        error = DSQRT(d1/bb)

        IF (text) THEN
            WRITE(*,*) iter, error
        END IF

    END DO

END SUBROUTINE
```



**Last Modified:** April/2019

