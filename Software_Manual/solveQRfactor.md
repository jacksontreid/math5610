# MATH 5610 Software Manual

### Subroutine: [_solveQRfactor_](../solveQRfactor.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the solution of a square linear system, _A_ _x_ = _b_, using QR-decomposition, matrix-vector multiplication, and back-substitution.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing non-zero values of _A_

​	_n_ : INTEGER -- the number describing the size of _A_ and _b_

​        _b_ : REAL*8 -- an array of size (n) containing the values on the right-hand-side of the linear system

**Outputs:** 

​        _x_ : REAL*8 -- an array of size (n) containing the solution to the linear system

**Example Usage:** 

```
    A = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                & 0.6d0, 0.4d0, 0.1d0, &
                & 1.9d0, 0.1d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    b = (/ 0.2d0, 0.4d0, 0.1d0 /)

    CALL solveQRfactor(A,3,b,x)
    WRITE(*,*) x
```
Output from the lines above:
```
  0.10985915492957754       0.88450704225352117      -0.19718309859154939 
```
**Implementation:**

```
SUBROUTINE solveQRfactor(A,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(in) :: b(n), A(n,n)
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: Q(n,n), R(n,n), y(n)
    INTEGER :: i

    !Perform QR-decomposition on the matrix
    CALL QRdecompmod(A,n,Q,R)

    !Use matrix-vector multiplication to modify the right-hand side
    CALL multmat(TRANSPOSE(Q),b,n,n,1,y)

    !Use back-substitution to determine the system solution
    CALL backsub(R,n,y,x)

END SUBROUTINE
```



**Last Modified:** April/2019

