# MATH 5610 Software Manual

### Subroutine: [_solvediagsys_](../solvediagsys.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the solution of a square linear system, _A_ _x_ = _b_, with a diagonal coefficient matrix.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing non-zero values only along the diagonal

​	_n_ : INTEGER -- the size of _A_ and _b_

​        _b_ : REAL*8 -- an array of size (n) containing the values on the right-hand-side of the linear system

**Outputs:** 

​        _x_ : REAL*8 -- an array of size (n) containing the solution to the linear system

**Example Usage:** 

```
    A = RESHAPE((/0.4d0, 0.0d0, 0.0d0, &
                   & 0.0d0, 0.4d0, 0.0d0, &
                   & 0.0d0, 0.0d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    b = (/ 0.2d0, 0.4d0, 0.1d0 /)

    CALL solvediagsys(A,3,b,x)
    WRITE(*,*) x
```
Output from the lines above:
```
  0.50000000000000000        1.0000000000000000       0.10000000000000001
```
**Implementation:**

```
SUBROUTINE solvediagsys(A,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(in) :: A(n,n), b(n)
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: Ad
    INTEGER :: i

    DO i = 1, n
        x(i) = b(i)/A(i,i)
    END DO

END SUBROUTINE
```



**Last Modified:** March/2019

