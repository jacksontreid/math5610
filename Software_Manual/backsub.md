# MATH 5610 Software Manual

### Subroutine: [_backsub_](../backsub.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the solution of a square linear system, _A_ _x_ = _b_, with an upper triangular coefficient matrix, using back-substitution.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing non-zero values only in the upper triangle

​	_n_ : INTEGER -- the number describing the size of _A_ and _b_

​        _b_ : REAL*8 -- an array of size (n) containing the values on the right-hand-side of the linear system

**Outputs:** 

​        _x_ : REAL*8 -- an array of size (n) containing the solution to the linear system

**Example Usage:** 

```
    A = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                & 0.0d0, 0.4d0, 0.1d0, &
                & 0.0d0, 0.0d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    b = (/ 0.2d0, 0.4d0, 0.1d0 /)

    CALL backsub(A,3,b,x)
    WRITE(*,*) x
```
Output from the lines above:
```
  -1.4374999999999998       0.97499999999999998       0.10000000000000001 
```
**Implementation:**

```
SUBROUTINE backsub(A,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(in) :: A(n,n), b(n)
    REAL*8, INTENT(out) :: x(n)
    INTEGER :: i, j

    x(n) = b(n)/A(n,n)

    DO i = n-1,1,-1
        x(i) = b(i)
        DO j = i+1,n
            x(i) = x(i) - x(j)*A(i,j)
        END DO
        x(i) = x(i)/A(i,i)
    END DO

END SUBROUTINE
```



**Last Modified:** March/2019

