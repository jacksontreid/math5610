# MATH 5610 Software Manual

### Subroutine: [_forsub_](../forsub.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the solution of a square linear system, _A_ _x_ = _b_, with an lower triangular coefficient matrix, using forward-substitution.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing non-zero values only in the lower triangle

​	_n_ : INTEGER -- the number describing the size of _A_ and _b_

​        _b_ : REAL*8 -- an array of size (n) containing the values on the right-hand-side of the linear system

**Outputs:** 

​        _x_ : REAL*8 -- an array of size (n) containing the solution to the linear system

**Example Usage:** 

```
    A = RESHAPE((/0.4d0, 0.0d0, 0.0d0, &
                & 0.6d0, 0.4d0, 0.0d0, &
                & 1.9d0, 0.1d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    b = (/ 0.2d0, 0.4d0, 0.1d0 /)

    CALL forsub(A,3,b,x)
    WRITE(*,*) x
```
Output from the lines above:
```
  0.50000000000000000       0.25000000000000006      -0.87500000000000000 
```
**Implementation:**

```
SUBROUTINE forsub(A,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(in) :: A(n,n), b(n)
    REAL*8, INTENT(out) :: x(n)
    INTEGER :: i, j

    x(1) = b(1)/A(1,1)

    DO i = 2,n
        x(i) = b(i)
        DO j = 1,i-1
            x(i) = x(i) - x(j)*A(i,j)
        END DO
        x(i) = x(i)/A(i,i)
    END DO

END SUBROUTINE
```



**Last Modified:** March/2019

