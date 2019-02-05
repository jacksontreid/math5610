# MATH 5610 Software Manual

### Subroutine: [_norm2abserr_](../norm2abserr.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the 2-norm of the absolute error between two vectors.

**Inputs:** 

​       _approx_ : REAL*8 -- an array of size (_length_) containing approximate values

​	_exact_ : REAL*8 -- an array of size (_length_) containing exact values

​       _length_ : INTEGER -- the length of the input vectors

**Outputs:** 

​	_error_ : REAL*8 -- the 2-norm of the absolute error of the approximate values

**Example Usage:** 

```
      approx = (/ 0.5d0, 0.6d0, 1.9d0, 2.0d0 /)
      exact = (/ 0.2d0, 0.4d0, 0.1d0, 1.0d0 /)
      CALL norm2abserr(approx, exact, 4, error)
      WRITE(*,*) error
```
Output from the lines above:
```
2.0904544960366871
```
**Implementation:**

```
SUBROUTINE norm2abserr(approx, exact, length, error)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, DIMENSION(length), INTENT(in) :: approx, exact
    REAL*8, INTENT(out) :: error
    INTEGER :: i

    error = 0.0d0

    DO i = 1, length
        error = error + DABS(approx(i) - exact(i))**2
    END DO

    error = DSQRT(error)

END SUBROUTINE
```



**Last Modified:** February/2019

