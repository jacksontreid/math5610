# MATH 5610 Software Manual

### Subroutine: [_norminfabserr_](../norminfabserr.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the infinity-norm of the absolute error between two vectors.

**Inputs:** 

​       _approx_ : REAL*8 -- an array of size (_length_) containing approximate values

​	_exact_ : REAL*8 -- an array of size (_length_) containing exact values

​       _length_ : INTEGER -- the length of the input vectors

**Outputs:** 

​	_error_ : REAL*8 -- the infinity-norm of the absolute error of the approximate values

**Example Usage:** 

```
      approx = (/ 0.5d0, 0.6d0, 1.9d0, 2.0d0 /)
      exact = (/ 0.2d0, 0.4d0, 0.1d0, 1.0d0 /)
      CALL norminfabserr(approx, exact, 4, error)
      WRITE(*,*) error
```
Output from the lines above:
```
1.7999999999999998
```
**Implementation:**

```
SUBROUTINE norminfabserr(approx, exact, length, error)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, DIMENSION(length), INTENT(in) :: approx, exact
    REAL*8, INTENT(out) :: error
    INTEGER :: i
    REAL*8 :: num

    error = 0.0d0

    DO i = 1, length
        num = DABS(approx(i) - exact(i))
        IF (num > error) THEN
            error = num
        END IF
    END DO

END SUBROUTINE
```



**Last Modified:** February/2019

