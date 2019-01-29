# MATH 5610 Software Manual

### Subroutine: [_smaceps_](../smaceps.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the single-precision machine rounding unit of a computer, and the number of digits used in the representation of real numbers in single precision.

**Inputs:** None.

**Outputs:** 

​	_seps_ : REAL*8 -- the single-precision machine rounding unit

​	_i_ : INTEGER -- the number of digits in the representation of real numbers in single precision

**Example Usage:** 

```
      CALL smaceps(seps,i)
      WRITE(*,*) i, seps
```
Output from the lines above:
```
24   5.96046448E-08
```
**Implementation:**

```
SUBROUTINE smaceps(seps, i)
    IMPLICIT NONE

    REAL, INTENT(out) :: seps
    REAL :: one, appone
    INTEGER, INTENT(out) :: i

    !Initialize variables to compute the machine value near 1.0
    one = 1.0
    seps = 1.0
    appone = one + seps

    !Iteratively divide the perturbation by 2 to determine when the difference
    !between one and the approximation is zero
    DO i=1,1000

        !Update the perturbation and compute the approximation of one
        seps = seps / 2.
        appone = one + seps

        !Compare values and break the loop if difference is zero
        IF(ABS(appone-one) == 0.0) RETURN

    END DO

    !Alert user that loop count has been reached
    WRITE(*,*) "The loop limit has been reached!"

END SUBROUTINE
```

**Last Modified:** January/2018