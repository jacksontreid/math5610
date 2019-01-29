# MATH 5610 Software Manual

### Subroutine: [_dmaceps_](../dmaceps.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the double-precision machine rounding unit of a computer, and the number of digits used in the representation of real numbers in double precision.

**Inputs:** None.

**Outputs:** 

​	_seps_ : REAL*8 -- the double-precision machine rounding unit

​	_i_ : INTEGER -- the number of digits in the representation of real numbers in double precision

**Example Usage:** 

```
      CALL dmaceps(seps,i)
      WRITE(*,*) i, seps
```
Output from the lines above:
```
53   1.1102230246251565E-016
```
**Implementation:**

```
SUBROUTINE dmaceps(deps, i)
    IMPLICIT NONE

    REAL*8, INTENT(out) :: deps
    REAL*8 :: one, appone
    INTEGER, INTENT(out) :: i

    !Initialize variables to compute the machine value near 1.0
    one = 1.0d0
    deps = 1.0d0
    appone = one + deps

    !Iteratively divide the perturbation by 2 to determine when the difference
    !between one and the approximation is zero
    DO i=1,1000

        !Update the perturbation and compute the approximation of one
        deps = deps / 2.0d0
        appone = one + deps

        !Compare values and break the loop if difference is zero
        IF(ABS(appone-one) == 0.0d0) RETURN

    END DO

    !Alert user that loop count has been reached
    WRITE(*,*) "The loop limit has been reached!"

END SUBROUTINE
```

**Last Modified:** January/2018