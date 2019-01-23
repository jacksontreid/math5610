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
**Last Modified:** January/2018