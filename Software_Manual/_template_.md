# MATH 5610 Software Manual

### Subroutine: [_smaceps_](../smaceps.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the single-precision machine rounding unit of a computer, and the number of digits used in the representation of real numbers in single precision.

**Inputs:** None.

**Outputs:** 

​	_seps_ : real -- the single-precision machine rounding unit

​	_i_ : int -- the number of digits in the representation of real numbers in single precision

**Example Usage:** 

```
      CALL smaceps(seps,i)
      WRITE(*,*) i, seps
```
Output from the lines above:
```
      24   5.96046448E-08
```
**Last Modified:** January/2018

