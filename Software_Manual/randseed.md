# MATH 5610 Software Manual

### Subroutine: [_randseed_](../randseed.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine seeds Fortran's intrinsic random number generator based on the milliseconds on the system clock.

**Inputs:** None.

**Outputs:** None.

**Example Usage:** 

```
      CALL randseed()
```
**Last Modified:** January/2018