# MATH 5610 Software Manual

### Subroutine: [_randseed_](../randseed.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be compiled using the GNU Fortran compiler (gfortran).

This routine can be linked to a program with the commands
```
    $ gfortran -c randseed.f90
    $ gfortran myprogram.f90 randseed.o
```

Or, a library can be created from this routine

```
    $ gfortran -c randseed.f90
    $ ar rcv mylib randseed.o
```

**Description:** This routine seeds FORTRAN's intrinsic random number generator based on the milliseconds on the system clock.

**Inputs:** None.

**Outputs:** None.

**Example Usage:** 

```
      CALL randseed()
```
**Last Modified:** January/2018