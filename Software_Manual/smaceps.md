# Math 5610 Fundamentals of Computational Mathematics Software Manual

### Subroutine: [_smaceps_](../smaceps.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be compiled using the GNU Fortran compiler (gfortran).

This routine can be linked to a program with the commands
```
    $ gfortran -c smaceps.f90
    $ gfortran myprogram.f90 smaceps.o
```

Or, a library can be created from this routine

```
    $ gfortran -c smaceps.f90
    $ ar rcv mylib smaceps.o
```

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