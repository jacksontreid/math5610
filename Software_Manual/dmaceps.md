# Math 5610 Fundamentals of Computational Mathematics Software Manual

### Subroutine: [_dmaceps_](../dmaceps.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be compiled using the GNU Fortran compiler (gfortran).

This routine can be linked to a program with the commands
```
    $ gfortran -c dmaceps.f90
    $ gfortran myprogram.f90 dmaceps.o
```

Or, a library can be created from this routine

```
    $ gfortran -c dmaceps.f90
    $ ar rcv mylib dmaceps.o
```

**Description:** This routine will compute the double-precision machine rounding unit of a computer, and the number of digits used in the representation of real numbers in double precision.

**Inputs:** None.

**Outputs:** 

​	_seps_ : double -- the double-precision machine rounding unit

​	_i_ : int -- the number of digits in the representation of real numbers in double precision

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