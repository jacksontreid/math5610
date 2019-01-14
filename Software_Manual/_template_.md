# Math 5610 Fundamentals of Computational Mathematics Software Manual

### Subroutine: [_smaceps_](./smaceps.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be compiled using the GNU Fortran compiler (gfortran).

A library can be created from this routine with the following commands

```
    $ gfortran -c maceps.f90
    $ ar rcv mylib maceps.o
```

**Description:** This routine will compute the single precision value for the machine epsilon or the number of digits in the representation of real numbers in single precision. This is a routine for analyzing the behavior of any computer. This usually will need to be run one time for each computer.

**Inputs:** None.

**Outputs:** This routine returns a single precision value for the number of decimal digits that can be represented on the computer being queried.

**Usage:** The routine has two arguments needed to return the values of the precision in terms of the smallest number that can be represented. Since the code is written in terms of a Fortran subroutine, the values of the machine machine epsilon and the power of two that gives the machine epsilon. Due to implicit Fortran typing, the first argument is a single precision value and the second is an integer.

```
      call smaceps(sval, ipow)
      print *, ipow, sval
```
Output from the lines above:
```
      24   5.96046448E-08
```
The first value (24) is the number of binary digits that define the machine epsilon and the second is related to the decimal version of the same value. The number of decimal digits that can be represented is roughly eight (E-08 on the end of the second value).

**Last Modified:** January/2018