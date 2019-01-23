# MATH 5610 Software Manual

The code described in this Software Manual can be compiled using the GNU Fortran compiler (gfortran).

The routines can be linked to a program with the commands
```
    $ gfortran -c mycode.f90
    $ gfortran myprogram.f90 mycode.o -o myprogram.out
```

Or, a library can be created from the routines

```
    $ gfortran -c *.f90
    $ ar rcv mylib *.o
```

**Last Modified:** January/2018

