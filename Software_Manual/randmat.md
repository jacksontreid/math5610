# MATH 5610 Software Manual

### Subroutine: [_randmat_](../randmat.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be compiled using the GNU Fortran compiler (gfortran).

This routine can be linked to a program with the commands
```
    $ gfortran -c randmat.f90
    $ gfortran myprogram.f90 randmat.o
```

Or, a library can be created from this routine

```
    $ gfortran -c randmat.f90
    $ ar rcv mylib randmat.o
```

**Description:** This routine will generate a two-dimensional array (matrix) with the number of rows and columns specified. The array is populated with random numbers in the range [0,1).

**Inputs:** 

​	_r_ : int -- the number of desired rows in the array

​	_c_ : int -- the number of desired columns in the array

**Outputs:** 

​	_mat_ : double -- the array, of size (r,c), containing random values

**Example Usage:** 

```
      r = 4
      c = 3
      CALL randmat(r,c,mat)
      DO i = 1,4
      	WRITE(*,*) mat(i,:)
	  END DO
```
Output from the lines above:
```
  0.97271631449084051       0.22564766902354538       0.76916076930675126     
  0.33091735759473473       0.26385362448526350       0.15606750906641209     
  0.25437725951443602       0.24935314780127815       0.33951724448334697     
  0.39717145361424211       0.47148224067412425       4.1826478143255241E-002
```
**Last Modified:** January/2018