# MATH 5610 Software Manual

### Subroutine: [_randmat_](../randmat.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will generate a two-dimensional array (matrix) with the number of rows and columns specified. The array is populated with random numbers in the range [0,1). (Random number generator can be seeded by calling the [_randseed_](randseed.md) routine.)

**Inputs:** 

​	_r_ : INTEGER -- the number of desired rows in the array

​	_c_ : INTEGER -- the number of desired columns in the array

**Outputs:** 

​	_mat_ : REAL*8 -- the array, of size (r,c), containing random values

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
**Implementation:**

```
SUBROUTINE randmat(r,c,mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r,c
    REAL*8, INTENT(out) :: mat(r,c)

    !Fill matrix with random numbers
    CALL RANDOM_NUMBER(mat)

END SUBROUTINE
```

**Last Modified:** February/2019