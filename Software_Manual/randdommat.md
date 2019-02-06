# MATH 5610 Software Manual

### Subroutine: [_randdommat_](../randdommat.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will generate a two-dimensional, diagonally-dominant array (matrix) with the number of rows and columns specified. The array is populated with random numbers in the range [0,1) on the off-diagonal. (Random number generator can be seeded by calling the [_randseed_](randseed.md) routine.)

**Inputs:** 

​	_n_ : INTEGER -- the number of desired rows and columns in the matrix

**Outputs:** 

​	_mat_ : REAL*8 -- the diagonally-dominant array, of size (n,n), containing random values

**Example Usage:** 

```
      n = 3
      CALL randdommat(n,mat)
      DO i = 1,3
        WRITE(*,*) mat(i,:)
      END DO
```

Output from the lines above:

```
   1.5029132412881849       0.16232490020852341        6.2258433209736452E-002
  0.50684127117481015        2.0127744587245489       0.92322888268523251     
  0.15553458611156223       0.81744370160165225        2.4275434277803090
```

**Implementation:**

```
SUBROUTINE randdommat(n,mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(out) :: mat(n,n)
    INTEGER :: i, j

    !Fill matrix with random numbers
    CALL RANDOM_NUMBER(mat)

    !Ensure diagonal dominance
    DO i = 1,n
        DO j = 1,n
            mat(i,i) = mat(i,i) + mat(i,j)
        END DO
    END DO

END SUBROUTINE
```

**Last Modified:** February/2019