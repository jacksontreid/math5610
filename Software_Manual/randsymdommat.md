# MATH 5610 Software Manual

### Subroutine: [_randsymdommat_](../randsymdommat.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will generate a square two-dimensional array (matrix) with the number of rows and columns specified. The array is populated with random numbers in the range [0,1), such that the result is a symmetric and diagonally-dominant matrix. (Random number generator can be seeded by calling the [_randseed_](randseed.md) routine.)

**Inputs:** 

​	_n_ : INTEGER -- the number of desired rows and columns in the array

**Outputs:** 

​	_mat_ : REAL*8 -- the array, of size (n,n), containing symmetric, diagonally-dominant, random values

**Example Usage:** 

```
      n = 3
      CALL randsymdommat(n,mat)
      DO i = 1,3
         WRITE(*,*) mat(i,:)
      END DO
```
Output from the lines above:
```
   1.6268502837653127       0.69145195206885002       0.21299332256940617     
  0.69145195206885002        3.9097177205561993       0.59840620108118370     
  0.21299332256940617       0.59840620108118370        3.4186296440401978
```
**Implementation:**

```
SUBROUTINE randsymdommat(n,mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(out) :: mat(n,n)
    INTEGER :: i, j

    !Fill matrix
    DO i = 1, n

        !Fill upper triangle with random numbers
        CALL RANDOM_NUMBER(mat(i,i:n))

        !Copy values into lower triangle
        mat(i+1:n,i) = mat(i,i+1:n)

    END DO

    !Ensure diagonal dominance
    DO i = 1,n
        DO j = 1,n
            mat(i,i) = mat(i,i) + mat(i,j)
        END DO
    END DO

END SUBROUTINE
```

**Last Modified:** March/2019