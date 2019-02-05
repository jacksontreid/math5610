# MATH 5610 Software Manual

### Subroutine: [_randsymmat_](../randsymmat.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will generate a square two-dimensional array (matrix) with the number of rows and columns specified. The array is populated with random numbers in the range [0,1), such that the result is a symmetric matrix. (Random number generator can be seeded by calling the [_randseed_](randseed.md) routine.)

**Inputs:** 

​	_n_ : INTEGER -- the number of desired rows and columns in the array

**Outputs:** 

​	_mat_ : REAL*8 -- the array, of size (n,n), containing symmetric, random values

**Example Usage:** 

```
      n = 3
      CALL randsymmat(n,mat)
      DO i = 1,3
         WRITE(*,*) mat(i,:)
      END DO
```
Output from the lines above:
```
  0.41216410446550689       0.83235106060734976       0.15677958344611054     
  0.83235106060734976       0.71062813195347563       0.14098649358772808     
  0.15677958344611054       0.14098649358772808       0.43465514787135873 
```
**Implementation:**

```
SUBROUTINE randsymmat(n,mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(out) :: mat(n,n)
    INTEGER :: i

    !Fill matrix
    DO i = 1, n

        !Fill upper triangle with random numbers
        CALL RANDOM_NUMBER(mat(i,i:n))

        !Copy values into lower triangle
        mat(i+1:n,i) = mat(i,i+1:n)

    END DO

END SUBROUTINE
```

**Last Modified:** February/2019