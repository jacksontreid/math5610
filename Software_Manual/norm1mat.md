# MATH 5610 Software Manual

### Subroutine: [_norm1mat_](../norm1mat.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the induced 1-norm of a square matrix.

**Inputs:** 

​        _mat_ : REAL*8 -- the array of size (_n_,_n_)

​	_n_ : INTEGER -- the number of rows and columns in the square matrix

**Outputs:** 

​	_norm_ : REAL*8 -- the 1-norm of the matrix

**Example Usage:** 

```
      mat = RESHAPE((/0.5d0, 0.6d0, 1.9d0, &
                  & 0.2d0, 0.4d0, 0.1d0, &
                  & 0.4d0, 0.1d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    DO i = 1,3
        WRITE(*,*) mat(i,:)
    END DO

    CALL norm1mat(mat, 3, norm)
    WRITE(*,*) norm
```
Output from the lines above:
```
3.0000000000000000 
```
**Implementation:**

```
SUBROUTINE norm1mat(mat, n, norm)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, DIMENSION(n,n), INTENT(in) :: mat
    REAL*8, INTENT(out) :: norm
    INTEGER :: i, j
    REAL*8 :: num

    norm = 0.0d0

    DO i = 1, n
        !Calculate the 1-norm of each column
        num = 0.0d0
        DO j = 1, n
            num = num + DABS(mat(j,i))
        END DO

        !Determine maximum column 1-norm
        IF (num > norm) THEN
            norm = num
        END IF
    END DO

END SUBROUTINE
```



**Last Modified:** February/2019

