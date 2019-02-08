# MATH 5610 Software Manual

### Subroutine: [_addmat_](../addmat.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine adds two matrices of the same size.

**Inputs:** 

​       _mat1_ : REAL*8 -- an array of size (_r_,_c_)

​      _mat2_ : REAL*8 -- an array of size (_r_,_c_)

​       _r_ : INTEGER -- number of rows in _mat1_ and _mat2_

​       _c_ : INTEGER -- number of columns in _mat1_ and _mat2_

**Outputs:** 

​	_new_mat_ : REAL*8 -- an array of size (_r_,_c_) containing the element-wise summation of _mat1_ and _mat2_

**Example Usage:** 

```
    mat1 = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                   & 0.2d0, 0.4d0, 0.1d0, &
                   & 0.4d0, 0.1d0, 1.0d0, &
                   & 2.0d0, 0.7d0, 0.2d0/),(/4,3/),ORDER=(/2,1/))
    mat3 = RESHAPE((/0.6d0, 0.4d0, -0.9d0, &
                   & 0.8d0, 0.6d0, 0.9d0, &
                   & 0.6d0, 0.9d0, 0.0d0, &
                   & -1.0d0, 0.3d0, 0.8d0/),(/4,3/),ORDER=(/2,1/))
    
    CALL addmat(mat1, mat2, 4, 3, mat3)
    DO i = 1,4
        WRITE(*,*) mat3(i,:)
    END DO
```
Output from the lines above:

```
   1.0000000000000000        1.0000000000000000       0.99999999999999989     
   1.0000000000000000        1.0000000000000000        1.0000000000000000     
   1.0000000000000000        1.0000000000000000        1.0000000000000000     
   1.0000000000000000        1.0000000000000000        1.0000000000000000  
```

**Implementation:**

```
SUBROUTINE addmat(mat1, mat2, r, c, new_mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, c
    REAL*8, DIMENSION(r,c), INTENT(in) :: mat1, mat2
    REAL*8, DIMENSION(r,c), INTENT(out) :: new_mat
    INTEGER :: i, j

    DO i = 1, r
        DO j = 1, c
            new_mat(i,j) = mat1(i,j) + mat2(i,j)
        END DO
    END DO

END SUBROUTINE
```

**Last Modified:** February/2019