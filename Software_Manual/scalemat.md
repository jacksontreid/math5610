# MATH 5610 Software Manual

### Subroutine: [_scalemat_](../scalemat.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine scales a matrix by a constant.

**Inputs:** 

​       _mat_ : REAL*8 -- an array of size (_r_,_c_)

​       _r_ : INTEGER -- number of rows in _mat_

​       _c_ : INTEGER -- number of columns in _mat_

​       _const_ : REAL*8 -- constant by which the matrix is scaled

**Outputs:** 

​	_mat_ : REAL*8 -- an array of size (_r_,_c_) containing the scaled values

**Example Usage:** 

```
    mat1 = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                   & 0.2d0, 0.4d0, 0.1d0, &
                   & 0.4d0, 0.1d0, 1.0d0, &
                   & 2.0d0, 0.7d0, 0.2d0/),(/4,3/),ORDER=(/2,1/))
    CALL scalemat(mat1, 4, 3, 2.0d0)
    DO i = 1,4
        WRITE(*,*) mat1(i,:)
    END DO
```
Output from the lines above:

```
  0.80000000000000004        1.2000000000000000        3.7999999999999998     
  0.40000000000000002       0.80000000000000004       0.20000000000000001     
  0.80000000000000004       0.20000000000000001        2.0000000000000000     
   4.0000000000000000        1.3999999999999999       0.40000000000000002 
```

**Implementation:**

```
SUBROUTINE scalemat(mat,r,c,const)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, c
    REAL*8, INTENT(in) :: const
    REAL*8, DIMENSION(r,c), INTENT(inout) :: mat
    INTEGER :: i, j

    DO i = 1, r
        DO j = 1,c
            mat(i,j) = const*mat(i,j)
        END DO
    END DO

END SUBROUTINE
```

**Last Modified:** February/2019