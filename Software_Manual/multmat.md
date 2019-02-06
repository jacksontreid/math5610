# MATH 5610 Software Manual

### Subroutine: [_multmat_](../multmat.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will multiply two matrices, of size (r,n) and (n,c), to generate a matrix (r,c).

**Inputs:** 

​        _mat1_ : REAL*8 -- an array of size (_r_,_n_)

​	_mat2_ : REAL*8 -- an array of size (_n_,_c_)

​	_r_ : INTEGER -- the number of rows in _mat1_ and the matrix product, _new_mat_

​	_n_ : INTEGER -- the number of columns in _mat1_ and rows, _mat2_

​	_c_ : INTEGER -- the number of columns in _mat2_ and the matrix product, _new_mat_

**Outputs:** 

​	_new_mat_ : REAL*8 -- an array of size (_r_,_c_) that is the matrix product of _mat1_ and _mat2_ 

**Example Usage:** 

```
    mat1 = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                   & 0.2d0, 0.4d0, 0.1d0, &
                   & 0.4d0, 0.1d0, 1.0d0, &
                   & 2.0d0, 0.7d0, 0.2d0/),(/4,3/),ORDER=(/2,1/))
    mat2 = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                   & 0.2d0, 0.4d0, 0.1d0, &
                   & 0.4d0, 0.1d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))

    CALL multmat(mat1,mat2,4,3,3,new_mat)
    DO i = 1,4
        WRITE(*,*) new_mat(i,:)
    END DO
```
Output from the lines above:
```
   1.0400000000000000       0.66999999999999993        2.7199999999999998     
  0.20000000000000004       0.29000000000000004       0.52000000000000002     
  0.58000000000000007       0.38000000000000000        1.7700000000000000     
   1.0200000000000000        1.5000000000000000        4.0699999999999994
```
**Implementation:**

```
SUBROUTINE multmat(mat1,mat2,r,n,c,new_mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, n, c
    REAL*8, INTENT(in) :: mat1(r,n), mat2(n,c)
    REAL*8, INTENT(out) :: new_mat(r,c)
    INTEGER :: i, j, k
    REAL*8 :: num

    DO i = 1,r
        DO j = 1,c

            num = 0.0d0

            DO k = 1,n
                 num = num + mat1(i,k)*mat2(k,j)
            END DO

            new_mat(i,j) = num

        END DO
    END DO

END SUBROUTINE
```



**Last Modified:** February/2019

