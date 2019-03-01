# MATH 5610 Software Manual

### Subroutine: [_rowechelon_](../rowechelon.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will perform elementary row operations to reduce a matrix to row echelon form.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_r_,_c_) 

​	_r_ : INTEGER -- the number of rows in _A_ 

​	_c_ : INTEGER -- the number of columns in _A_ 

**Outputs:** 

​        _A_ : REAL*8 -- an array of size (_r_,_c_) in row echelon form

**Example Usage:** 

```
    A = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                & 0.2d0, 0.4d0, 0.1d0, &
                & 0.4d0, 0.1d0, 1.0d0, &
                & 2.0d0, 0.7d0, 0.2d0/),(/4,3/),ORDER=(/2,1/))
    
    CALL rowechelon(A,4,3)
    DO i = 1,4
        WRITE(*,*) A
    END DO
```
Output from the lines above:
```
   0.40000000000000002      0.59999999999999998       1.8999999999999999     
   0.0000000000000000       0.10000000000000003      -0.84999999999999998     
   0.0000000000000000       0.0000000000000000       -5.1499999999999986     
   0.0000000000000000       0.0000000000000000        0.00000000000000000 
```
**Implementation:**

```
SUBROUTINE rowechelon(A,r,c)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, c
    REAL*8, INTENT(inout) :: A(r,c)
    REAL*8 :: factor
    INTEGER :: i, j, k

    DO k = 1,r-1
        DO i = k+1,r
            factor = A(i,k)/A(k,k)
            DO j = k,c
                A(i,j) = A(i,j) - A(k,j)*factor
            END DO
        END DO
    END DO

END SUBROUTINE
```



**Last Modified:** March/2019

