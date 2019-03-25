# MATH 5610 Software Manual

### Subroutine: [_LUdecomp_](../LUdecomp.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will calculate the LU decomposition of a matrix. The decomposition produces a unit lower triangular matrix (whose main diagonal consists of ones) and an upper triangular matrix. The product of these two matrices is the original matrix. These two matrices are returned as one matrix whose main diagonal is the main diagonal of the upper triangular matrix.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) 

​	_n_ : INTEGER -- the size of matrix _A_ 

**Outputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) storing the lower and upper triangles from the decomposition

**Example Usage:** 

```
    A = RESHAPE((/1.0d0, -1.0d0, 3.0d0, &
                & 1.0d0, 1.0d0, 0.0d0, &
                & 3.0d0, -2.0d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    
    CALL LUdecomp(A,3)
    DO i = 1,3
        WRITE(*,*) A
    END DO
```
Output from the lines above:
```
   1.0000000000000000       -1.0000000000000000        3.0000000000000000     
   1.0000000000000000        2.0000000000000000       -3.0000000000000000     
   3.0000000000000000       0.50000000000000000       -6.5000000000000000 
```
**Implementation:**

```
SUBROUTINE LUdecomp(A,n)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(inout) :: A(n,n)
    REAL*8 :: factor
    INTEGER :: i, j, k

    DO k = 1,n-1
        DO i = k+1,n
            factor = A(i,k)/A(k,k)
            DO j = k+1,n
                A(i,j) = A(i,j) - A(k,j)*factor
            END DO
            A(i,k) = factor
        END DO
    END DO

END SUBROUTINE
```



**Last Modified:** March/2019

