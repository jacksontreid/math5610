# MATH 5610 Software Manual

### Subroutine: [_choldecomp_](../choldecomp.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will calculate the Cholesky decomposition of a matrix. The decomposition produces a lower-triangular matrix. The product of this lower-triangular matrix and its transpose is the original matrix. 

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing the original values

​	_n_ : INTEGER -- the size of matrix _A_ 

**Outputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) storing the decomposition in its lower triangle with the other values unchanged

​       _error_ : INTEGER -- flag describing if decomposition was successful (0) or not (1)

**Example Usage:** 

```
    A = RESHAPE((/4.0d0, 12.0d0, -16.0d0, &
               & 12.0d0, 37.0d0, -43.0d0, &
              & -16.0d0, -43.0d0, 98.0d0/),(/3,3/),ORDER=(/2,1/))
    
    CALL choldecomp(A,3,error)
    DO i = 1,3
        WRITE(*,*) A
    END DO
```
Output from the lines above:
```
   2.0000000000000000        12.000000000000000       -16.000000000000000     
   6.0000000000000000        1.0000000000000000       -43.000000000000000     
  -8.0000000000000000        5.0000000000000000        3.0000000000000000  
```
**Implementation:**

```
SUBROUTINE choldecomp(A,n,error)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(inout) :: A(n,n)
    INTEGER, INTENT(out) :: error
    INTEGER :: i, j, k

    DO k = 1,n-1
        !Check for decomposition failure
        IF (A(k,k) <= 0.0d0) THEN
            WRITE(*,*) "Cholesky Decomposition FAILURE!"
            error = 1
            RETURN
        END IF
        A(k,k) = SQRT(A(k,k))

        DO i = k+1,n
            A(i,k) = A(i,k)/A(k,k)
        END DO

        DO j = k+1,n
            DO i = j,n
                A(i,j) = A(i,j) - A(i,k)*A(j,k)
            END DO
        END DO
    END DO

    !Check for decomposition failure
    IF (A(k,k) < 0.0d0) THEN
        WRITE(*,*) "Cholesky Decomposition FAILURE!"
        error = 1
        RETURN
    END IF
    A(n,n) = SQRT(A(n,n))

    error = 0

END SUBROUTINE
```



**Last Modified:** March/2019

