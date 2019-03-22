# MATH 5610 Software Manual

### Subroutine: [_solvegausselim2_](../solvegausselim2.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the solution of a square linear system, _A_ _x_ = _b_, using Gaussian elimination and back-substitution, without calling external routines.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing non-zero values only in the upper triangle

​	_n_ : INTEGER -- the number describing the size of _A_ and _b_

​        _b_ : REAL*8 -- an array of size (n) containing the values on the right-hand-side of the linear system

**Outputs:** 

​        _x_ : REAL*8 -- an array of size (n) containing the solution to the linear system

**Example Usage:** 

```
    A = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                & 0.6d0, 0.4d0, 0.1d0, &
                & 1.9d0, 0.1d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    b = (/ 0.2d0, 0.4d0, 0.1d0 /)

    CALL solvegausselim2(A,3,b,x)
    WRITE(*,*) x
```
Output from the lines above:
```
  0.10985915492957729       0.88450704225352117      -0.19718309859154928 
```
**Implementation:**

```
SUBROUTINE solvegausselim2(A,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(in) :: A(n,n), b(n)
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: A_aug(n,n+1), factor
    INTEGER :: i,j,k

    !Create augmented coefficient matrix
    A_aug(:,1:n) = A(:,:)
    A_aug(:,n+1) = b(:)

    !Reduce augmented coefficient matrix to row echelon form
    DO k = 1,n-1
        DO i = k+1,n
            factor = A_aug(i,k)/A_aug(k,k)
            DO j = k,n+1
                A_aug(i,j) = A_aug(i,j) - A_aug(k,j)*factor
            END DO
        END DO
    END DO

    !Use back-substitution to determine the system solution
    x(n) = A_aug(n,n+1)/A_aug(n,n)
    DO i = n-1,1,-1
        x(i) = A_aug(i,n+1)
        DO j = i+1,n
            x(i) = x(i) - x(j)*A_aug(i,j)
        END DO
        x(i) = x(i)/A_aug(i,i)
    END DO

END SUBROUTINE
```



**Last Modified:** March/2019

