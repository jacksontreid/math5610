# MATH 5610 Software Manual

### Subroutine: [_solveLUfactor_](../solveLUfactor.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the solution of a square linear system, _A_ _x_ = _b_, using LU-decomposition, forward-substitution, and back-substitution.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) containing non-zero values of _A_, or the _LU_-decomposition of _A_ (such that the upper-triangle of _A_ is _U_, and the remaining values are _L_ without its main diagonal of ones)

​	_n_ : INTEGER -- the number describing the size of _A_ and _b_

​        _b_ : REAL*8 -- an array of size (n) containing the values on the right-hand-side of the linear system

​	_decomp_ : LOGICAL -- flag signifying if the decomposition step the routine must be performed

**Outputs:** 

​        _x_ : REAL*8 -- an array of size (n) containing the solution to the linear system

**Example Usage:** 

```
    A = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                & 0.6d0, 0.4d0, 0.1d0, &
                & 1.9d0, 0.1d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    b = (/ 0.2d0, 0.4d0, 0.1d0 /)

    CALL solveLUfactor(A,3,b,x,.TRUE.)
    WRITE(*,*) x
```
Output from the lines above:
```
  0.10985915492957729       0.88450704225352117      -0.19718309859154928 
```
**Implementation:**

```
SUBROUTINE solveLUfactor(A,n,b,x,decomp)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    LOGICAL, INTENT(in) :: decomp
    REAL*8, INTENT(in) :: b(n)
    REAL*8, INTENT(inout) :: A(n,n)
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: diag(n), y(n)
    INTEGER :: i

    !Decompose A if it is not already decomposed
    IF (decomp) THEN
        !Perform LU-decompositino on the matrix
        CALL LUdecomp(A,n)
    END IF

    !Add ones to diagonal to extract lower-triangular matrix
    DO i = 1, n
        diag(i) = A(i,i)
        A(i,i) = 1.0d0
    END DO
    !Use forward-substitution to determine the system solution
    CALL forsub(A,n,b,y)

    !Replace diagonal to restore upper-triangular matrix
    DO i = 1, n
        A(i,i) = diag(i)
    END DO
    !Use back-substitution to determine the system solution
    CALL backsub(A,n,y,x)

END SUBROUTINE
```



**Last Modified:** March/2019

