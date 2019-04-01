# MATH 5610 Software Manual

### Subroutine: [_lsnormal_](../lsnormal.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute an approximate solution of a least squares problem, using the normal equations.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_r_,_c_), containing the left hand side of the system

​	_r_ : INTEGER -- the number of data points

​	_c_ : INTEGER -- the number of degrees in the fit

​        _b_ : REAL*8 -- an array of size (_r_), containing the right hand side of the system

**Outputs:** 

​        _x_ : REAL*8 -- an array of size (_c_), containing the fit coefficients

**Example Usage:** 

```
    A = RESHAPE((/1.0d0, 0.0d0, 1.0d0, &
                & 2.0d0, 3.0d0, 5.0d0, &
                & 5.0d0, 3.0d0, -2.0d0, &
                & 3.0d0, 5.0d0, 4.0d0, &
               & -1.0d0, 6.0d0, 3.0d0/),(/5,3/),ORDER=(/2,1/))
    b = (/ 4.0d0, -2.0d0, 5.0d0, -2.0d0, 1.0d0 /)
    
    CALL lsnormal(A,5,3,b,x)
    WRITE(*,*) x
    WRITE(*,*)
```
Output from the lines above:
```
  0.34722617354196289       0.39900426742532014      -0.78591749644381226 
```
**Implementation:**

```
SUBROUTINE lsnormal(A,r,c,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, c
    REAL*8, INTENT(in) :: A(r,c), b(r)
    REAL*8, INTENT(out) :: x(c)
    REAL*8 :: Asq(c,c), y(c)
    INTEGER :: error

    !Multiply the system by A^T
    CALL multmat(TRANSPOSE(A),A,c,r,c,Asq)
    CALL multmat(TRANSPOSE(A),b,c,r,1,x)

    !Compute Cholesky factorization of Asq
    CALL choldecomp(Asq,c,error)

    !Solve the lower-triangular system
    CALL forsub(Asq,c,x,y)

    !Solve the upper-triangular system
    CALL backsub(TRANSPOSE(Asq),c,y,x)

END SUBROUTINE
```



**Last Modified:** April/2019

