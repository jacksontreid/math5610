# MATH 5610 Software Manual

### Subroutine: [_lsQRhouse_](../lsQRhouse.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute an approximate solution of a least squares problem, using Householder QR factorization.

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_m_,_n_), containing the left hand side of the system

​	_m_ : INTEGER -- the number of data points

​	_n_ : INTEGER -- the number of degrees in the fit

​        _b_ : REAL*8 -- an array of size (_m_), containing the right hand side of the system

**Outputs:** 

​        _x_ : REAL*8 -- an array of size (_n_), containing the fit coefficients

**Example Usage:** 

```
    A = RESHAPE((/1.0d0, 0.0d0, 1.0d0, &
                & 2.0d0, 3.0d0, 5.0d0, &
                & 5.0d0, 3.0d0, -2.0d0, &
                & 3.0d0, 5.0d0, 4.0d0, &
               & -1.0d0, 6.0d0, 3.0d0/),(/5,3/),ORDER=(/2,1/))
    b = (/ 4.0d0, -2.0d0, 5.0d0, -2.0d0, 1.0d0 /)
    
    CALL lsQRhouse(A,5,3,b,x)
    WRITE(*,*) x
    WRITE(*,*)
```
Output from the lines above:
```
  0.34722617354196289       0.39900426742532019      -0.78591749644381259   
```
**Implementation:**

```
SUBROUTINE lsQRhouse(A,m,n,b,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: m, n
    REAL*8, INTENT(in) :: A(m,n), b(m)
    REAL*8, INTENT(out) :: x(n)
    REAL*8 :: y(m), Q(m,m), R(m,n)
    INTEGER :: i

    !Compute QR factorization of A
    CALL QRdecomphouse(A,m,n,Q,R)

    !Multiply the system by Q^T
    CALL multmat(TRANSPOSE(Q),b,m,m,1,y)

    !Solve the upper-triangular system
    CALL backsub(R(1:n,:),n,y(1:n),x)

END SUBROUTINE
```



**Last Modified:** April/2019

