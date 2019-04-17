# MATH 5610 Software Manual

### Subroutine: [_QRdecompmod_](../QRdecompmod.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will calculate the QR decomposition of a matrix, using modified Gram-Schmidt. The decomposition produces an orthonormal matrix, _Q_, and an upper triangular matrix, _R_. The product of these two matrices is the original matrix. 

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_m_,_n_) 

​        _m_ : INTEGER -- the number of rows in matrix _A_ 

​	_n_ : INTEGER -- the number of columns in matrix _A_ 

**Outputs:** 

​        _Q_ : REAL*8 -- an array of size (_m_,_n_), containing the orthonormal matrix

​        _R_ : REAL*8 -- an array of size (_n_,_n_), containing the upper triangular matrix

**Example Usage:** 

```
    A = RESHAPE((/12.0d0, -51.0d0, 4.0d0, &
                 & 6.0d0, 167.0d0, -68.0d0, &
                & -4.0d0, 24.0d0, -41.0d0/),(/3,3/),ORDER=(/2,1/))
    
    CALL QRdecompmod(A,3,3,Q,R)
    WRITE(*,*) "Q = "
    DO i = 1,3
        WRITE(*,*) Q(i,:)
    END DO
    WRITE(*,*) "R = "
    DO i = 1,3
        WRITE(*,*) R(i,:)
    END DO
```
Output from the lines above:
```
 Q = 
  0.85714285714285710      -0.39428571428571429      -0.33142857142857124     
  0.42857142857142855       0.90285714285714280        3.4285714285713760E-002
 -0.28571428571428570       0.17142857142857143      -0.94285714285714284     
 R = 
   14.000000000000000        20.999999999999996       -14.000000000000002     
   0.0000000000000000        175.00000000000000       -69.999999999999986     
   0.0000000000000000        0.0000000000000000        35.000000000000000
```
**Implementation:**

```
SUBROUTINE QRdecompmod(A,m,n,Q,R)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: m, n
    REAL*8, INTENT(in) :: A(m,n)
    REAL*8, INTENT(out) :: Q(m,n), R(n,n)
    REAL*8 :: factor
    INTEGER :: i, j

    R = 0.0d0

    DO j = 1,n
        Q(:,j) = A(:,j)
        DO i = 1,j-1
            CALL dotvec(Q(:,j),Q(:,i),m,R(i,j))
            Q(:,j) = Q(:,j) - R(i,j)*Q(:,i)
        END DO
        CALL norm2vec(Q(:,j),m,R(j,j))
        Q(:,j) = Q(:,j)/R(j,j)
    END DO

END SUBROUTINE
```



**Last Modified:** April/2019

