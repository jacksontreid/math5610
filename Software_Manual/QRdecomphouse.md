# MATH 5610 Software Manual

### Subroutine: [_QRdecomphouse_](../QRdecomphouse.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will calculate the QR decomposition of a matrix, using Householder transformations. The decomposition produces an orthonormal matrix, _Q_, and an upper triangular matrix, _R_. The product of these two matrices is the original matrix. 

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_n_,_n_) 

​	_n_ : INTEGER -- the size of matrix _A_ 

**Outputs:** 

​        _Q_ : REAL*8 -- an array of size (_n_,_n_), containing the orthonormal matrix

​        _R_ : REAL*8 -- an array of size (_n_,_n_), containing the upper triangular matrix

**Example Usage:** 

```
    A = RESHAPE((/12.0d0, -51.0d0, 4.0d0, &
                 & 6.0d0, 167.0d0, -68.0d0, &
                & -4.0d0, 24.0d0, -41.0d0/),(/3,3/),ORDER=(/2,1/))
    
    CALL QRdecomphouse(A,3,Q,R)
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
 -0.85714285714285721       0.39428571428571424      -0.33142857142857146     
 -0.42857142857142860      -0.90285714285714291        3.4285714285714308E-002
  0.28571428571428575      -0.17142857142857146      -0.94285714285714284     
 R = 
  -14.000000000000000       -21.000000000000004        14.000000000000000     
   8.8271270634812444E-016  -174.99999999999997        70.000000000000014     
  -9.8382840336013887E-017   0.0000000000000000        35.000000000000000 
```
**Implementation:**

```
SUBROUTINE QRdecomphouse(A,n,Q,R)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(in) :: A(n,n)
    REAL*8, INTENT(out) :: Q(n,n), R(n,n)
    REAL*8 :: normx, s, u1, tau, w(n), H(n,n), temp(n,n)
    INTEGER :: i, l

    !Initialize orthogonal matrix, Q, and transformed matrix R
    R = A
    Q = 0.0d0
    DO i = 1,n
        Q(i,i) = 1.0d0
    END DO

    !Perform transformations
    DO i = 1,n
        l = n-i+1

        ! Find H = I - tau*w*w'
        CALL norm2vec(R(i:n,i),l,normx)
        s = -SIGN(1.0d0,R(i,i))
        u1 = R(i,i) - s*normx
        tau = -s*u1/normx
        w(i:n) = R(i:n,i)/u1
        w(i) = 1.0d0
        CALL outervec(tau*w(i:n),l,w(i:n),l,H(i:n,i:n))

        !R = H*R and Q = Q*H
        CALL multmat(H(i:n,i:n),R(i:n,:),l,l,n,temp(i:n,:))
        R(i:n,:) = R(i:n,:) - temp(i:n,:)
        CALL multmat(Q(:,i:n),H(i:n,i:n),n,l,l,temp(:,i:n))
        Q(:,i:n) = Q(:,i:n) - temp(:,i:n)

    END DO


END SUBROUTINE
```



**Last Modified:** April/2019

