!Calculates the QR decomposition of a matrix using Householder transformations.
!http://www.cs.cornell.edu/~bindel/class/cs6210-f09/lec18.pdf
!@author: Jackson Reid


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
