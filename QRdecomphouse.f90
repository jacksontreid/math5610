!Calculates the QR decomposition of a matrix using Householder transformations.
!http://www.cs.cornell.edu/~bindel/class/cs6210-f09/lec18.pdf
!@author: Jackson Reid


SUBROUTINE QRdecomphouse(A,m,n,Q,R)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: m, n
    REAL*8, INTENT(in) :: A(m,n)
    REAL*8, INTENT(out) :: Q(m,m), R(m,n)
    REAL*8 :: normx, s, u1, tau, w(m), H(m,m), temp(m,m)
    INTEGER :: i, l

    !Initialize orthogonal matrix, Q, and transformed matrix R
    R = A
    Q = 0.0d0
    DO i = 1,m
        Q(i,i) = 1.0d0
    END DO

    !Perform transformations
    DO i = 1,n
        l = m-i+1

        ! Find H = I - tau*w*w'
        CALL norm2vec(R(i:,i),l,normx)
        s = -SIGN(1.0d0,R(i,i))
        u1 = R(i,i) - s*normx
        tau = -s*u1/normx
        w(i:) = R(i:,i)/u1
        w(i) = 1.0d0
        CALL outervec(tau*w(i:),l,w(i:),l,H(i:,i:))

        !R = H*R and Q = Q*H
        CALL multmat(H(i:,i:),R(i:,:),l,l,n,temp(i:,:))
        R(i:,:) = R(i:,:) - temp(i:,:)
        CALL multmat(Q(:,i:),H(i:,i:),m,l,l,temp(:,i:))
        Q(:,i:) = Q(:,i:) - temp(:,i:)

    END DO


END SUBROUTINE
