!Computes the solution of a square linear system using LU-decomposition,
!forward-substitution, and back-substitution.
!@author: Jackson Reid


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
