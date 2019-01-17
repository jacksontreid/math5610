PROGRAM test
IMPLICIT NONE

REAL :: seps
REAL*8 :: deps, mat(4,3)
INTEGER :: i


!Test machine precision
WRITE(*,*)
WRITE(*,*) ">>>TEST: MACHINE PRECISION"
    CALL smaceps(seps,i)
    WRITE(*,*) i, seps

    CALL dmaceps(deps,i)
    WRITE(*,*) i, deps

!Test random matrix generator
WRITE(*,*)
WRITE(*,*) ">>>TEST: RANDOM MATRIX"
    CALL randmat(4,3,mat)

    DO i = 1,4
        WRITE(*,*) mat(i,:)
    END DO

END PROGRAM
