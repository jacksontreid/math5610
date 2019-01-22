PROGRAM test
IMPLICIT NONE

REAL :: seps
REAL*8 :: deps, mat(4,3), a_error, num1, num2
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

!Test error calculators
WRITE(*,*)
WRITE(*,*) ">>>TEST: ERROR CALCS"
    num1 = 25.0001
    num2 = 25.0
    CALL abserr(num1,num2,a_error)
    WRITE(*,*) a_error

    CALL relerr(num1,num2,a_error)
    WRITE(*,*) a_error

END PROGRAM
