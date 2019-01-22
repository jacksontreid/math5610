PROGRAM test
IMPLICIT NONE

REAL :: seps
REAL*8 :: deps, mat(4,3), a_error, num1, num2, vec1(4), vec2(4), vec3(4)
INTEGER :: i

!Seed random number generator
CALL randseed()

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

!Test vector creation, addition, and scaling
WRITE(*,*)
WRITE(*,*) ">>>TEST: VECTOR ADDITION AND SCALING"
    CALL randvec(4,vec1)
    WRITE(*,*) vec1
    CALL randvec(4,vec2)
    WRITE(*,*) vec2

    CALL addvec(vec1,vec2,4,vec3)
    WRITE(*,*) vec3

END PROGRAM
