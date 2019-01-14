PROGRAM test
IMPLICIT NONE

REAL :: seps
REAL*8 :: deps
INTEGER :: i

CALL smaceps(seps,i)
WRITE(*,*) i, seps

CALL dmaceps(deps,i)
WRITE(*,*) i, deps

END PROGRAM
