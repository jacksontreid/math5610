!Returns the double-precision machine rounding unit
!@author: Jackson Reid


SUBROUTINE dmaceps(deps, i)
    IMPLICIT NONE

    REAL*8, INTENT(out) :: deps
    REAL*8 :: one, appone
    INTEGER, INTENT(out) :: i

    !Initialize variables to compute the machine value near 1.0
    one = 1.0
    deps = 1.0
    appone = one + deps

    !Iteratively divide the perturbation by 2 to determine when the difference
    !between one and the approximation is zero
    DO i=1,1000

        !Update the perturbation and compute the approximation of one
        deps = deps / 2
        appone = one + deps

        !Compare values and break the loop if difference is zero
        IF(ABS(appone-one) == 0.0) RETURN

    END DO

    !Alert user that loop count has been reached
    WRITE(*,*) "The loop limit has been reached!"

END SUBROUTINE
