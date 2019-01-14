!Returns the single-precision machine rounding unit
!@author: Jackson Reid


SUBROUTINE smaceps(seps, i)
    IMPLICIT NONE

    REAL, INTENT(out) :: seps
    REAL :: one, appone
    INTEGER, INTENT(out) :: i

    !Initialize variables to compute the machine value near 1.0
    one = 1.0
    seps = 1.0
    appone = one + seps

    !Iteratively divide the perturbation by 2 to determine when the difference
    !between one and the approximation is zero
    DO i=1,1000

        !Update the perturbation and compute the approximation of one
        seps = seps / 2.
        appone = one + seps

        !Compare values and break the loop if difference is zero
        IF(ABS(appone-one) == 0.0) RETURN

    END DO

    !Alert user that loop count has been reached
    WRITE(*,*) "The loop limit has been reached!"

END SUBROUTINE
