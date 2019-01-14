!Returns the value of the machine rounding unit.
!
!Routines
!--------
!smaceps()   Returns the single-precision machine rounding unit.
!dmaceps()   Returns the double-precision machine rounding unit.
!
!@author: Jackson Reid


subroutine smaceps(seps, ipow)
!    c
!    c set up storage for the algorithm
!    c --------------------------------
!    c
          real seps, one, appone
!    c
!    c initialize variables to compute the machine value near 1.0
!    c ----------------------------------------------------------
!    c
          one = 1.0
          seps = 1.0
          appone = one + seps
!    c
!    c loop, dividing by 2 each time to determine when the difference between one and
!    c the approximation is zero in single precision
!    c ---------------------------------------------
!    c
          ipow = 0
          do 1 i=1,1000
             ipow = ipow + 1
!    c
!    c update the perturbation and compute the approximation to one
!    c ------------------------------------------------------------
!    c
            seps = seps / 2
            appone = one + seps
!    c
!    c do the comparison and if small enough, break out of the loop and return
!    c control to the calling code
!    c ---------------------------
!    c
            if(abs(appone-one) .eq. 0.0) return
!    c
        1 continue
!    c
!    c if the code gets to this point, there is a bit of trouble
!    c ---------------------------------------------------------
!    c
          print *,"The loop limit has been exceeded"
!    c
!    c done
!    c ----
!    c
          return
    end

!def smaceps():
!    '''
!    Returns the single-precision machine rounding unit.

!    RETURNS
!    -------
!    macheps : float
!        The single precision machine rounding unit.
!    '''

!    return 2**(-24)


!def smaceps():
!    '''
!    Returns the double-precision machine rounding unit.

!    RETURNS
!    -------
!    macheps : float
!        The single precision machine rounding unit.
!    '''

!    return 2**(-53)
