'''
Returns the value of the machine rounding unit.

Routines
--------
smaceps()   Returns the single-precision machine rounding unit.
dmaceps()   Returns the double-precision machine rounding unit.

Notes
-----
Machine rounding unit calculations originate from equation on page 23 of
Ascher and Greif's "A First Course in Numerical Methods".

@author: Jackson Reid
'''


def smaceps():
    '''
    Returns the single-precision machine rounding unit.

    RETURNS
    -------
    macheps : float
        The single precision machine rounding unit.
    '''

    return 2**(-24)


def smaceps():
    '''
    Returns the double-precision machine rounding unit.

    RETURNS
    -------
    macheps : float
        The single precision machine rounding unit.
    '''

    return 2**(-53)
