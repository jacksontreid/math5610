!Seeds FORTRAN's intrinsic random number generator.
!@author Jackson Reid

SUBROUTINE randseed()
    IMPLICIT NONE

    INTEGER :: values(8), k
    INTEGER, ALLOCATABLE :: seed(:)

    !Get system clock values for random number seeding
    CALL DATE_AND_TIME(values=values)

    !Seed random number generator
    CALL RANDOM_SEED(size=k)
    ALLOCATE(seed(k))
    seed(:) = values(8)
    CALL RANDOM_SEED(put=seed)

END SUBROUTINE

