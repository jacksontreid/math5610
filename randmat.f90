!Generates a matrix of size (r,c) populated with random values, 0 <= a < 1.
!@author Jackson Reid

SUBROUTINE randmat(r,c,mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r,c
    REAL*8, INTENT(out) :: mat(r,c)
    INTEGER :: values(8), k
    INTEGER, ALLOCATABLE :: seed(:)

    !Get system clock values for random number seeding
    CALL DATE_AND_TIME(values=values)

    !Seed random number generator
    CALL RANDOM_SEED(size=k)
    ALLOCATE(seed(k))
    seed(:) = values(8)
    CALL RANDOM_SEED(put=seed)

    !Fill matrix with random numbers
    CALL RANDOM_NUMBER(mat)

END SUBROUTINE

