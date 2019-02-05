# MATH 5610 Software Manual

### Subroutine: [_randseed_](../randseed.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine seeds Fortran's intrinsic random number generator based on the milliseconds on the system clock.

**Inputs:** None.

**Outputs:** None.

**Example Usage:** 

```
      CALL randseed()
```
**Implementation:**

```
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
```

**Last Modified:** February/2019