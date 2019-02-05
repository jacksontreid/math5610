# MATH 5610 Software Manual

### Subroutine: [_randvec_](../randvec.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will generate a one-dimensional array (vector) with the number of rows specified. The array is populated with random numbers in the range [0,1). (Random number generator can be seeded by calling the [_randseed_](randseed.md) routine.)

**Inputs:** 

​       _r_ : INTEGER -- the number of desired rows in the array

**Outputs:** 

​	_vec_ : REAL*8 -- the array, of size (_r_), containing random values

**Example Usage:** 

```
      r = 4
      CALL randvec(r,vec)
      WRITE(*,*) vec
```
Output from the lines above:
```
0.95935432750334526       0.50626493135725559       0.63550156014060255       0.39751387707198982
```
**Implementation:**

```
SUBROUTINE randvec(r,vec)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r
    REAL*8, INTENT(out) :: vec(r)

    !Fill vector with random numbers
    CALL RANDOM_NUMBER(vec)

END SUBROUTINE
```

**Last Modified:** February/2019