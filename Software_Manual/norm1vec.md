# MATH 5610 Software Manual

### Subroutine: [_norm1vec_](../norm1vec.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine computes the 1-norm of a vector.

**Inputs:** 

​       _vec_ : REAL*8 -- an array of size (_length_)

​       _length_ : INTEGER -- length of the vector

**Outputs:** 

​	_norm_ : REAL*8 -- the 1-norm of the vector

**Example Usage:** 

```
      vec = (/ 0.5d0, 0.6d0, 1.9d0, 2.0d0 /)
      CALL norm1vec(vec, 4, norm)
      WRITE(*,*) norm
```
Output from the lines above:

```
5.0000000000000000  
```

**Implementation:**

```
SUBROUTINE norm1vec(vec, length, norm)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, DIMENSION(length), INTENT(in) :: vec
    REAL*8, INTENT(out) :: norm
    INTEGER :: i

    norm = 0.0d0

    DO i = 1, length
        norm = norm + DABS(vec(i))
    END DO

END SUBROUTINE
```

**Last Modified:** February/2019

