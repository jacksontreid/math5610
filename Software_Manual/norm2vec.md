# MATH 5610 Software Manual

### Subroutine: [_norm2vec_](../norm2vec.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine computes the 2-norm of a vector.

**Inputs:** 

​       _vec_ : REAL*8 -- an array of size (_length_)

​       _length_ : INTEGER -- length of the vector

**Outputs:** 

​	_norm_ : REAL*8 -- the 2-norm of the vector

**Example Usage:** 

```
      vec = (/ 0.5d0, 0.6d0, 1.9d0, 2.0d0 /)
      CALL norm2vec(vec, 4, norm)
      WRITE(*,*) norm
```
Output from the lines above:

```
2.8670542373662902 
```

**Implementation:**

```
SUBROUTINE norm2vec(vec, length, norm)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, DIMENSION(length), INTENT(in) :: vec
    REAL*8, INTENT(out) :: norm
    INTEGER :: i

    norm = 0.0d0

    DO i = 1, length
        norm = norm + DABS(vec(i))**2
    END DO

    norm = DSQRT(norm)

END SUBROUTINE
```

**Last Modified:** February/2019

