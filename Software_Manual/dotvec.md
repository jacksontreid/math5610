# MATH 5610 Software Manual

### Subroutine: [_dotvec_](../dotvec.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine computes the dot product of two vectors of the same length.

**Inputs:** 

​       _vec1_ : REAL*8 -- an array of size (_length_)

​       _vec2_ : REAL*8 -- an array of size (_length_)

​       _length_ : INTEGER -- length of the vectors

**Outputs:** 

​	_dot_ : REAL*8 -- the dot product of _vec1_ and _vec2_

**Example Usage:** 

```
      vec1 = (/ 0.5d0, 0.6d0, 1.9d0 /)
      vec2 = (/ 0.2d0, 0.4d0, 0.1d0 /)
      CALL dotvec(vec1, vec2, 4, dot)
      WRITE(*,*) dot
```
Output from the lines above:

```
0.53000000000000003 
```

**Implementation:**

```
SUBROUTINE dotvec(vec1, vec2, length, dot)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, DIMENSION(length), INTENT(in) :: vec1, vec2
    REAL*8, INTENT(out) :: dot
    INTEGER :: i

    dot = 0.0d0

    DO i = 1, length
        dot = dot + vec1(i)*vec2(i)
    END DO

END SUBROUTINE
```

**Last Modified:** February/2019