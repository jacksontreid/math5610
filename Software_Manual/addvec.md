# MATH 5610 Software Manual

### Subroutine: [_addvec_](../addvec.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine adds two vectors of the same length.

**Inputs:** 

​       _vec1_ : REAL*8 -- an array of size (_length_)

​       _vec2_ : REAL*8 -- an array of size (_length_)

​       _length_ : INTEGER -- length of the vectors

**Outputs:** 

​	_new_vec_ : REAL*8 -- an array of size (_length_) containing the element-wise summation of _vec1_ and _vec2_

**Example Usage:** 

```
      vec1 = (/ 0.5d0, 0.6d0, 1.9d0, 2.0d0 /)
      vec2 = (/ 0.2d0, 0.4d0, 0.1d0, 1.0d0 /)
      CALL addvec(vec1, vec2, 4, new_vec)
      WRITE(*,*) new_vec
```
Output from the lines above:

```
0.69999999999999996        1.0000000000000000        2.0000000000000000        3.0000000000000000 
```

**Implementation:**

```
SUBROUTINE addvec(vec1, vec2, length, new_vec)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length
    REAL*8, DIMENSION(length), INTENT(in) :: vec1, vec2
    REAL*8, DIMENSION(length), INTENT(out) :: new_vec
    INTEGER :: i

    DO i = 1, length
        new_vec(i) = vec1(i) + vec2(i)
    END DO

END SUBROUTINE
```

**Last Modified:** February/2019