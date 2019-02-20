# MATH 5610 Software Manual

### Subroutine: [_outervec_](../outervec.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine computes the outer product of two vectors.

**Inputs:** 

​       _vec1_ : REAL*8 -- an array of size (_r_)

​       _r_ : INTEGER -- the length of _vec1_

​       _vec2_ : REAL*8 -- an array of size (_c_)

​       _c_ : INTEGER -- the length of _vec2_

**Outputs:** 

​	_outer_ : REAL*8 -- an array, of size (_r_,_c_), containing the outer product of _vec1_ and _vec2_

**Example Usage:** 

```
      vec1 = (/ 0.5d0, 0.6d0, 1.9d0, 2.0d0 /)
      vec2 = (/ 0.2d0, 0.4d0, 0.1d0 /)
      CALL dotvec(vec1, 4, vec2, 3, outer)
      DO i = 1,4
          WRITE(*,*) outer(i,:)
      END DO
```
Output from the lines above:

```
  0.10000000000000001       0.20000000000000001       5.0000000000000003E-002
  0.12000000000000000       0.23999999999999999       5.9999999999999998E-002
  0.38000000000000000       0.76000000000000001       0.19000000000000000     
  0.40000000000000002       0.80000000000000004       0.20000000000000001 
```

**Implementation:**

```
SUBROUTINE outervec(vec1, r, vec2, c, outer)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, c
    REAL*8, INTENT(in) :: vec1(r), vec2(c)
    REAL*8, INTENT(out) :: outer(r,c)
    INTEGER :: i, j

    DO i = 1, r
        DO j = 1, c

            outer(i,j) = vec1(i)*vec2(j)

        END DO
    END DO

END SUBROUTINE
```

**Last Modified:** February/2019