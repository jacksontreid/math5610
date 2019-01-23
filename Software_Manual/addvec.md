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
      vec1 = (/ 0.5, 0.6, 1.9, 2.0 /)
      vec2 = (/ 0.2, 0.4, 0.1, 1.0 /)
      CALL addvec(vec1, vec2, 4, new_vec)
      WRITE(*,*) new_vec
```
Output from the lines above:

```
0.70000000298023224        1.0000000298023224        1.9999999776482582        3.0000000000000000  
```

**Last Modified:** January/2018