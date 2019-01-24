# MATH 5610 Software Manual

### Subroutine: [_norminfvec_](../norminfvec.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine computes the infinity-norm of a vector.

**Inputs:** 

​       _vec_ : REAL*8 -- an array of size (_length_)

​       _length_ : INTEGER -- length of the vector

**Outputs:** 

​	_norm_ : REAL*8 -- the infinity-norm of the vector

**Example Usage:** 

```
      vec = (/ 0.5, 0.6, 1.9, 2.0 /)
      CALL norminfvec(vec, 4, norm)
      WRITE(*,*) norm
```
Output from the lines above:

```
2.0000000000000000  
```

**Last Modified:** January/2018

