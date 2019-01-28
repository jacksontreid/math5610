# MATH 5610 Software Manual

### Subroutine: [_scalevec_](../scalevec.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine scales a vector by a constant.

**Inputs:** 

​       _vec_ : REAL*8 -- an array of size (_length_)

​       _length_ : INTEGER -- length of the vector

​       _const_ : REAL*8 -- constant by which the vector is scaled

**Outputs:** 

​	_vec_ : REAL*8 -- an array of size (_length_) containing the scaled values

**Example Usage:** 

```
      vec = (/ 0.5, 0.6, 1.9, 2.0 /)
      CALL addvec(vec, 4, DBLE(2.))
      WRITE(*,*) vec
```
Output from the lines above:

```
1.0000000000000000        1.2000000000000000        3.7999999999999998        4.0000000000000000  
```

**Last Modified:** January/2018