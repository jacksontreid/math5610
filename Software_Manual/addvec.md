# MATH 5610 Software Manual

### Subroutine: [_addvec_](../addvec.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be compiled using the GNU Fortran compiler (gfortran).

This routine can be linked to a program with the commands
```
    $ gfortran -c addvec.f90
    $ gfortran myprogram.f90 addvec.o
```

Or, a library can be created from this routine

```
    $ gfortran -c addvec.f90
    $ ar rcv mylib addvec.o
```

**Description:** This routine adds two vectors of the same length.

**Inputs:** 

​       _vec1_ : REAL*8 -- an array of size (_length_)

​       _vec2_ : REAL*8 -- an array of size (_length_)

​       _length_ : INTEGER -- length of the vectors

**Outputs:** 

​	_new_vec_ : REAL*8 -- an array of size (_length_) containing the element-wise summation of _vec1_ and _vec2_

**Example Usage:** 

```
      vec1 = (/ 0.95935432750334526       0.50626493135725559       0.63550156014060255       0.39751387707198982 /)
      vec2 = (/ 0.78487619993830038       0.79987983474518221       0.16138729018585640       0.85747822372292093 /)
      CALL addvec(vec1, vec2, length, new_vec)
      WRITE(*,*) new_vec
```
Output from the lines above:

```
1.7442305274416456        1.3061447661024377       0.79688885032645895        1.2549921007949107
```

**Last Modified:** January/2018