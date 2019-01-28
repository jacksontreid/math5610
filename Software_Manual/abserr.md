# MATH 5610 Software Manual

### Subroutine: [_abserr_](../abserr.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the absolute error between two numbers.

**Inputs:** 

​        _approx_ : REAL*8 -- the value whose error is calculated

​	_exact_ : REAL*8 -- the value respect to which the error is calculated

**Outputs:** 

​	_error_ : REAL*8 -- the absolute error of the approximation with respect to the exact value

**Example Usage:** 

```
      approx = 25.0001
      exact = 25.0
      CALL abserr(approx, exact, error)
      WRITE(*,*) error
```
Output from the lines above:
```
9.9999999999766942E-005
```

**Last Modified:** January/2018

