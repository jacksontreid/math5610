# MATH 5610 Software Manual

### Subroutine: [_relerr_](../relerr.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the relative error between two numbers.

**Inputs:** 

​        _approx_ : REAL*8 -- the value whose error is calculated

​	_exact_ : REAL*8 -- the value respect to which the error is calculated

**Outputs:** 

​	_error_ : REAL*8 -- the absolute error of the approximation with respect to the exact value

**Example Usage:** 

```
      approx = 25.0001
      exact = 25.0
      CALL relerr(approx, exact, error)
      WRITE(*,*) error
```
Output from the lines above:
```
3.9999999999906774E-006
```

**Last Modified:** January/2018

