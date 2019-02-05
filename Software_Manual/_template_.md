# MATH 5610 Software Manual

### Subroutine: [_temp_](../temp.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute the single-precision machine rounding unit of a computer, and the number of digits used in the representation of real numbers in single precision.

**Inputs:** 

​       _in1_ : REAL*8 -- the single-precision machine rounding unit

​	_in2_ : INTEGER -- the number of digits in the representation of real numbers in single precision

**Outputs:** 

​	_out1_ : REAL*8 -- the single-precision machine rounding unit

​	_out2_ : INTEGER -- the number of digits in the representation of real numbers in single precision

**Example Usage:** 

```
      CALL temp(in1, in2, out1, out2)
      WRITE(*,*) out1, out2
```
Output from the lines above:
```

```
**Implementation:**

```

```



**Last Modified:** MONTH/YEAR

