# MATH 5610 Software Manual

### Subroutine: [_randspdmat_](../randspdmat.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will generate a two-dimensional symmetric-positive-definite array (matrix) of the size specified. (Random number generator can be seeded by calling the [_randseed_](randseed.md) routine.)

**Inputs:** 

​	_n_ : INTEGER -- the number of desired rows and columns in the array

**Outputs:** 

​	_mat_ : REAL*8 -- the array, of size (_n_,_n_), containing random, symmetric-positive-definite values

**Example Usage:** 

```
      n = 3
      CALL randspdmat(n,mat)
      DO i = 1,3
         WRITE(*,*) mat(i,:)
      END DO
```
Two example outputs from the lines above:
```
   1.6434484911634828       0.77904129563761093       0.88527030214543578     
  0.77904129563761093       0.54818301874388597       0.19254654780399083     
  0.88527030214543578       0.19254654780399083       0.80874482574867268
```
```
   1.0593445466682698        1.3379835800607713        1.0859530777257471     
   1.3379835800607713        2.2056506896334360        1.6021435321874291     
   1.0859530777257471        1.6021435321874291        1.3059182174936366 
```
**Implementation:**

```
SUBROUTINE randspdmat(n,mat)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(out) :: mat(n,n)
    REAL*8 :: tempmat(n,n)
    INTEGER :: error

    error = 1

    DO WHILE (error == 1)
        !Fill matrix with random numbers
        CALL RANDOM_NUMBER(tempmat)

        !Multiply random matrix by its transpose
        CALL multmat(tempmat,TRANSPOSE(tempmat),n,n,n,mat)

        !Check that matrix is positive definate
        tempmat = mat
        CALL choldecomp(tempmat,n,error)
    END DO

END SUBROUTINE
```

**Last Modified:** March/2019