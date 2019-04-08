# Math 5610 Fundamentals of Computational Mathematics

## Homework #6

- [x] [Task 1](#task-1)
- [x] [Task 2](#task-2)
- [ ] [Task 3](#task-3)
- [ ] [Task 4](#task-4)
- [ ] [Task 5](#task-5)
- [ ] [Task 6](#task-6)
- [ ] [Task 7](#task-7)
- [ ] [Task 8](#task-8)
- [ ] [Task 9](#task-9)
- [ ] [Task 10](#task-10)

### Task 1
Implement a method that will compute the solution of a square linear system of equations using the QR-factorization of the matrix. Give examples and document the code in your software manual.
- Code: [solveQRfactor.f90](solveQRfactor.f90)
- Software Manual [entry](Software_Manual/solveQRfactor.md).


### Task 2
Create another version of the QR-factorization algorithm using the Modified Gram-Schmidt process. Document the code in your software manual. For examples, use the same matrices used when testing the modified version. Compare the results to the first version of the QR-factorization.
- Code: [QRdecompmod.f90](QRdecompmod.f90)
- Software Manual [entry](Software_Manual/QRdecompmod.md).
- Gram-Schmidt (Q^T*Q, Hilbert n = 8)

```
  0.1E+01 -0.7E-15  0.7E-14 -0.1E-12  0.3E-11 -0.1E-09  0.6E-08 -0.6E-08
 -0.7E-15  0.1E+01  0.4E-13 -0.5E-12  0.6E-11 -0.5E-10 -0.2E-08  0.2E-08
  0.7E-14  0.4E-13  0.1E+01 -0.1E-10  0.3E-09 -0.6E-08  0.1E-06 -0.1E-06
 -0.1E-12 -0.5E-12 -0.1E-10  0.1E+01  0.1E-07 -0.4E-06  0.1E-04 -0.1E-04
  0.3E-11  0.6E-11  0.3E-09  0.1E-07  0.1E+01 -0.2E-04  0.2E-02 -0.2E-02
 -0.1E-09 -0.5E-10 -0.6E-08 -0.4E-06 -0.2E-04  0.1E+01  0.1E+00 -0.1E+00
  0.6E-08 -0.2E-08  0.1E-06  0.1E-04  0.2E-02  0.1E+00  0.1E+01 -0.1E+01
 -0.6E-08  0.2E-08 -0.1E-06 -0.1E-04 -0.2E-02 -0.1E+00 -0.1E+01  0.1E+01
```
- Modified Gram-Schmidt (Q^T*Q, Hilbert n = 8)
```
  0.1E+01 -0.7E-15  0.6E-14 -0.1E-12  0.3E-11 -0.1E-09  0.6E-08 -0.4E-06
 -0.7E-15  0.1E+01  0.4E-14 -0.3E-13 -0.3E-12  0.4E-10 -0.3E-08  0.3E-06
  0.6E-14  0.4E-14  0.1E+01  0.2E-14  0.2E-13 -0.4E-11  0.3E-09 -0.2E-07
 -0.1E-12 -0.3E-13  0.2E-14  0.1E+01 -0.2E-14  0.2E-12 -0.3E-10  0.4E-08
  0.3E-11 -0.3E-12  0.2E-13 -0.2E-14  0.1E+01  0.2E-13 -0.1E-11  0.5E-10
 -0.1E-09  0.4E-10 -0.4E-11  0.2E-12  0.2E-13  0.1E+01 -0.8E-14 -0.8E-13
  0.6E-08 -0.3E-08  0.3E-09 -0.3E-10 -0.1E-11 -0.8E-14  0.1E+01  0.8E-14
 -0.4E-06  0.3E-06 -0.2E-07  0.4E-08  0.5E-10 -0.8E-13  0.8E-14  0.1E+01
```

> The modified Gram-Schmidt algorithm produces a _Q_ matrix that is much closer to being orthonormal than the classical algorithm.

### Task 3

Create a third version of the QR-factorization algorithm using Householder Transformations. As usual, document you code in your software manual. Use the third incarnation of the code on the same matrices as the previous two QR-factorization and compare/explain your results.
- 

### Task 4
Build a code that will solve the least squares problem using QR factorization. Document the code in your software manual.
- 

### Task 5

- 

### Task 6

- 

### Task 7

- 

### Task 8


- 


### Task 9

- 

### Task 10


- > 
  >
  > - 