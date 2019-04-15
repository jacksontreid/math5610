# Math 5610 Fundamentals of Computational Mathematics

## Homework #6

- [x] [Task 1](#task-1)
- [x] [Task 2](#task-2)
- [x] [Task 3](#task-3)
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
- Code: [QRdecomphouse.f90](QRdecomphouse.f90)
- Software Manual [entry](Software_Manual/QRdecomphouse.md).
- Gram-Schmidt (Q^T*Q, Hilbert n = 8)

```
  0.1E+01 -0.3E-16  0.5E-16  0.7E-17  0.0E+00 -0.1E-16  0.1E-16 -0.3E-16
 -0.3E-16  0.1E+01 -0.1E-15  0.2E-15 -0.3E-16  0.1E-16  0.1E-15 -0.1E-15
  0.5E-16 -0.1E-15  0.1E+01  0.6E-16 -0.1E-15  0.8E-16 -0.7E-16 -0.4E-16
  0.7E-17  0.2E-15  0.6E-16  0.1E+01 -0.6E-16 -0.1E-15  0.0E+00 -0.3E-16
  0.0E+00 -0.3E-16 -0.1E-15 -0.6E-16  0.1E+01  0.3E-16 -0.3E-15  0.2E-15
 -0.1E-16  0.1E-16  0.8E-16 -0.1E-15  0.3E-16  0.1E+01 -0.1E-15  0.2E-15
  0.1E-16  0.1E-15 -0.7E-16  0.0E+00 -0.3E-15 -0.1E-15  0.1E+01 -0.1E-15
 -0.3E-16 -0.1E-15 -0.4E-16 -0.3E-16  0.2E-15  0.2E-15 -0.1E-15  0.1E+01
```
> Using Householder transformations produces a _Q_ matrix that is orthogonal to machine precision for the Hilbert 8x8 matrix! This is a huge improvement over the Gram-Schmidt algorithms.

### Task 4
Build a code that will solve the least squares problem using QR factorization. Document the code in your software manual.
- 

### Task 5
Build a code that will solve the least squares problem using QR factorization. Document the code in your software manual.
- 

### Task 6
Implement the Jacobi Iteration algorithm for computing a sequence of approximate solutions for the linear system equations, _A_ _x_=_b_ . Include a software manual entry for the code you write. Include at least one example that solves a system of equations with 1000 equations in 1000 unknowns. You can use the code you developed to create a diagonally dominant system.
- 

### Task 7
Repeat the previous task using the Gauss-Seidel algorithm.
- 

### Task 8
Compare the Jacobi and Gauss-Seidel in terms of the number of iterations needed to converge to a given tolerance. For example, compute the number of iterations needed to produce a solution to within four digits of accuracy. Tabulate and/or plot the number of iterations needed for the two methods as the size of the system changes. Do this for large systems of equations - greater than 500 by 500.
- 


### Task 9
Do an internet search for pages that discuss the difference between the solution of the least squares problem using the normal equations and the solution via QR factorization of the matrix. Make sure that you cite the sites you use and include a couple of paragraphs in your own words.
- > 
  >
  > - 

### Task 10
Complete an internet search for sites that discuss the stability of various algorithms used in computing the QR factorization of both rectangular and square matrices. Give a brief description of what you found and include citations for the pages you find.
- > 
  >
  > - 