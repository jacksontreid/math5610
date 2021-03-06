# Math 5610 Fundamentals of Computational Mathematics

## Homework #6

- [x] [Task 1](#task-1)
- [x] [Task 2](#task-2)
- [x] [Task 3](#task-3)
- [x] [Task 4](#task-4)
- [x] [Task 5](#task-5)
- [x] [Task 6](#task-6)
- [x] [Task 7](#task-7)
- [x] [Task 8](#task-8)
- [x] [Task 9](#task-9)
- [x] [Task 10](#task-10)

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
Build a code that will solve the least squares problem using QR factorization. Document the code in your software manual. Use the modified Gram-Schmidt algorithm to compute the QR factorization.
- Code: [lsQRgsmod.f90](lsQRgsmod.f90)
- Software Manual [entry](Software_Manual/lsQRgsmod.md).

### Task 5
Build a code that will solve the least squares problem using QR factorization. Document the code in your software manual.
- Code: [lsQRhouse.f90](lsQRhouse.f90)
- Software Manual [entry](Software_Manual/lsQRhouse.md).

### Task 6
Implement the Jacobi Iteration algorithm for computing a sequence of approximate solutions for the linear system equations, _A_ _x_=_b_ . Include a software manual entry for the code you write. Include at least one example that solves a system of equations with 1000 equations in 1000 unknowns. You can use the code you developed to create a diagonally dominant system.
- Code: [solvejacobi.f90](solvejacobi.f90)

- Software Manual [entry](Software_Manual/solvejacobi.md).

  > See Task 8 for large system result

### Task 7
Repeat the previous task using the Gauss-Seidel algorithm.
- Code: [solvegaussseidel.f90](solvegaussseidel.f90)

- Software Manual [entry](Software_Manual/solvegaussseidel.md).

  > See Task 8 for large system result

### Task 8
Compare the Jacobi and Gauss-Seidel in terms of the number of iterations needed to converge to a given tolerance. For example, compute the number of iterations needed to produce a solution to within four digits of accuracy. Tabulate and/or plot the number of iterations needed for the two methods as the size of the system changes. Do this for large systems of equations - greater than 500 by 500.
- For the system _A_ _x_ = _b_ -- where _A_ is a random, diagonally-dominant matrix and _b_ is generated from the known solution _x_ = [1.0] -- the solution is calculated within a tolerance of 10E-10.

|   n    | Jacobi Iter. |      Jacobi Error       | GS Iter. |        GS Error         |
| :----: | :----------: | :---------------------: | :------: | :---------------------: |
|   10   |      46      | 2.9589071957989963E-010 |    13    | 1.2124514010849118E-010 |
|  100   |      63      | 3.5940887506048294E-010 |    14    | 5.5617615886132428E-011 |
| 1,000  |      68      | 3.9336334230202365E-010 |    15    | 2.2027483520222965E-011 |
| 10,000 |      72      | 3.1469510181285337E-010 |    15    | 7.1753017680488300E-011 |

### Task 9
Do an internet search for pages that discuss the difference between the solution of the least squares problem using the normal equations and the solution via QR factorization of the matrix. Make sure that you cite the sites you use and include a couple of paragraphs in your own words.
- > The principle draw-back to the normal equations is the potential exaggeration of the poor conditioning. For example, because the system to be solved is _A_^T _A_ _x_ = _b_, if the condition number of _A_ is "large", the condition number of _A_^T _A_ is quadratically larger.  That being said, for well-conditioned systems with many more rows than columns, the normal equations can be faster than QR factorization. The speed advantage, however, is usually not deemed worth the decrease in robustness, and thus QR factorization is typically the default for computing least squares problems.
  >
  > - https://stats.stackexchange.com/questions/343069/why-not-use-the-normal-equations-to-find-simple-least-squares-coefficients
  > - https://www.quora.com/Is-it-better-to-do-QR-Cholesky-or-SVD-for-solving-least-squares-estimate-and-why

### Task 10
Complete an internet search for sites that discuss the stability of various algorithms used in computing the QR factorization of both rectangular and square matrices. Give a brief description of what you found and include citations for the pages you find.
- > There are two main approaches of QR factorization algorithms: first, successively multiplying _A_ by upper-triangular matrices, to obtain an orthonormal matrix, _Q_, then inverting the triangular matrices to obtain _R_; second, successively multiplying _A_ by orthonormal matrices, to obtain an upper-triangular matrix, _R_, then inverting the orthogonal matrices to obtain _Q_. The first method is applied in the Gram-Schmidt and modified Gram-Schmidt algorithms, and the second method is applied in the algorithms employing Housholder transformations and Givens rotations. The advantage that the Householder and Givens algorithms has is due to the stability of orthogonal matrices. Orthogonal matrices have a condition number of one, so, error amplification does not occur in the successive multiplication and inversion process used in the second category of algorithms. The upper triangular matrices used in the first set of algorithms, however, have condition numbers on the order of that of the original matrix, thus, these algorithms have the propensity to amplify error throughout the algorithm if the original matrix is poorly conditioned.
  >
  > - https://www-old.math.gatech.edu/academic/courses/core/math2601/Web-notes/3num.pdf