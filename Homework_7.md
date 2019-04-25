# Math 5610 Fundamentals of Computational Mathematics

## Homework #7

- [x] [Task 1](#task-1)
- [x] [Task 2](#task-2)
- [x] [Task 3](#task-3)
- [x] [Task 4](#task-4)
- [x] [Task 5](#task-5)
- [x] [Task 6](#task-6)
- [x] [Task 7](#task-7)
- [x] [Task 8](#task-8)
- [ ] [Task 9](#task-9)
- [ ] [Task 10](#task-10)

### Task 1
Compare the results for the Jacobi iteration and Gaussian elimination on matrices that are diagonally dominant. Tabulate the CPU time necessary to complete the iteration to a given number of digits of accuracy. Tabulate results for successfully larger matrices. Is there an intersection for the curves?
- For the system _A_ _x_ = _b_ -- where _A_ is a random, diagonally-dominant matrix and _b_ is generated from the known solution _x_ = [1.0] -- the solution is calculated within a tolerance of 10E-15.

|   n    | Jacobi Time |      Jacobi Error       | G-Elim Time |      G-Elim Error       |
| :----: | :---------: | :---------------------: | :---------: | :---------------------: |
|   10   |   0.00 s    | 3.3966293202022058E-015 |   0.00 s    | 8.0059320849734419E-016 |
|  100   |  8.00E-3 s  | 5.7076694188870971E-015 |  4.00E-3 s  | 8.8086245009917961E-015 |
| 1,000  |   0.78 s    | 5.6297065823097849E-014 |   3.28 s    | 8.7043042827550363E-014 |
| 10,000 |  114.15 s   | 5.9152457706392559E-013 |  4300.22 s  | 9.0672917653730400E-013 |

### Task 2
Repeat the previous task using the Gauss-Seidel algorithm.
- For the system _A_ _x_ = _b_ -- where _A_ is a random, diagonally-dominant matrix and _b_ is generated from the known solution _x_ = [1.0] -- the solution is calculated within a tolerance of 10E-15.

|   n    | G-S Time |        G-S Error        | G-Elim Time |      G-Elim Error       |
| :----: | :------: | :---------------------: | :---------: | :---------------------: |
|   10   |  0.00 s  | 4.4408920985006262E-016 |   0.00 s    | 8.0059320849734419E-016 |
|  100   |  0.00 s  | 6.2172489379008766E-015 |   4.00E-3   | 8.8086245009917961E-015 |
| 1,000  |  0.25 s  | 6.9083176078338208E-014 |   3.28 s    | 8.7043042827550363E-014 |
| 10,000 | 46.93 s  | 6.9295168415164453E-013 |  4300.22 s  | 9.0672917653730400E-013 |

### Task 3
Implement the steepest descent method for solving linear systems of equations _A_ _x_=_b_. Write up an entry in your software manual.
- Code: [solvesteepest.f90](solvesteepest.f90)
- Software Manual [entry](Software_Manual/solvesteepest.md).

### Task 4
Try out your steepest descent method on Hilbert matrices of size 4, 8, 16, 32. Explain your results.
- For the system _A_ _x_ = _b_ -- where _A_ is a Hilbert matrix and _b_ is generated from the known solution _x_ = [1.0] -- the solution is calculated with 500,000 iterations.

|  n   | Steepest-Descent Error  |
| :--: | :---------------------: |
|  4   | 2.0986699318578702E-010 |
|  8   | 1.4479002872956040E-003 |
|  16  | 1.7942141697499905E-003 |
|  32  | 2.4664697772506013E-003 |

>The steepest descent method has a hard time converging with the Hilbert matrices. Even after 500,000 iterations, the method did not converge within the tolerance (10E-15), except for n = 4, and the resulting error is at an unacceptable level for n = 8, 16, and 32. 

### Task 5
Implement the Conjugate Gradient method. Document the algorithm in an entry in your software manual.
- Code: [solveconjgrad.f90](solveconjgrad.f90)
- Software Manual [entry](Software_Manual/solveconjgrad.md).

### Task 6
Try out the conjugate gradient method from the previous task on Hilbert matrices of order 4, 8, 16, and 32. Describe the results you obtained.
- For the system _A_ _x_ = _b_ -- where _A_ is a Hilbert matrix and _b_ is generated from the known solution _x_ = [1.0] -- the solution is calculated within a tolerance of 10E-15.

|  n   | Iterations | Conjugate-Gradient Error |
| :--: | :--------: | :----------------------: |
|  4   |     6      | 5.0771533139137843E-013  |
|  8   |     13     | 3.1191834773973931E-005  |
|  16  |     21     | 3.4884110716025324E-005  |
|  32  |     30     | 3.3513510808611327E-005  |

>The conjugate gradient method did better than the steepest descent method with the Hilbert matrices. It converged within the tolerance in approximately _n_ iterations, though, the error for n = 8, 16, and 32 remains at an unacceptable level. 


### Task 7
Do an internet search on the use of iterative methods for the solution of linear systems of equations. Provide a list of at least three methods that do not use preconditioning of the system.
- > Jacobi Iteration:
  >
  > - https://en.wikipedia.org/wiki/Jacobi_method
- > Gauss-Seidel Method:
  >
  > - https://en.wikipedia.org/wiki/Gauss%E2%80%93Seidel_method
- > Gradient Descent:
  >
  > - https://en.wikipedia.org/wiki/Gradient_descent

### Task 8
Look for internet sites that include descriptions of preconditioning of systems of equations. Document at least 3 different preconditioning strategies.
- > A pre-conditioner is a matrix that is pre-multiplied (or post-multiplied) to a linear system to make it better-conditioned for the desired solution algorithm. Examples of pre-conditioners include: Jacobi, Incomplete Cholesky Factorization, Incomplete LU Factorization, Successive Over-Relaxation, and Multigrid Preconditioning.
  >
  > - http://www.math.iit.edu/~fass/477577_Chapter_16.pdf
  > - https://math-linux.com/mathematics/linear-systems/article/preconditioned-conjugate-gradient-method
  > - https://en.wikipedia.org/wiki/Preconditioner#Preconditioning_for_linear_systems
  > - https://en.wikipedia.org/wiki/Incomplete_Cholesky_factorization
  > - https://en.wikipedia.org/wiki/Incomplete_LU_factorization

### Task 9
Compare results on symmetric positive definite linear systems of equations using Jacobi versus the conjugate gradient methods. Discuss the results on systems with at least 500 equations and unknowns.
- For the system _A_ _x_ = _b_ -- where _A_ is a symmetric, diagonally-dominant matrix, and _b_ is generated from the known solution _x_ = [1.0] -- the solution is calculated within a tolerance of 10E-15.

|   n    | Jacobi Iterations | Conjugate-Gradient Iterations |
| :----: | :---------------: | :---------------------------: |
|   10   |        65         |              10               |
|  100   |        94         |              19               |
| 1,000  |        100        |              19               |
| 10,000 |        106        |              18               |

>The conjugate gradient method did better than the Jacobi in terms of the number of iterations required to converge. The comparison between the two methods in terms of the error, with respect to the true solution, is difficult to generalize because of its dependence of how diagonally dominant _A_ is and the difference in how the convergence criteria is calculated for the two methods.

### Task 10
Describe an algorithm for computing the solution of linear least squares systems using Jacobi iteration. Create an entry in your software manual for your algorithm and an example. You can use one of the examples in the current textbook.

- 