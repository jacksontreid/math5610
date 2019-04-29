# Math 5610 Fundamentals of Computational Mathematics

## Homework #8

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
Implement a code that will approximate the largest eigenvalue of a matrix using the Power Iteration algorithm. Test the code in the Hilbert matrix. Include an entry in your software manual for the algorithm.
- Code: [eigpower.f90](eigpower.f90)
- Software Manual [entry](Software_Manual/eigpower.md).

  |  n   | Hilbert Matrix Largest Eigenvalue |
  | :--: | :-------------------------------: |
  |  4   |        1.5002142800592426         |
  |  6   |        1.6188998589243391         |
  |  8   |        1.6959389969219498         |
  |  10  |        1.7519196702651774         |

### Task 2
Implement the Inverse Iteration method with shifting to find approximate eigenvalues of a matrix A. Include an entry in your software manual documenting the algorithm. Find an eigenvalue less than the largest for the Hilbert matrix.
- Code: [eiginvpower.f90](eiginvpower.f90)
- Software Manual [entry](Software_Manual/eiginvpower.md).

  |  n   | Hilbert Matrix Smallest Eigenvalue |
  | :--: | :--------------------------------: |
  |  4   |      9.6702304022602893E-005       |
  |  6   |      1.0827994843911298E-007       |
  |  8   |      1.1115389699614863E-010       |
  |  10  |      1.0932581386257379E-013       |

### Task 3
Write a code that will compute an approximation of the condition in the 2-norm using your code to produce a largest and smallest eigenvalue of a matrix. Document your algorithm in your software manual. Try the code on a Hilbert matrix (in next task).
- Code: [matcond.f90](matcond.f90)
- Software Manual [entry](Software_Manual/matcond.md).

### Task 4
Test the code developed above to compute the condition number of the Hilbert matrix as the size of the matrix is increased. Tabulate and/or graph the results from your work.

|  n   | Hilbert Matrix Condition Number |
| :--: | :-----------------------------: |
|  4   |       15513.738738930018        |
|  6   |       14951058.642539570        |
|  8   |       15257575692.381819        |
|  10  |       16024757633795.428        |

### Task 5
Write a code that will compute approximations of the smallest and largest eigenvalues. Next subdivide the interval containing the smallest and largest eigenvalues and use the points in the subdivision to shift and locate other eigenvalues. Discuss your results.
- Code: [eigsearch.f90](eigsearch.f90)

- Software Manual [entry](Software_Manual/eigsearch.md).

- > This method seems to do a good job finding the eigenvalues if they are spread out within the interval. However, if the eigenvalues are clustered to one end of the interval (such as with Hilbert matrices), this search method struggles. Though, even in the case of extreme clustering, this routine gives valuable information about where to refine the search.

### Task 6
Implement the Raleigh Quotient algorithm for computing approximations for eigenvalues of a matrix. Test the code on a Hilbert matrix and document the code in a software entry manual.
- Code: [eigrayleigh.f90](eigrayleigh.f90)

- Software Manual [entry](Software_Manual/eigrayleigh.md).

- > For a Hilbert matrix of _n_ = 4, with an initial guess of v0 = [1.0, 1.0, 1.0, 1.0], Rayleigh Quotient Iteration locates the eigenvalue 
  >
  > l = 1.5002142800592428
  >
  > with eigenvector 
  >
  > v = [-1.000000,      -0.570172,     -0.406779,      -0.318141 ]


### Task 7
Use the Rayleigh Quotient algorthm to compute an approximation for the condition of the matrix. Test your code on a Hilbert matrix of reasonable size. Discuss the results from the work.
- Code: [matcond2.f90](matcond2.f90)

- Software Manual [entry](Software_Manual/matcond2.md).

- > For a Hilbert matrix of _n_ = 4, with 100 guesses, Rayleigh Quotient Iteration the highest eigenvalue found is
  >
  > "l1" = 1.5002142800592431
  >
  > and the lowest eigenvalue found is
  >
  > "ln" = 6.7382736057607206E-003
  >
  > Thus, the predicted condition number is
  >
  > cond(_A_) = 222.64074862983776
  >
  > This condition number is much lower than the one predicted using the power and inverse power methods. This is due to the fact that Rayleigh Quotient Iteration is unable to locate the true lowest eigenvalue from the random guess vectors.


### Task 8
Look for internet sites that estimate the condition number of a matrix. Document the sites and as usual cite the web pages used in your explanation of the information found.
- > The condition number of a matrix is very important in the estimation of the error incurred in the solution of a linear system. If the condition number is small ("close" to 1), very little error is expected to incur. But, if the condition number is very large, the error incurred in the solution process is also likely to be large. 
  >
  > A high computational cost is required to calculate the condition number, because it requires the inverse of the matrix whose condition number is sought. There are methods that try to decease this computational cost, by estimating the matrix inverse or using byproducts of the solution process.
  >
  > - http://www.cse.iitd.ernet.in/~dheerajb/CS210_lect07.pdf
  > - https://www.jstor.org/stable/2156842?seq=5#metadata_info_tab_contents
  > - https://www.mathworks.com/help/symbolic/cond.html
  > - https://docs.scipy.org/doc/numpy-1.14.5/reference/generated/numpy.linalg.cond.html


### Task 9
Compare the Inverse Iteration Algorithm and the Rayleigh Quotient Algorithm in terms of the amount of time needed to compute an eigenvalue. Tabulate your results for the two methods.
- Using randomly generated, symmetric, diagonally-dominant matrices of various sizes, and eigenvalue is found using an initial vector of [1.0] and shift of _alpha_ = 0. (The times presented in this table are dependent on the random matrix.)

|  n   | Inverse Iteration (sec) | Rayleigh Quotient (sec) |
| :--: | :---------------------: | :---------------------: |
|  10  |   0.0000000000000000    |   0.0000000000000000    |
|  20  |   0.0000000000000000    |   0.0000000000000000    |
|  30  |   0.0000000000000000    | 4.0000000000000001E-003 |
|  40  | 4.0000000000000001E-003 |   0.0000000000000000    |
|  50  |   0.0000000000000000    | 4.0000000000000001E-003 |
|  60  | 2.4000000000000000E-002 | 4.0000000000000036E-003 |
|  70  | 2.0000000000000018E-002 | 4.0000000000000036E-003 |
|  80  | 1.2000000000000011E-002 | 4.0000000000000036E-003 |
|  90  | 1.2000000000000011E-002 | 8.0000000000000071E-003 |
| 100  | 1.2000000000000011E-002 | 8.0000000000000071E-003 |

### Task 10
Implement a version of the Inverse Iteration Algorithm that uses Jacobi Iteration to compute solutions of the linear systems of equations. Test this algorithm using a random diagonally dominant matrix. Document the algorithm in your software manual.

- Code: [eiginvpower_jacobi.f90](eiginvpower_jacobi.f90)
- Software Manual [entry](Software_Manual/eiginvpower_jacobi.md).