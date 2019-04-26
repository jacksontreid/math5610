# Math 5610 Fundamentals of Computational Mathematics

## Homework #8

- [x] [Task 1](#task-1)
- [x] [Task 2](#task-2)
- [x] [Task 3](#task-3)
- [x] [Task 4](#task-4)
- [x] [Task 5](#task-5)
- [ ] [Task 6](#task-6)
- [ ] [Task 7](#task-7)
- [ ] [Task 8](#task-8)
- [ ] [Task 9](#task-9)
- [ ] [Task 10](#task-10)

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
- 


### Task 7
Use the Rayleigh Quotient algorthm to compute an approximation for the condition of the matrix. Test your code on a Hilbert matrix of reasonable size. Discuss the results from the work.
- 


### Task 8
Look for internet sites that estimate the condition number of a matrix. Document the sites and as usual cite the web pages used in your explanation of the information found.
- > 
  >
  > - 


### Task 9
Compare the Inverse Iteration Algorithm and the Rayleigh Quotient Algorithm in terms of the amount of time needed to compute an eigenvalue. Tabulate your results for the two methods.
- 

### Task 10
Implement a version of the Inverse Iteration Algorithm that uses Jacobi Iteration to compute solutions of the linear systems of equations. Test this algorithm using a random diagonally dominant matrix. Document the algorithm in your software manual.

- 