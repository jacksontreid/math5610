# Math 5610 Fundamentals of Computational Mathematics

## Homework #4

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
Implement a method that returns the scalar multiple of a given matrix. Add an entry to your software manual documenting the method.
- Code: [scalemat.f90](scalemat.f90)
- Software Manual [entry](Software_Manual/scalemat.md).

### Task 2
Implement a method that returns the sum of two matrices of the same size. Add an entry to your software manual that documents the method.
- Code: [addmat.f90](addmat.f90)
- Software Manual [entry](Software_Manual/addmat.md).

### Task 3
Implement a method that will return the outer product of two vectors. Add an entry to your software manual documenting the method.
- Code: [outervec.f90](outervec.f90)
- Software Manual [entry](Software_Manual/outervec.md).

### Task 4
Implement a method that will compute the solution of a square linear system of equations where the coefficient matrix is a diagonal matrix.
- Code: [solvediagsys.f90](solvediagsys.f90)
- Software Manual [entry](Software_Manual/solvediagsys.md).

### Task 5
Implement a method that will compute the solution of a square linear system of equations where the coefficient matrix is upper triangular. Document this backsubstitution method in a software manual entry.
- Code: [backsub.f90](backsub.f90)
- Software Manual [entry](Software_Manual/backsub.md).

### Task 6
Implement a method that will compute the solution of a square linear system of equations where the coefficient matrix is lower triangular. Document this forwardsubstitution method in a software manual entry.
- Code: [forsub.f90](forsub.f90)
- Software Manual [entry](Software_Manual/forsub.md).

### Task 7
Implement a method that will perform elementary row operations on a matrix to take the matrix to row echelon form. The resulting matrix should be upper triangular through the rows. If the matrix is not a square matrix, define an appropriate output for the method tha will return the row echelon form. Add an entry to your software manual documenting the method.
- Code: [rowechelon.f90](rowechelon.f90)
- Software Manual [entry](Software_Manual/rowechelon.md).

### Task 8
Using previous methods you have created, write a code that will solve a square linear system of equations using Gaussian elimination (elementary row operations) to reduce the augmented coefficient matrix to row echelon form and then apply backsubstitution to determine an approximate solution for the linear system.
- Code: [solvegausselim.f90](solvegausselim.f90)
- Software Manual [entry](Software_Manual/solvegausselim.md).

### Task 9
Write a routine that will generate a symmetric diagonally dominant matrix that has real values in all entries of the matrix. Add an entry to your for the method you create.
- Code: [randsymdommat.f90](randsymdommat.f90)
- Software Manual [entry](Software_Manual/randsymdommat.md).

### Task 10
Search the internet for sites that discuss parallel algorithms for matrix-vector multiplication and matrix-matrix multiplication. Summarize in a paragraph or two and as always, cite your sites.

- > There are three main decompositions used to parallelize matrix-vector multiplication. First, row-decomposition, where each row is dotted by the vector on a separate processor. Second, column-decomposition, where each column is multiplied by the corresponding vector entry on a separate processor, and then summed together at the end.  Finally, checkerboard-decomposition, where sub-blocks of the matrix and vector are multiplied together on separate processors.
  >
  > The advantages and disadvantages of these three decompositions depend on the size of the matrix-vector system and the number of processors being utilized. Row-decomposition, for example, requires that the vector be copied to each processor to be dotted with each row. For a large system, being distributed across many processors, this copying could be unacceptably  expensive.
  >
  > - http://www.hpcc.unn.ru/mskurs/ENG/DOC/pp07.pdf
- > A common method of parallelizing a matrix-matrix multiplication (_A_._B_=_C_) is a divide-and-conquer algorithm. In this algorithm, matrices _A_ and _B_ are divided into sub-blocks. Each pair of corresponding sub-blocks are sent to a processor and multiplied. The results of these sub-block multiplications are saved as output sub-blocks of matrix _C_. Then, the sub-blocks of _A_ are shifted "to the left" and the sub-blocks of _B_ are shifted "up", the new pairs of sub-blocks are sent to processors to be multiplied, and the resulting sub-blocks are added to _C_. This process is repeated until all the sub-blocks of _A_ and _B_ return to their original positions.
  >
  > - https://cse.buffalo.edu/faculty/miller/Courses/CSE633/Ortega-Fall-2012-CSE633.pdf