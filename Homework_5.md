# Math 5610 Fundamentals of Computational Mathematics

## Homework #5

- [x] [Task 1](#task-1)
- [x] [Task 2](#task-2)
- [x] [Task 3](#task-3)
- [x] [Task 4](#task-4)
- [x] [Task 5](#task-5)
- [x] [Task 6](#task-6)
- [x] [Task 7](#task-7)
- [ ] [Task 8](#task-8)
- [ ] [Task 9](#task-9)
- [ ] [Task 10](#task-10)

### Task 1
Implement a method that will return the approximate solution of a square linear system of equations where previous methods are not used. That is, inline the row reduction operations and the backsubstitution methods. Test the speed of the code you generated in this problem and the code that references your previous methods. Try this for increasing sizes of the linear system. You will likely need to use large systems of linear equations - possibly 10,000 by 10,000 to see any kind of time. Use cpu_timing methods in the language you have chosen to do your coding. Add a manual page to document the inline version of the solution process. Report any differences you see in the time it takes to solve the linear systems in the two approaches.
- Code: [solvegausselim2.f90](solvegausselim2.f90)

- Software Manual [entry](Software_Manual/solvegausselim2.md).



|   n    |     External (sec)      |      In-Line (sec)      |
| :----: | :---------------------: | :---------------------: |
|   10   |   0.0000000000000000    |   0.0000000000000000    |
|  100   | 4.0000000000000001E-003 | 4.0000000000000001E-003 |
| 1,000  |   1.4199999999999999    |   1.3839999999999999    |
| 10,000 |   6597.9240000000000    |   2526.2799999999997    |

  

### Task 2
Implement a method that returns the LU-factorization of a square matrix. Add an entry to your software manual to document the method you have created. Hint: You can actually modify the Gaussian elimination code in two lines to come up with the new method.
- Code: [LUdecomp.f90](LUdecomp.f90)
- Software Manual [entry](Software_Manual/LUdecomp.md).

### Task 3
Use the LU factorization method you created in the previous step, along with the forward substitution method (for lower triangular square systems) and the back substitution method (for upper triangular square systems) to create a method that will solve a square linear system of equations. Document the method in your software manual.
- Code: [solveLUfactor.f90](solveLUfactor.f90)
- Software Manual [entry](Software_Manual/solveLUfactor.md).

### Task 4
Write a code that will generate a symmetric, positive definite matrix for a given integer, n. Make sure that you add an entry to your software manual with a couple of examples. Your examples should be relatively small for your examples, but you should include a large example in the task solution write-up.
- Code: [randspdmat.f90](randspdmat.f90)
- Software Manual [entry](Software_Manual/randspdmat.md).
- [large_spd_matrix.dat](large_spd_matrix.dat)

### Task 5
Implement the Cholesky factorization method for square matrices. Do not include any pivoting in the algorithm. Document the algorithm in your software manual. Test the code on at least 2 or 3 matrices of different sizes. At least one example should involve a matrix that is bigger than 100×100 in size. Use output from the method you created in the previous task.
- Code: [choldecomp.f90](choldecomp.f90)
- Software Manual [entry](Software_Manual/choldecomp.md).
- [large_cholesky_decomp.dat](large_cholesky_decomp.dat)

### Task 6
Write a routine/method that will return an approximate solution of the least squares problem using the normal equation approach. Create an entry in your software manual for the method. Also, make sure you use the Cholesky algorithm that you created in a previous task.
- Code: [lsnormal.f90](lsnormal.f90)
- Software Manual [entry](Software_Manual/lsnormal.md).

### Task 7
Implement the QR factorization of a square matrix. Use the Gram-Schmidt process to create the orthogonal vectors for the orthogonal matrix. Document the method in your software manual. Include examples showing the orthogonal matrix and the other factor which should be upper triangular.
- Code: [QRdecomp.f90](QRdecomp.f90)
- Software Manual [entry](Software_Manual/QRdecomp.md).

### Task 8
Try out your QR-factorization method from the previous task on the Hilbert matrices of sizes n=4,6,8,10. Test to see if the orthogonal matrix is really orthogonal by multiplying _Q_ by its transpose and comparing the result to the identity matrix. Explain the results you obtain.
- The results of the multiplication _Q_^T*_Q_ are shown below. The results here are shown in scientific notation to emphasis the order of magnitude of each entry in the matrix, and displayed with few digits in order to fit on the page. The diagonals of each matrix are the same as the identity matrix (= 1.0) in every case, but the off-diagonal values increase in error as the size of the Hilbert matrix increases. In the case that n = 4, the result is very close to the identity matrix, signifying that _Q_ is very close to orthonormal.  However, the error increases to the point that, for n = 10, off-diagonal values exist at the same magnitude as the diagonal, clearly proving that _Q_ is not close to being orthonormal.
- n = 4
```
  0.1E+01  0.5E-15 -0.5E-14  0.2E-12
  0.5E-15  0.1E+01 -0.4E-13  0.1E-11
 -0.5E-14 -0.4E-13  0.1E+01  0.4E-10
  0.2E-12  0.1E-11  0.4E-10  0.1E+01
```

- n = 6
```
  0.1E+01 -0.2E-15  0.5E-14 -0.2E-12  0.4E-11  0.2E-09
 -0.2E-15  0.1E+01  0.4E-14 -0.9E-13  0.3E-11 -0.2E-09
  0.5E-14  0.4E-14  0.1E+01 -0.6E-11  0.2E-09 -0.6E-08
 -0.2E-12 -0.9E-13 -0.6E-11  0.1E+01  0.1E-07 -0.7E-06
  0.4E-11  0.3E-11  0.2E-09  0.1E-07  0.1E+01 -0.5E-04
  0.2E-09 -0.2E-09 -0.6E-08 -0.7E-06 -0.5E-04  0.1E+01
```

- n = 8
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

- n = 10
```
  0.1E+01  0.1E-14 -0.2E-13  0.2E-12 -0.3E-11  0.5E-10 -0.3E-09 -0.3E-09 -0.4E-09 -0.4E-09
  0.1E-14  0.1E+01 -0.5E-13  0.6E-12 -0.6E-11  0.6E-10 -0.7E-09 -0.7E-09 -0.7E-09 -0.6E-09
 -0.2E-13 -0.5E-13  0.1E+01  0.2E-10 -0.4E-09  0.6E-08 -0.8E-07 -0.8E-07 -0.7E-07 -0.7E-07
  0.2E-12  0.6E-12  0.2E-10  0.1E+01 -0.1E-07  0.4E-06 -0.9E-05 -0.8E-05 -0.7E-05 -0.7E-05
 -0.3E-11 -0.6E-11 -0.4E-09 -0.1E-07  0.1E+01  0.2E-04 -0.8E-03 -0.7E-03 -0.7E-03 -0.6E-03
  0.5E-10  0.6E-10  0.6E-08  0.4E-06  0.2E-04  0.1E+01 -0.5E-01 -0.4E-01 -0.4E-01 -0.3E-01
 -0.3E-09 -0.7E-09 -0.8E-07 -0.9E-05 -0.8E-03 -0.5E-01  0.1E+01  0.1E+01  0.1E+01  0.1E+01
 -0.3E-09 -0.7E-09 -0.8E-07 -0.8E-05 -0.7E-03 -0.4E-01  0.1E+01  0.1E+01  0.1E+01  0.1E+01
 -0.4E-09 -0.7E-09 -0.7E-07 -0.7E-05 -0.7E-03 -0.4E-01  0.1E+01  0.1E+01  0.1E+01  0.1E+01
 -0.4E-09 -0.6E-09 -0.7E-07 -0.7E-05 -0.6E-03 -0.3E-01  0.1E+01  0.1E+01  0.1E+01  0.1E+01
```


### Task 9
Implement a method that will return a square diagonally dominant matrix. Document this method in your software manual.
- 

### Task 10
Search the internet for sites that discuss the use of direct methods for the approximate solution of linear systems of systems of equations. Note that direct methods include factorization methods and the standard Gaussian eliimination with back substitution. Find at least a couple of sites where limitations of direct methods are discussed. As usual, cite your sites.

- > 
  >
  > - 