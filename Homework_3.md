# Math 5610 Fundamentals of Computational Mathematics

## Homework #3

- [x] [Task 1](#task-1)
- [x] [Task 2](#task-2)
- [x] [Task 3](#task-3)
- [x] [Task 4](#task-4)
- [x] [Task 5](#task-5)
- [x] [Task 6](#task-6)
- [x] [Task 7](#task-7)
- [x] [Task 8](#task-8)
- [x] [Task 9](#task-9)
- [ ] [Task 10](#task-10)

### Task 1
Implement a method that returns the absolute error in the approximation of one vector by another when the 2-norm is used. Make sure to add an entry to your software manual to document the work.
- Code: [norm2abserr.f90](norm2abserr.f90)
- Software Manual [entry](Software_Manual/norm2abserr.md).

### Task 2
Implement a method that returns the absolute error in the approximation of one vector by another when the 1-norm is used. Add an entry to your software manual that documents the method.
- Code: [norm1abserr.f90](norm1abserr.f90)
- Software Manual [entry](Software_Manual/norm1abserr.md).

### Task 3
Implement a method that returns the absolute error in the approximation of one vector by another when the ∞-norm is used. Add an entry to your software manual that documents the method.
- Code: [norminfabserr.f90](norminfabserr.f90)
- Software Manual [entry](Software_Manual/norminfabserr.md).

### Task 4
Implement a method that returns the 1-matrix norm of a given square matrix. Add an entry to your software manual that documents the method.
- Code: [norm1mat.f90](norm1mat.f90)
- Software Manual [entry](Software_Manual/norm1mat.md).

### Task 5
Implement a method that returns the ∞-norm of a given square matrix. Add an entry to your software manual that documents the method.
- Code: [norminfmat.f90](norminfmat.f90)
- Software Manual [entry](Software_Manual/norminfmat.md).

### Task 6
Implement a method that returns the dot-product of two vectors of the same length. Add an entry to your software manual that documents the method.
- Code: [dotvec.f90](dotvec.f90)
- Software Manual [entry](Software_Manual/dotvec.md).

### Task 7
Implement a method that returns the cross-product of two vectors of length three. Add an entry to your software manual that documents the method.
- Code: [crossvec.f90](crossvec.f90)
- Software Manual [entry](Software_Manual/crossvec.md).

### Task 8
Implement a method that returns the product of two matrices with an equal inner dimension. Add an entry to your software manual that documents the method.
- Code: [multmat.f90](multmat.f90)
- Software Manual [entry](Software_Manual/multmat.md).

### Task 9
Write a routine that will generate a diagonally dominant matrix that has real values in all entries of the matrix. Add an entry to your for the method you create.
- Code: [randdommat.f90](randdommat.f90)
- Software Manual [entry](Software_Manual/randdommat.md).

### Task 10
Search the internet for sites that define and discuss the Frobenius matrix norm. Also, look for sites that define consistent matrix norms. Summarize in a paragraph or two what you found and as usual cite the sites.

- > A matrix norm is considered consistent if the norm of the matrix product, _AB_, is less than or equal to of the norm of matrix _A_ multiplied by the norm of matrix _B_. This, in effect, is satisfied only by a norm that is calculated using the same formula, regardless of the shape of the matrix. Induced norms, by definition, are examples of consistent matrix norms.
  >
  > The Frobenius matrix norm is another example of a consistent matrix norm. It is defined as the square root of the sum of every element of the the matrix squared. While consistent and simple to implement, the Frobenius matrix norm is disadvantaged by the fact is "measures" the size of the matrix. As the number of elements of the matrix, whose Frobenius norm is being calculated, increases, the norm will naturally tend to increase. Thus, the norm of small matrices will inherently be smaller than the norm of a large matrix, due to the number of elements being summed. So, the Frobenius norm may be an effective means to compare matrices of similar size, but would be inadequate in the comparison of matrices of largely different sizes.
  >
  > - https://www.uio.no/studier/emner/matnat/ifi/nedlagte-emner/INF-MAT3350/h07/undervisningsmateriale/chap13slides.pdf
  > - https://ipfs.io/ipfs/QmXoypizjW3WknFiJnKLwHCnL72vedxjQkDDP1mXWo6uco/wiki/Matrix_norm.html
  > - https://www.sciencedirect.com/topics/engineering/frobenius-norm
  > - http://mathworld.wolfram.com/FrobeniusNorm.html