# MATH 5610 Software Manual

### Subroutine: [_lsjacobi_](../lsjacobi.f90)

**Author:** Jackson Reid

**Language:** Fortran. The code can be [compiled](compilation.md) using the GNU Fortran compiler (gfortran).

**Description:** This routine will compute an approximate solution of a least squares problem, using the normal equations with a Jacobi iterative solver, as follows:
- Create the normal equations
  <a href="https://www.codecogs.com/eqnedit.php?latex=\left(A^T&space;A&space;\right&space;)\mathbf{\vv{x}}&space;=&space;A^T&space;\mathbf{\vv{b}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\left(A^T&space;A&space;\right&space;)\mathbf{\vv{x}}&space;=&space;A^T&space;\mathbf{\vv{b}}" title="\left(A^T A \right )\mathbf{\vv{x}} = A^T \mathbf{\vv{b}}" /></a>

- Condition the system to be diagonally dominant (as required by Jacobi Iteration)
  <a href="https://www.codecogs.com/eqnedit.php?latex=\left(A^TA&space;&plus;&space;\alpha{I}&space;\right&space;)\mathbf{\vv{x}}&space;=&space;A^T\mathbf{\vv{b}}&space;&plus;&space;\alpha{I}\mathbf{\vv{x}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\left(A^TA&space;&plus;&space;\alpha{I}&space;\right&space;)\mathbf{\vv{x}}&space;=&space;A^T\mathbf{\vv{b}}&space;&plus;&space;\alpha{I}\mathbf{\vv{x}}" title="\left(A^TA + \alpha{I} \right )\mathbf{\vv{x}} = A^T\mathbf{\vv{b}} + \alpha{I}\mathbf{\vv{x}}" /></a>

- Split the diagonal from the LHS matrix 
  <a href="https://www.codecogs.com/eqnedit.php?latex=\left(D_{A^TA}&space;&plus;&space;\alpha{I}&space;\right&space;)\mathbf{\vv{x}}_{k&plus;1}&space;=&space;A^T\mathbf{\vv{b}}&space;&plus;&space;\alpha{I}\mathbf{\vv{x}}_k&space;-&space;\left(L&space;&plus;&space;U&space;\right&space;)_{A^TA}\mathbf{\vv{x}}_k" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\left(D_{A^TA}&space;&plus;&space;\alpha{I}&space;\right&space;)\mathbf{\vv{x}}_{k&plus;1}&space;=&space;A^T\mathbf{\vv{b}}&space;&plus;&space;\alpha{I}\mathbf{\vv{x}}_k&space;-&space;\left(L&space;&plus;&space;U&space;\right&space;)_{A^TA}\mathbf{\vv{x}}_k" title="\left(D_{A^TA} + \alpha{I} \right )\mathbf{\vv{x}}_{k+1} = A^T\mathbf{\vv{b}} + \alpha{I}\mathbf{\vv{x}}_k - \left(L + U \right )_{A^TA}\mathbf{\vv{x}}_k" /></a>

- Invert the diagonal matrix
<a href="https://www.codecogs.com/eqnedit.php?latex=\mathbf{\vv{x}}_{k&plus;1}&space;=&space;\left[A^T\mathbf{\vv{b}}&space;&plus;&space;\alpha{I}\mathbf{\vv{x}}_k&space;-&space;\left(L&space;&plus;&space;U&space;\right&space;)_{A^TA}\mathbf{\vv{x}}_k\right]/\left(D_{A^TA}&space;&plus;&space;\alpha{I}&space;\right&space;)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mathbf{\vv{x}}_{k&plus;1}&space;=&space;\left[A^T\mathbf{\vv{b}}&space;&plus;&space;\alpha{I}\mathbf{\vv{x}}_k&space;-&space;\left(L&space;&plus;&space;U&space;\right&space;)_{A^TA}\mathbf{\vv{x}}_k\right]/\left(D_{A^TA}&space;&plus;&space;\alpha{I}&space;\right&space;)" title="\mathbf{\vv{x}}_{k+1} = \left[A^T\mathbf{\vv{b}} + \alpha{I}\mathbf{\vv{x}}_k - \left(L + U \right )_{A^TA}\mathbf{\vv{x}}_k\right]/\left(D_{A^TA} + \alpha{I} \right )" /></a>

**Inputs:** 

​        _A_ : REAL*8 -- an array of size (_r_,_c_), containing the left hand side of the system

​	_r_ : INTEGER -- the number of data points

​	_c_ : INTEGER -- the number of degrees in the fit

​        _b_ : REAL*8 -- an array of size (_r_), containing the right hand side of the system

​        _x0_ : REAL*8 -- an array of size (_c_) containing the initial guess of the solution

​        _tol_ : REAL*8 -- the error tolerance for the iterations, norm2(x(k+1) - x(k))

​        _maxiter_ : INTEGER -- the maximum number of iterations

​        _text_ : LOGICAL -- flag to toggle writing of error/iteration count to the screen

**Outputs:** 

​        _x_ : REAL*8 -- an array of size (_c_), containing the fit coefficients

**Example Usage:** 

```
    A = RESHAPE((/1.0d0, 0.0d0, 1.0d0, &
                & 2.0d0, 3.0d0, 5.0d0, &
                & 5.0d0, 3.0d0, -2.0d0, &
                & 3.0d0, 5.0d0, 4.0d0, &
               & -1.0d0, 6.0d0, 3.0d0/),(/5,3/),ORDER=(/2,1/))
    b = (/ 4.0d0, -2.0d0, 5.0d0, -2.0d0, 1.0d0 /)
    x0 = (/ 0.0d0, 0.0d0, 0.0d0 /)
    
    CALL lsjacobi(A,5,3,b,x0,10.d-15,1000,x)
    WRITE(*,*) x
    WRITE(*,*)
```
Output from the lines above:
```
           1  0.19688925702464727     
           2  0.14603072174580925     
           3  0.10928819320298105     
           4   8.2794897546026522E-002
           5   6.3809322043600139E-002
           6   5.0315543130529501E-002
           7   4.0796993359818130E-002
           8   3.4098406693678925E-002
           9   2.9344876772903025E-002
          10   2.5892960873073013E-002
...
         410   1.5524177531609529E-014
         411   1.4446557141323809E-014
         412   1.3571199318915374E-014
         413   1.2714962180503807E-014
         414   1.1783802792238344E-014
         415   1.0932455813622359E-014
         416   1.0253947971971527E-014
         417   9.4631453641027804E-015
0.34722617354201935       0.39900426742523354      -0.78591749644373210  
```
**Implementation:**

```
SUBROUTINE lsjacobi(A,r,c,b,x0,tol,maxiter,text,x)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: r, c, maxiter
    REAL*8, INTENT(in) :: A(r,c), b(r), x0(c), tol
    LOGICAL, INTENT(in) :: text
    REAL*8, INTENT(out) :: x(c)
    REAL*8 :: Asq(c,c), A_LU(c,c), D(c), ATb(r), alpha(c), xold(c), y(c), error
    INTEGER :: i, iter

    !Multiply the system by A^T
    CALL multmat(TRANSPOSE(A),A,c,r,c,Asq)
    CALL multmat(TRANSPOSE(A),b,c,r,1,ATb)

    !Generate conditioning array
    alpha = 0.0d0
    DO i = 1,c
        alpha = alpha + Asq(:,i)
    END DO
    alpha = alpha

    !Split the coefficient matrix (A = [L - aI + U] + [D + aI])
    A_LU = Asq
    DO i = 1,c
        D(i) = 1.0d0/(Asq(i,i) + alpha(i))
        A_LU(i,i) = -alpha(i)
    END DO

    error = 10.0d0*tol
    iter = 0
    xold = x0

    !Perform Jacobi Iteration
    DO WHILE (error > tol .AND. iter < maxiter)

        CALL multmat(A_LU,xold,c,c,1,y)

        x = D*(ATb - y)

        iter = iter + 1

        CALL norm2abserr(x,xold,c,error)

        xold = x

        IF (text) THEN
            WRITE(*,*) iter, error
        END IF

    END DO


END SUBROUTINE
```



**Last Modified:** April/2019

