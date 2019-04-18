PROGRAM test
IMPLICIT NONE

REAL :: snum
REAL*8 :: num1, num2, num3, num4
REAL*8 :: mat1(4,3), mat2(3,3), mat3(4,3), mat4(4,3), mat5(3,4), mat6(5,3)
REAL*8 :: mat7(3,3), mat8(3,3), mat9(3,2)
REAL*8, ALLOCATABLE :: matall1(:,:), matall2(:,:), matall3(:,:)
REAL*8, ALLOCATABLE :: vecall1(:), vecall2(:), vecall3(:)
REAL*8 :: vec1(4), vec2(4), vec3(4), vec4(3), vec5(3), vec6(3), vec7(5), vec8(2)
INTEGER :: i, j, k, n
CHARACTER :: wrt_fmt(20)

!Seed random number generator
CALL randseed()

!Test machine precision
WRITE(*,*)
WRITE(*,*) ">>>TEST: MACHINE PRECISION"
    CALL smaceps(snum,i)
    WRITE(*,*) i, snum

    CALL dmaceps(num1,i)
    WRITE(*,*) i, num1

!Test random matrix generator
WRITE(*,*)
WRITE(*,*) ">>>TEST: RANDOM MATRIX"
    CALL randmat(4,3,mat1)
    DO i = 1,4
        WRITE(*,*) mat1(i,:)
    END DO

!Test random vector generator
WRITE(*,*)
WRITE(*,*) ">>>TEST: RANDOM VECTOR"
    CALL randvec(4,vec1)
    WRITE(*,*) vec1

!Test error calculators
WRITE(*,*)
WRITE(*,*) ">>>TEST: ERROR CALCS"
    num1 = 25.0001d0
    num2 = 25.0d0
    CALL abserr(num1,num2,num3)
    WRITE(*,*) num3

    CALL relerr(num1,num2,num3)
    WRITE(*,*) num3

!Test vector creation, addition, and scaling
WRITE(*,*)
WRITE(*,*) ">>>TEST: VECTOR ADDITION AND SCALING"
    vec1 = (/ 0.5d0, 0.6d0, 1.9d0, 2.0d0 /)
    vec2 = (/ 0.2d0, 0.4d0, 0.1d0, 1.0d0 /)
    WRITE(*,*) vec1
    WRITE(*,*) vec2

    CALL addvec(vec1,vec2,4,vec3)
    WRITE(*,*) vec3

    CALL scalevec(vec1, 4, DBLE(2.))
    WRITE(*,*) vec1

!Test vector norms
WRITE(*,*)
WRITE(*,*) ">>>TEST: VECTOR NORMS"
    vec1 = (/ 0.5d0, 0.6d0, 1.9d0, 2.0d0 /)
    WRITE(*,*) vec1

    CALL norm2vec(vec1,4,num1)
    WRITE(*,*) num1

    CALL norm1vec(vec1,4,num1)
    WRITE(*,*) num1

    CALL norminfvec(vec1,4,num1)
    WRITE(*,*) num1

!Test symmetric matrix generator
WRITE(*,*)
WRITE(*,*) ">>>TEST: SYMMETRIC MATRIX"
    CALL randsymmat(3,mat2)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO

!Test vector error
WRITE(*,*)
WRITE(*,*) ">>>TEST: VECTOR ERRORS"
    vec1 = (/ 0.5d0, 0.6d0, 1.9d0, 2.0d0 /)
    vec2 = (/ 0.2d0, 0.4d0, 0.1d0, 1.0d0 /)
    WRITE(*,*) vec1
    WRITE(*,*) vec2

    CALL norm2abserr(vec1, vec2, 4, num1)
    WRITE(*,*) num1

    CALL norm1abserr(vec1, vec2, 4, num1)
    WRITE(*,*) num1

    CALL norminfabserr(vec1, vec2, 4, num1)
    WRITE(*,*) num1

!Test matrix norms
WRITE(*,*)
WRITE(*,*) ">>>TEST: MATRIX NORMS"
    mat2 = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                   & 0.2d0, 0.4d0, 0.1d0, &
                   & 0.4d0, 0.1d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO

    CALL norm1mat(mat2, 3, num1)
    WRITE(*,*) num1

    CALL norminfmat(mat2, 3, num1)
    WRITE(*,*) num1

!Test vector dot and cross products
WRITE(*,*)
WRITE(*,*) ">>>TEST: VECTOR DOT AND CROSS PRODUCT"
    vec4 = (/ 0.5d0, 0.6d0, 1.9d0 /)
    vec5 = (/ 0.2d0, 0.4d0, 0.1d0 /)
    WRITE(*,*) vec4
    WRITE(*,*) vec5

    CALL dotvec(vec4,vec5,3,num1)
    WRITE(*,*) num1

    CALL crossvec(vec4,vec5,vec6)
    WRITE(*,*) vec6

!Test matrix multiplication
WRITE(*,*)
WRITE(*,*) ">>>TEST: MATRIX MULTIPLICATION"
    mat1 = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                   & 0.2d0, 0.4d0, 0.1d0, &
                   & 0.4d0, 0.1d0, 1.0d0, &
                   & 2.0d0, 0.7d0, 0.2d0/),(/4,3/),ORDER=(/2,1/))
    mat2 = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                   & 0.2d0, 0.4d0, 0.1d0, &
                   & 0.4d0, 0.1d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    DO i = 1,4
        WRITE(*,*) mat1(i,:)
    END DO
    WRITE(*,*)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)

    CALL multmat(mat1,mat2,4,3,3,mat3)
    DO i = 1,4
        WRITE(*,*) mat3(i,:)
    END DO

!Test random diagonally dominant matrix generator
WRITE(*,*)
WRITE(*,*) ">>>TEST: RANDOM D-DOM MATRIX"
    CALL randdommat(3,mat2)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)

    CALL randsymdommat(3,mat2)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO

!Test matrix addition and scaling
WRITE(*,*)
WRITE(*,*) ">>>TEST: VECTOR ADDITION AND SCALING"
    mat1 = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                   & 0.2d0, 0.4d0, 0.1d0, &
                   & 0.4d0, 0.1d0, 1.0d0, &
                   & 2.0d0, 0.7d0, 0.2d0/),(/4,3/),ORDER=(/2,1/))
    mat3 = RESHAPE((/0.6d0, 0.4d0, -0.9d0, &
                   & 0.8d0, 0.6d0, 0.9d0, &
                   & 0.6d0, 0.9d0, 0.0d0, &
                   & -1.0d0, 0.3d0, 0.8d0/),(/4,3/),ORDER=(/2,1/))
    DO i = 1,4
        WRITE(*,*) mat1(i,:)
    END DO
    WRITE(*,*)
    DO i = 1,4
        WRITE(*,*) mat3(i,:)
    END DO
    WRITE(*,*)

    CALL addmat(mat1, mat3, 4, 3, mat4)
    DO i = 1,4
        WRITE(*,*) mat4(i,:)
    END DO
    WRITE(*,*)

    CALL scalemat(mat1, 4, 3, 2.0d0)
    DO i = 1,4
        WRITE(*,*) mat1(i,:)
    END DO

!Test vector outer product
WRITE(*,*)
WRITE(*,*) ">>>TEST: VECTOR OUTER PRODUCT"
    vec1 = (/ 0.5d0, 0.6d0, 1.9d0, 2.0d0 /)
    vec5 = (/ 0.2d0, 0.4d0, 0.1d0 /)
    WRITE(*,*) vec1
    WRITE(*,*) vec5
    WRITE(*,*)

    CALL outervec(vec1,4,vec5,3,mat1)
    DO i = 1,4
        WRITE(*,*) mat1(i,:)
    END DO

!Test linear system solvers
WRITE(*,*)
WRITE(*,*) ">>>TEST: LINEAR SYSTEM SOLVERS"
WRITE(*,*) "   DIAGONAL"
    mat2 = RESHAPE((/0.4d0, 0.0d0, 0.0d0, &
                   & 0.0d0, 0.4d0, 0.0d0, &
                   & 0.0d0, 0.0d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    vec4 = (/ 0.2d0, 0.4d0, 0.1d0 /)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)
    WRITE(*,*) vec4
    WRITE(*,*)

    CALL solvediagsys(mat2,3,vec4,vec5)
    WRITE(*,*) vec5
    WRITE(*,*)

    WRITE(*,*) "   UPPER TRIANGULAR"
    mat2 = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                   & 0.0d0, 0.4d0, 0.1d0, &
                   & 0.0d0, 0.0d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    vec4 = (/ 0.2d0, 0.4d0, 0.1d0 /)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)
    WRITE(*,*) vec4
    WRITE(*,*)

    CALL backsub(mat2,3,vec4,vec5)
    WRITE(*,*) vec5
    WRITE(*,*)

    WRITE(*,*) "   LOWER TRIANGULAR"
    mat2 = RESHAPE((/0.4d0, 0.0d0, 0.0d0, &
                   & 0.6d0, 0.4d0, 0.0d0, &
                   & 1.9d0, 0.1d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    vec4 = (/ 0.2d0, 0.4d0, 0.1d0 /)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)
    WRITE(*,*) vec4
    WRITE(*,*)

    CALL forsub(mat2,3,vec4,vec5)
    WRITE(*,*) vec5
    WRITE(*,*)

    WRITE(*,*) "   ROW ECHELON"
    mat1 = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                   & 0.2d0, 0.4d0, 0.1d0, &
                   & 0.4d0, 0.1d0, 1.0d0, &
                   & 2.0d0, 0.7d0, 0.2d0/),(/4,3/),ORDER=(/2,1/))

    DO i = 1,3
        WRITE(*,*) mat1(i,:)
    END DO
    WRITE(*,*)

    CALL rowechelon(mat1,4,3)
    DO i = 1,4
        WRITE(*,*) mat1(i,:)
    END DO
    WRITE(*,*)

    WRITE(*,*) "   GAUSSIAN ELIMINATION"
    mat2 = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                   & 0.6d0, 0.4d0, 0.1d0, &
                   & 1.9d0, 0.1d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    vec4 = (/ 0.2d0, 0.4d0, 0.1d0 /)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)
    WRITE(*,*) vec4
    WRITE(*,*)

    CALL solvegausselim(mat2,3,vec4,vec5)
    WRITE(*,*) vec5
    WRITE(*,*)

    CALL solvegausselim2(mat2,3,vec4,vec5)
    WRITE(*,*) vec5
    WRITE(*,*)

!    WRITE(*,*) "   GAUSSIAN ELIMINATION TIME TEST"

!    DO i = 1,4
!        n = 10**i
!        ALLOCATE(matall1(n,n),vecall1(n),vecall2(n),vecall3(n))

!        CALL randmat(n,n,matall1)

!        CALL CPU_TIME(num1)
!        CALL solvegausselim(matall1,n,vecall1,vecall2)
!        CALL CPU_TIME(num2)
!        num3 = num2 - num1

!        CALL CPU_TIME(num1)
!        CALL solvegausselim2(matall1,n,vecall1,vecall3)
!        CALL CPU_TIME(num2)
!        num4 = num2 - num1

!        WRITE(*,*) n, num3, num4

!        DEALLOCATE(matall1,vecall1,vecall2,vecall3)
!    END DO

    WRITE(*,*) "   LU DECOMPOSITION"
    mat2 = RESHAPE((/1.0d0, -1.0d0, 3.0d0, &
                   & 1.0d0, 1.0d0, 0.0d0, &
                   & 3.0d0, -2.0d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)

    CALL LUdecomp(mat2,3)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO

    WRITE(*,*)
    WRITE(*,*) "   SOLVE LU DECOMPOSITION"
    mat2 = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                   & 0.6d0, 0.4d0, 0.1d0, &
                   & 1.9d0, 0.1d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    vec4 = (/ 0.2d0, 0.4d0, 0.1d0 /)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)
    WRITE(*,*) vec4
    WRITE(*,*)

    CALL solveLUfactor(mat2,3,vec4,vec5,.TRUE.)
    WRITE(*,*) vec5
    WRITE(*,*)


    WRITE(*,*) "   CHOLESKY DECOMPOSITION"
    mat2 = RESHAPE((/4.0d0, 12.0d0, -16.0d0, &
                   & 12.0d0, 37.0d0, -43.0d0, &
                   & -16.0d0, -43.0d0, 98.0d0/),(/3,3/),ORDER=(/2,1/))
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)

    CALL choldecomp(mat2,3,i)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)


    WRITE(*,*) "   SYMMETRIC POSITIVE DEFINITE MATRIX"

    CALL randspdmat(3,mat2)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)

!    ALLOCATE(matall1(100,100))
!    OPEN(10, FILE='out.dat', STATUS='new')
!    CALL randspdmat(100,matall1)
!    CALL choldecomp(matall1,100,i)
!    DO i = 1,100
!        WRITE(10,*) matall1(i,:)
!    END DO
!    WRITE(*,*)
!    CLOSE(10)
!    DEALLOCATE(matall1)

    WRITE(*,*) "   LEAST SQUARES (NORMAL EQUATIONS)"
    mat6 = RESHAPE((/1.0d0, 0.0d0, 1.0d0, &
                   & 2.0d0, 3.0d0, 5.0d0, &
                   & 5.0d0, 3.0d0, -2.0d0, &
                   & 3.0d0, 5.0d0, 4.0d0, &
                   & -1.0d0, 6.0d0, 3.0d0/),(/5,3/),ORDER=(/2,1/))
    vec7 = (/ 4.0d0, -2.0d0, 5.0d0, -2.0d0, 1.0d0 /)
    DO i = 1,5
        WRITE(*,*) mat6(i,:)
    END DO
    WRITE(*,*)
    WRITE(*,*) vec7
    WRITE(*,*)

    CALL lsnormal(mat6,5,3,vec7,vec4)
    WRITE(*,*) vec4
    WRITE(*,*)

    WRITE(*,*) "   QR DECOMPOSITION"
    mat2 = RESHAPE((/12.0d0, -51.0d0, 4.0d0, &
                   & 6.0d0, 167.0d0, -68.0d0, &
                   & -4.0d0, 24.0d0, -41.0d0/),(/3,3/),ORDER=(/2,1/))
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)

    CALL QRdecomp(mat2,3,3,mat7,mat8)
    WRITE(*,*) "Q = "
    DO i = 1,3
        WRITE(*,*) mat7(i,:)
    END DO
    WRITE(*,*) "R = "
    DO i = 1,3
        WRITE(*,*) mat8(i,:)
    END DO
    WRITE(*,*)

    WRITE(*,*) "   QR DECOMPOSITION MOD"
    mat2 = RESHAPE((/12.0d0, -51.0d0, 4.0d0, &
                   & 6.0d0, 167.0d0, -68.0d0, &
                   & -4.0d0, 24.0d0, -41.0d0/),(/3,3/),ORDER=(/2,1/))
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)

    CALL QRdecompmod(mat2,3,3,mat7,mat8)
    WRITE(*,*) "Q = "
    DO i = 1,3
        WRITE(*,*) mat7(i,:)
    END DO
    WRITE(*,*) "R = "
    DO i = 1,3
        WRITE(*,*) mat8(i,:)
    END DO
    WRITE(*,*)

    WRITE(*,*) "   QR DECOMPOSITION HOUSE"
    mat2 = RESHAPE((/12.0d0, -51.0d0, 4.0d0, &
                   & 6.0d0, 167.0d0, -68.0d0, &
                   & -4.0d0, 24.0d0, -41.0d0/),(/3,3/),ORDER=(/2,1/))
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)

    CALL QRdecomphouse(mat2,3,3,mat7,mat8)
    WRITE(*,*) "Q = "
    DO i = 1,3
        WRITE(*,*) mat7(i,:)
    END DO
    WRITE(*,*) "R = "
    DO i = 1,3
        WRITE(*,*) mat8(i,:)
    END DO
    WRITE(*,*)

    !Test QR factorization of Hilbert Matrices
!    DO i = 4,10,2
!        WRITE(*,*) i
!        ALLOCATE(matall1(i,i),matall2(i,i),matall3(i,i))

!        DO j = 1,i
!            DO k = j,i
!                matall1(j,k) = 1.0d0/DBLE(j+k-1)
!                matall1(k,j) = matall1(j,k)
!            END DO
!        END DO

!        CALL QRdecomphouse(matall1,i,matall2,matall3)

!        CALL multmat(TRANSPOSE(matall2),matall2,i,i,i,matall3)

!        DO j = 1,i
!            WRITE(*,'(*(E9.1))') matall3(j,:)
!        END DO
!        WRITE(*,*)

!        DEALLOCATE(matall1,matall2,matall3)
!    END DO

    WRITE(*,*)
    WRITE(*,*) "   SOLVE QR DECOMPOSITION"
    mat2 = RESHAPE((/0.4d0, 0.6d0, 1.9d0, &
                   & 0.6d0, 0.4d0, 0.1d0, &
                   & 1.9d0, 0.1d0, 1.0d0/),(/3,3/),ORDER=(/2,1/))
    vec4 = (/ 0.2d0, 0.4d0, 0.1d0 /)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)
    WRITE(*,*) vec4
    WRITE(*,*)

    CALL solveQRfactor(mat2,3,vec4,vec5)
    WRITE(*,*) vec5
    WRITE(*,*)

    WRITE(*,*) "   LEAST SQUARES (QR GSmod)"
    mat6 = RESHAPE((/1.0d0, 0.0d0, 1.0d0, &
                   & 2.0d0, 3.0d0, 5.0d0, &
                   & 5.0d0, 3.0d0, -2.0d0, &
                   & 3.0d0, 5.0d0, 4.0d0, &
                   & -1.0d0, 6.0d0, 3.0d0/),(/5,3/),ORDER=(/2,1/))
    vec7 = (/ 4.0d0, -2.0d0, 5.0d0, -2.0d0, 1.0d0 /)
    DO i = 1,5
        WRITE(*,*) mat6(i,:)
    END DO
    WRITE(*,*)
    WRITE(*,*) vec7
    WRITE(*,*)

    CALL lsQRgsmod(mat6,5,3,vec7,vec4)
    WRITE(*,*) vec4
    WRITE(*,*)

     WRITE(*,*) "   LEAST SQUARES (QR Householder)"
    mat6 = RESHAPE((/1.0d0, 0.0d0, 1.0d0, &
                   & 2.0d0, 3.0d0, 5.0d0, &
                   & 5.0d0, 3.0d0, -2.0d0, &
                   & 3.0d0, 5.0d0, 4.0d0, &
                   & -1.0d0, 6.0d0, 3.0d0/),(/5,3/),ORDER=(/2,1/))
    vec7 = (/ 4.0d0, -2.0d0, 5.0d0, -2.0d0, 1.0d0 /)
    DO i = 1,5
        WRITE(*,*) mat6(i,:)
    END DO
    WRITE(*,*)
    WRITE(*,*) vec7
    WRITE(*,*)

    CALL lsQRhouse(mat6,5,3,vec7,vec4)
    WRITE(*,*) vec4
    WRITE(*,*)

    WRITE(*,*) "   SOLVE JACOBI"
    mat2 = RESHAPE((/3.0d0, -1.0d0, 1.0d0, &
                   & 1.0d0, -4.0d0, 1.0d0, &
                   & 1.0d0, 2.0d0, -6.0d0/),(/3,3/),ORDER=(/2,1/))
    vec4 = (/ 2.0d0, -0.0d0, 1.0d0 /)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)
    WRITE(*,*) vec4
    WRITE(*,*)

    vec5 = (/ 0.1d0, 0.1d0, 0.1d0 /)

    CALL solvejacobi(mat2,3,vec4,vec5,10.d-16,100,.False.,vec6)
    WRITE(*,*) vec6
    WRITE(*,*)

    WRITE(*,*) "   SOLVE GAUSS-SEIDEL"
    mat2 = RESHAPE((/3.0d0, -1.0d0, 1.0d0, &
                   & 1.0d0, -4.0d0, 1.0d0, &
                   & 1.0d0, 2.0d0, -6.0d0/),(/3,3/),ORDER=(/2,1/))
    vec4 = (/ 2.0d0, -0.0d0, 1.0d0 /)
    DO i = 1,3
        WRITE(*,*) mat2(i,:)
    END DO
    WRITE(*,*)
    WRITE(*,*) vec4
    WRITE(*,*)

    vec5 = (/ 0.1d0, 0.1d0, 0.1d0 /)

    CALL solvegaussseidel(mat2,3,vec4,vec5,10.d-16,100,.False.,vec6)
    WRITE(*,*) vec6
    WRITE(*,*)


    !    ALLOCATE(matall1(100,100))
!    OPEN(10, FILE='out.dat', STATUS='new')
!    CALL randspdmat(100,matall1)
!    CALL choldecomp(matall1,100,i)
!    DO i = 1,100
!        WRITE(10,*) matall1(i,:)
!    END DO
!    WRITE(*,*)
!    CLOSE(10)
!    DEALLOCATE(matall1)



END PROGRAM
