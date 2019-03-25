PROGRAM test
IMPLICIT NONE

REAL :: snum
REAL*8 :: num1, num2, num3, num4
REAL*8 :: mat1(4,3), mat2(3,3), mat3(4,3), mat4(4,3), mat5(3,4)
REAL*8, ALLOCATABLE :: matall1(:,:), vecall1(:), vecall2(:), vecall3(:)
REAL*8 :: vec1(4), vec2(4), vec3(4), vec4(3), vec5(3), vec6(3)
INTEGER :: i, n

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

    WRITE(*,*) "   GAUSSIAN ELIMINATION TIME TEST"

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


END PROGRAM
