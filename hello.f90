!Asks each processor on the computer to print a string.
!@author: Jackson Reid

PROGRAM hello
IMPLICIT NONE

INTEGER :: id, nthrds
INTEGER :: omp_get_thread_num, omp_get_num_threads

!$OMP PARALLEL PRIVATE(id)
id = omp_get_thread_num()
WRITE(*,*) "Hello World! From thread ", id

!$OMP BARRIER
IF (id == 0) THEN

    nthrds = omp_get_num_threads()
    WRITE(*,*) "There are ", nthrds, " threads!"

END IF

!$OMP END PARALLEL

END
