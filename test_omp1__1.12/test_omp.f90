
program omp_test

use omp_lib

integer :: i
double precision :: sum
double precision :: t1, t2

sum=0.d0

t1 = omp_get_wtime()

!$omp parallel do private(i) reduction(+:sum)
do i=1,100000000
!  if (i.eq.1) write(*,*) 'omp_get_num_threads = ', omp_get_num_threads()
  sum = sum + cos(1.0d0*i)
enddo
!$omp end parallel do

t2 = omp_get_wtime()

write(*,*) 'sum = ', sum, '; omp_get_max_threads = ', omp_get_max_threads(), '; omp_get_wtime = ', t2-t1, ' s'

stop
end

