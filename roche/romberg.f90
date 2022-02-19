! romberg.f90

module romberg_module

contains

! Romberg integration of f(x).

double precision function romberg(func, a, b) 

implicit none
double precision :: func
double precision, intent(in) :: a, b

integer :: i, j, k, l, m, n
double precision :: h, sum
double precision, allocatable ::  R(:,:)

n = 7
allocate(R(n, n))
h = b - a   
R(1,1) = 0.5*h*(func(a) + func(b))
l = 1
do i = 2, n
  h = 0.5*h
  l = l + l
  sum = 0.0d0
  do k = 1, l-1, 2
    sum = sum + func(a + h*dble(k))
  enddo
  m = 1
  R(i,1) = 0.5d0*R(i-1,1) + h*sum
  do j = 2, i
    m = 4*m
    R(i, j) = R(i, j-1) + (R(i, j-1) - R(i-1, j-1))/dble(m-1)
  enddo
enddo

romberg = R(n,n)
deallocate(R)
return

end function romberg

end module romberg_module


