! factorial.f90
! Compute factorial (centre of mass).
! Miroslav Broz (miroslav.broz@email.cz), Nov 18th 2019

module factorial_module

contains

double precision function factorial(n)

implicit none

integer :: n
integer :: i

factorial = 1.d0

do i = n, 2, -1
  factorial = factorial*i
enddo

return

end function factorial

end module factorial_module

