! geodetic.f90
! Convert xyz to geodetic coordinates.
! Miroslav Broz (miroslav.broz@email.cz), Jun 17th 2022

module geodetic_module

contains

subroutine geodetic(e, axes, lambda, phi)

use const_module

implicit none
double precision, dimension(3), intent(in) :: e, axes
double precision, intent(out) :: lambda, phi

integer :: i
double precision :: a, b, esq, h, N, phi_, delta, abse

a = axes(1)
b = axes(3)
esq = 1.d0-b*b/(a*a)
h = 0.d0

abse = sqrt(dot_product(e, e))
lambda = atan2(e(2),e(1))
phi = asin(e(3)/abse)
delta = pi
i = 1

do while ((abs(delta).gt.1.0d-8).and.(i.lt.50))
  phi_ = phi
  N = a/sqrt(1.d0-esq*(sin(phi))**2)
  phi = asin(e(3)/(b*b/(a*a)*N + h))
  delta = phi-phi_
  i = i+1
enddo

return
end subroutine geodetic

end module geodetic_module


