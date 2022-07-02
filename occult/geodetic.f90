! geodetic.f90
! Convert xyz to geodetic coordinates.
! Miroslav Broz (miroslav.broz@email.cz), Jun 17th 2022

! Reference:
! https://en.wikipedia.org/wiki/Geodetic_coordinates

module geodetic_module

contains

subroutine geodetic(e, axes, lambda, phi, h)

use const_module

implicit none
double precision, dimension(3), intent(in) :: e, axes
double precision, intent(out) :: lambda, phi
double precision, intent(in) :: h

integer :: i
double precision :: a, b, esq, N, phi_, delta, abse

a = axes(1)
b = axes(3)
esq = 1.d0-b*b/(a*a)

! first-guess on a sphere
abse = sqrt(dot_product(e, e))
lambda = atan2(e(2), e(1))
phi = asin(e(3)/abse)
delta = pi
i = 1

! iterations on an ellipsoid
do while ((abs(delta).gt.1.0d-8).and.(i.lt.50))
  phi_ = phi
  N = a/sqrt(1.d0-esq*(sin(phi))**2)
  phi = asin(e(3)/(b*b/(a*a)*N + h))
  delta = phi-phi_
  i = i+1
enddo

return
end subroutine geodetic

subroutine geodetic2(lambda, phi, h, e, axes)

implicit none
double precision, intent(in) :: lambda, phi, h
double precision, dimension(3), intent(out) :: e
double precision, dimension(3), intent(in) :: axes

double precision :: a, b, N

a = axes(1)
b = axes(3)

N = a*a/sqrt((a*cos(phi))**2 + (b*sin(phi))**2)

e(1) = (N+h)*cos(lambda)*cos(phi)
e(2) = (N+h)*cos(lambda)*cos(phi)
e(3) = (b*b/(a*a)*N+h)*sin(phi)

return
end subroutine geodetic2

end module geodetic_module


