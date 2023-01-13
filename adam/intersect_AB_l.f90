! intersect_AB_l.f90
! Intersection of line AB with line l, 2-dimensional.
! Miroslav Broz (miroslav.broz@email.cz), Dec 25th 2022

! Note: l = A + B*x, not (B-A)*x

! \vec A + \vec B*x = \vec C + \vec D*y
! A1 + B1*x = C1 + D1*y
! A2 + B2*x = C2 + D2*y
! A1*B2 - A2*B1 = C1*B2 - C2*B1 + (D1*B2 - D2*B1)*y

module intersect_AB_l_module

contains

subroutine intersect_AB_l(A, B, l, has_solution)

implicit none
double precision, dimension(2), intent(in) :: A, B
double precision, dimension(2,2), intent(in) :: l
logical, intent(out) :: has_solution

double precision, parameter :: EPS = 1.0d-8
double precision, dimension(2) :: C, D
double precision :: tmp, x, y

has_solution = .false.

C = l(1,:)
D = l(2,:)-C

tmp = D(1)*B(2) - D(2)*B(1)
if (tmp.eq.0.d0) then
  return
endif

y = (A(1)*B(2) - A(2)*B(1) - C(1)*B(2) + C(2)*B(1)) / tmp
x = (C(1) + D(1)*y - A(1))/B(1)
!e = C + D*y

if ((y.gt.0.d0-EPS).and.(y.lt.1.d0+EPS).and.(x.gt.0.d0-EPS)) then
  has_solution = .true.
endif

return
end subroutine intersect_AB_l

end module intersect_AB_l_module


