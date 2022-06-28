! intersect_AB_p.f90
! Intersection of line AB with plane p(a, b, c, d).
! Miroslav Broz (miroslav.broz@email.cz), Jun 28th 2022

! A .. 
! B .. 
! p .. 

! \vec A + x \vec B = \vec p
! a p1 + b p2 + c p3 + d = 0
! a (A1 + x B1) + b (A2 + x B2) + c (A3 + x B3) + d = 0
! x (a B1 + b B2 + c B3) + a A1 + b A2 + c A3 + d = 0

module intersect_AB_p_module

contains

subroutine intersect_AB_p(A, B, p, param, has_solution)

use const_module

implicit none
double precision, dimension(3), intent(in) :: A, B
double precision, dimension(3), intent(out) :: p
double precision, dimension(4), intent(in) :: param
logical :: has_solution

double precision :: a_, b_, c_, d_, x

a_ = param(1)
b_ = param(2)
c_ = param(3)
d_ = param(4)

x = a_*B(1) + b_*B(2) + c_*B(3)

if (x.eq.0.d0) then
  has_solution = .False.
  return
else
  has_solution = .True.
endif

x = (a_*A(1) + b_*A(2) + c_*A(3) + d_)/x

p = A + x*B

return
end subroutine intersect_AB_p

end module intersect_AB_p_module

