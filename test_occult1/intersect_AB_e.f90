! intersect_AB_e.f90
! Intersection of line AB with ellipsoid e(R).
! Miroslav Broz (miroslav.broz@email.cz), Jun 9th 2022

module intersect_AB_e_module

contains

subroutine intersect_AB_e(A, B, e, axes, has_solution)

use const_module

implicit none
double precision, dimension(3), intent(in) :: A, B
double precision, dimension(3), intent(out) :: e
double precision, dimension(3), intent(in) :: axes

double precision :: a_, b_, c_, x, x1, x2
logical :: has_solution

! \vec A ... Earth->asteroid
! \vec B ... star->asteroid (normalized)
! \vec k ... intersection point on sphere
! R      ... radius

! \vec A + x*\vec B = \vec k
! (k1/a)^2 + (k2/b)^2 + (k3/c)^2 = 1
! ((A1+x*B1)/a)^2 + ((A2+x*B2)/b)^2 + ((A3+x*B3)/c)^2 = 1
! A1^2/a^2 + 2 A1 B1/a^2 x + B1^2/a^2 x^2 + A2^2/b^2 + .. - 1 = 0

!write(*,*) '# A = ', A
!write(*,*) '# B = ', B
!write(*,*) '# R = ', R, ' au = ', R*au/1.d3, ' km'

a_ = (B(1)/axes(1))**2 + (B(2)/axes(2))**2 + (B(3)/axes(3))**2
b_ = 2.d0*(A(1)*B(1)/axes(1)**2 + A(2)*B(2)/axes(2)**2 + A(3)*B(3)/axes(3)**2)
c_ = (A(1)/axes(1))**2 + (A(2)/axes(2))**2 + (A(3)/axes(3))**2 - 1.d0

call quadratic_eq(a_, b_, c_, x1, x2, has_solution)

!write(*,*) '# x1 = ', x1
!write(*,*) '# x2 = ', x2
!write(*,*) '# has_solution = ', has_solution

if (.not.has_solution) then
  return
endif

x = min(x1, x2)
if (x.lt.0.d0) then
  has_solution = .False.
endif

e = A + x*B

!write(*,*) '# k = ', k
!write(*,*) '# |k| = ', sqrt(dot_product(k, k))*au/1.d3, " km"

return
end subroutine intersect_AB_e

subroutine quadratic_eq(a, b, c, x1, x2, has_solution)

implicit none
double precision, intent(in) :: a, b, c
double precision, intent(out) :: x1, x2
logical, intent(out) :: has_solution

real*16 :: D, a_, b_, c_, x1_, x2_

a_ = a
b_ = b
c_ = c

D = b_*b_ - 4.d0*a_*c_

if (D.lt.0.d0) then
  has_solution = .False.
  return
endif

has_solution = .True.
x1_ = (-b_ - sqrt(D))/(2.d0*a_)
x2_ = (-b_ + sqrt(D))/(2.d0*a_)

x1 = x1_
x2 = x2_

return
end subroutine quadratic_eq

end module intersect_AB_e_module

