! intersect_AB_k.f90
! Intersection of line AB with sphere c(R).
! Miroslav Broz (miroslav.broz@email.cz), Jun 9th 2022

module intersect_AB_k_module

contains

subroutine intersect_AB_k(A, B, k, R, has_solution)

use const_module

implicit none
double precision, dimension(3), intent(in) :: A, B
double precision, dimension(3), intent(out) :: k
double precision, intent(in) :: R

double precision :: a_, b_, c_, x, x1, x2
logical :: has_solution

! \vec A ... Earth->asteroid
! \vec B ... star->asteroid (normalized)
! \vec k ... intersection point on sphere
! R      ... radius

! \vec A + x*\vec B = \vec k
! |\vec k| = R
! A.A + 2 A.B x + B.B x^2 = R^2

!write(*,*) '# A = ', A
!write(*,*) '# B = ', B
!write(*,*) '# R = ', R, ' au = ', R*au/1.d3, ' km'

a_ = dot_product(B, B)
b_ = 2.d0*dot_product(A, B)
c_ = dot_product(A, A) - R*R

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

k = A + x*B

!write(*,*) '# k = ', k
!write(*,*) '# |k| = ', sqrt(dot_product(k, k))*au/1.d3, " km"

return
end subroutine intersect_AB_k

subroutine quadratic_eq(a, b, c, x1, x2, has_solution)

implicit none
double precision, intent(in) :: a, b, c
double precision, intent(out) :: x1, x2
logical, intent(out) :: has_solution

double precision :: D

D = b*b - 4.d0*a*c

if (D.lt.0.d0) then
  has_solution = .False.
  return
endif

has_solution = .True.
x1 = (-b - sqrt(D))/(2.d0*a)
x2 = (-b + sqrt(D))/(2.d0*a)

return
end subroutine quadratic_eq

end module intersect_AB_k_module

