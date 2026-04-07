! distance_AB_C.f90
! Distance of point C from line AB.
! Miroslav Broz (miroslav.broz@email.cz), Jul 24th 2015

module distance_AB_C_module

contains

double precision function distance_AB_C(A, B, C, t, extra)

implicit none
double precision, dimension(:), intent(in) :: A, B, C
double precision, intent(out) :: t
logical, intent(out) :: extra

double precision, parameter :: eps = 1.d-16
double precision :: d_, AB_sq, CD
double precision, dimension(:), allocatable :: D

AB_sq = dot_product(B-A, B-A)

if (abs(AB_sq).lt.eps) then
  extra = .TRUE.
  distance_AB_C = 0.d0
  return
endif

t = dot_product(C-A, B-A) / AB_sq

if ((t.lt.0d0).or.(t.gt.1.d0)) then
  extra = .TRUE.
  distance_AB_C = 0.d0
  return
else
  extra = .FALSE.
endif

allocate(D(size(A,1)))

D = A + (B-A)*t
CD = sqrt(dot_product(D-C, D-C))

deallocate(D)

distance_AB_C = CD
return
end function distance_AB_C

end module distance_AB_C_module
