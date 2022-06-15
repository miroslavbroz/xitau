! occult.f90
! Occultation computation (star -> asteroid -> Earth).
! Miroslav Broz (miroslav.broz@email.cz), Jun 9th 2022

module occult_module

contains

subroutine occult(jd, r_EA, r_AS, k, R, lambda, phi, has_solution)

use intersect_AB_k_module
use const_module
use rotate_module
use lmst_module

implicit none
double precision, intent(in) :: jd, R
double precision, dimension(3), intent(in) :: r_EA, r_AS
double precision, dimension(3), intent(out) :: k
double precision, intent(out) :: lambda, phi
logical, intent(out) :: has_solution

double precision :: eps, eps_earth, s, absk
double precision, dimension(3) :: k_, k__

call intersect_AB_k(r_EA, -r_AS, k, R, has_solution)

if (.not.has_solution) then
  return
endif

k_ = k

! Earth rotation
s = lmst(jd, 0.d0)
k__ = rot_z(k_, cos(-s), sin(-s))

absk = sqrt(dot_product(k__, k__))
lambda = atan2(k__(2),k__(1))
phi = asin(k__(3)/absk)

return
end subroutine occult

end module occult_module

 
