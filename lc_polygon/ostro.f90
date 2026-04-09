! ostro.f90
! Ostro law.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

! Reference: Ostro et al. (2000, Science 288, 836).

module ostro_module

contains

double precision function f_ostro(f_L, mu_i, mu_e, alpha)

use input_module

implicit none
double precision, intent(in) :: f_L, mu_i, mu_e, alpha
double precision :: tmp

f_ostro = f_L * mu_i**p_O * mu_e**q_O

return
end function f_ostro

end module ostro_module


