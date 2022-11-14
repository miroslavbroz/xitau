! planck.f90
! Planck function, i.e. black-body intensity in J s^-1 sr^-1 m^-2 m^-1 units.
! Miroslav Broz (miroslav.broz@email.cz), Jun 24th 2016

module planck_module

contains

double precision function planck(T, lambda)

use const_module

implicit none
double precision :: T, lambda

planck = 2.d0*h*clight**2/lambda**5 / (exp(h*clight/(lambda*k_B*T))-1.d0)

return
end function planck

end module planck_module


