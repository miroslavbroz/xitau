! lambert.f90
! Lambert law.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

module lambert_module

contains

double precision function f_lambert(f_L, mu_i, mu_e, alpha)

implicit none
double precision, intent(in) :: f_L, mu_i, mu_e, alpha

f_lambert = f_L

return
end function f_lambert

end module lambert_module


