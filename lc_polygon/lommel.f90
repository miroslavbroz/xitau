! lommel.f90
! Lommel law.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

module lommel_module

contains

double precision function f_lommel(f_L, mu_i, mu_e, alpha)

implicit none
double precision, intent(in) :: f_L, mu_i, mu_e, alpha
double precision :: tmp

tmp = mu_i+mu_e
if (tmp.ne.0.d0) then
  f_lommel = f_L/tmp
else
  f_lommel = 0.d0
endif

return
end function f_lommel

end module lommel_module


