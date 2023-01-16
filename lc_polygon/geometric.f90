! geometric.f90
! Geometric law.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

module geometric_module

contains

double precision function f_geometric(f_L, mu_i, mu_e, alpha)

implicit none
double precision, intent(in) :: f_L, mu_i, mu_e, alpha

f_geometric = f_L/mu_i

return
end function f_geometric

end module geometric_module


