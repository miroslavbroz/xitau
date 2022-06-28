! fplane.f90
! Fundamental plane for occultations.
! Miroslav Broz (miroslav.broz@email.cz), Jun 28th 2022

!      /|                
!     / <--___           
!    /  |     --A--___   
!   | E-----------------O
!   |  /                 
!   | /                  
!   |/                   

module fplane_module

contains

subroutine fplane(r_EA, r_AO, r_EO, u, v, has_solution)

use normalize_module
use vector_product_module
use intersect_AB_p_module

implicit none
double precision, dimension(3), intent(in) :: r_EA, r_AO, r_EO
double precision, intent(out) :: u, v
logical, intent(out) :: has_solution

double precision, dimension(3) :: p
double precision, dimension(3) :: hatu, hatv, hatw
double precision :: w

call intersect_AB_p(r_EA, -r_AO, p, (/r_EO(1),r_EO(2),r_EO(3),0.d0/), has_solution)

hatw = normalize(r_EO)
hatu = normalize((/r_EO(2), -r_EO(1), 0.d0/))
hatv = vector_product(hatu,hatw)

u = dot_product(hatu, p)
v = dot_product(hatv, p)
w = dot_product(hatw, p)

return
end subroutine fplane

end module fplane_module


