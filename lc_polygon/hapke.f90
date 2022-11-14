! hapke.f90
! Hapke law.
! Miroslav Broz (miroslav.broz@email.cz), Nov 8th 2022

! Reference: Spjuth (2009)

! Notation:
!
! f_L             .. Lambert
! f_L/(mu_i+mu_e) .. Lommel
! P               .. Henyey-Greenstein, 1-term, dark surface
! ming            .. asymmetry factor, g = 0 is isotropic
! H               .. Chandrasekhar
! H(mu_i)H(mu_e)  .. multiple scattering, isotropic, bright surface
! 1+B             .. opposition effect, shadow hiding
! B0              .. amplitude of o. e.
! minh            .. width of o. e.
! bartheta        .. mean slope; NOT USED!

! M(mu_i,mu_e)    .. multiple scattering, anisotropic; Eq. (2.21) NOT USED!
! 1+B_C           .. opposition effect, coherent back-scattering; Eq. (2.27) NOT USED!
! K               .. porosity; Eq. (2.48) NOT USED!
! S               .. surface roughness; Eqs. (2.36)-(2.45) NOT USED!
! i               .. mean incident angle, cos i = mu_i; Eq. (2.34) NOT USED!
! e               .. mean outgoing angle, cos e = mu_e; Eq. (2.35) NOT USED!

module hapke_module

double precision :: B, P, r0

contains

double precision function f_hapke(f_L, mu_i, mu_e, alpha)

implicit none
double precision, intent(in) :: f_L, mu_i, mu_e, alpha
double precision :: tmp

tmp = mu_i+mu_e
if (tmp.ne.0.d0) then
  f_hapke = f_L/tmp * ((1.d0+B)*P + H(mu_i)*H(mu_e) - 1.d0)  ! Eq. (2.16)
else
  f_hapke = 0.d0
endif

return
end function f_hapke

! Note: in Broz & Solc (2013), a bracket in H(mu) is missing!

double precision function H(mu)

use input_module, only : A_w

implicit none
double precision, intent(in) :: mu

H = (1.d0 - A_w*mu*(r0 + (1.d0-2.d0*r0*mu)/2.d0 * log((1.d0+mu)/mu)))**(-1.d0)  ! Eq. (2.17)

return
end function H

subroutine init_hapke(alpha)

use input_module

implicit none
double precision, intent(in) :: alpha

B = B0/(1.d0+1.d0/minh*tan(alpha/2.d0))  ! Eq. (2.26)
P = (1.d0-ming**2)/(1.d0 + 2.d0*ming*cos(alpha) + ming**2)**(3.d0/2.d0)  ! Eq. (2.13)
r0 = (1.d0-sqrt(1.d0-A_w))/(1.d0+sqrt(1.d0-A_w))  ! Eq. (2.18)

!write(*,*) 'B = ', B 
!write(*,*) 'P = ', P 
!write(*,*) 'r0 = ', r0 
!write(*,*) ''

end subroutine init_hapke

end module hapke_module


