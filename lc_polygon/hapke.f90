! hapke.f90
! Hapke law.
! Miroslav Broz (miroslav.broz@email.cz), Nov 18th 2022

! Reference: Spjuth (2009)

! Notation:
!
! f_L             .. bi-directonal scattering function, Lambert, sr^-1
! f_L/(mu_i+mu_e) .. Lommel
! A_w             .. single-scattering albedo, 1
! P               .. Henyey-Greenstein function, 1-term, dark surface
! ming            .. asymmetry factor, g = 0 is isotropic
! H               .. Chandrasekhar function
! H(mu_i)H(mu_e)  .. multiple scattering, isotropic, bright surface
! 1+B             .. opposition effect, shadow hiding
! B0              .. amplitude of o. e.
! minh            .. width of o. e.
! bartheta        .. mean slope
! Sr              .. surface roughness
! i               .. incident angle, cos i = mu_i
! e               .. outgoing angle, cos e = mu_e
! psi             .. azimuthal angle, phi_i-phi_e

! M(mu_i,mu_e)    .. multiple scattering, anisotropic; Eq. (2.21) NOT USED!
! 1+B_C           .. opposition effect, coherent back-scattering; Eq. (2.27) NOT USED!
! K               .. porosity; Eq. (2.48) NOT USED!

module hapke_module

double precision :: B, P, tanbartheta

contains

! Hapke bi-directional scattering function.

double precision function f_hapke(f_L, mu_i, mu_e, alpha)

use const_module

implicit none
double precision, intent(in) :: f_L, mu_i, mu_e, alpha
double precision :: tmp, A_w

tmp = mu_i+mu_e
if (tmp.ne.0.d0) then
  A_w = 4.d0*pi*f_L
  f_hapke = f_L/tmp * ((1.d0+B)*P + H(mu_i,A_w)*H(mu_e,A_w) - 1.d0) * Sr(mu_i,mu_e,alpha)  ! Eqs. (2.16), (2.31)
else
  f_hapke = 0.d0
endif

return
end function f_hapke

! Precopute parameters dependent on the phase angle.

subroutine init_hapke(alpha)

use input_module

implicit none
double precision, intent(in) :: alpha

B = B0/(1.d0+1.d0/minh*tan(alpha/2.d0))  ! Eq. (2.26)
P = (1.d0-ming**2)/(1.d0 + 2.d0*ming*cos(alpha) + ming**2)**(3.d0/2.d0)  ! Eq. (2.13)
tanbartheta = tan(bartheta)

end subroutine init_hapke

! Chandrasekhar function.
! Note: in Broz & Solc (2013), a bracket in H(mu) is missing!

double precision function H(mu, A_w)

implicit none
double precision, intent(in) :: mu, A_w
double precision :: gamma, r0

gamma = sqrt(1.d0-A_w)                                                          ! Eq. (2.19)
r0 = (1.d0-gamma)/(1.d0+gamma)                                                  ! Eq. (2.18)
H = (1.d0 - A_w*mu*(r0 + (1.d0-2.d0*r0*mu)/2.d0 * log((1.d0+mu)/mu)))**(-1.d0)  ! Eq. (2.17)

return
end function H

! Surface roughness.

double precision function Sr(mu_i, mu_e, alpha)

use const_module
use input_module

implicit none
double precision, intent(in) :: mu_i, mu_e, alpha

double precision :: cosi, cose, sini, sine, tani, tane, cospsi, psi, sinpsihalfsq
double precision :: xi, f, E1i, E1e, E2i, E2e
double precision :: K1, K2, K3
double precision :: mu_i_, mu_e_, mu_i0, mu_e0

cosi = mu_i
cose = mu_e
sini = sqrt(1.d0-cosi**2)
sine = sqrt(1.d0-cose**2)
tani = sini/cosi
tane = sine/cose

cospsi = (cos(alpha)-cosi*cose)/(sini*sine)
psi = acos(min(cospsi, 1.d0))
sinpsihalfsq = (sin(psi/2.d0))**2

xi = 1.d0/sqrt(1.d0 + pi*tanbartheta**2)
f = exp(-2.d0*tan(psi/2.d0))

E1i = exp(-2.d0/(pi*tanbartheta*tani))
E1e = exp(-2.d0/(pi*tanbartheta*tane))
E2i = exp(-1.d0/(pi*(tanbartheta*tani)**2))
E2e = exp(-1.d0/(pi*(tanbartheta*tane)**2))

if (sine.ge.sini) then

  K1 = cospsi*E2e + sinpsihalfsq*E2i
  K2 = 2.d0 - E1e - psi/pi*E1i
  K3 = E2e + sinpsihalfsq*E2i

  mu_i_ = cosi + sini*tanbartheta * K1/K2  ! Eq. (2.36)
  mu_e_ = cose + sine*tanbartheta * K3/K2  ! Eq. (2.37)

  K1 = E2e
  K2 = 2.d0 - E1e
  K3 = E2e

  mu_i0 = cosi + sini*tanbartheta * K1/K2
  mu_e0 = cose + sine*tanbartheta * K3/K2

  Sr = mu_i_/mu_i0 * mu_e_/mu_e0 * xi/(1.d0 - f + f*xi*mu_i_/mu_i0)  ! Eq. (2.38) ???

else

  K1 = cospsi*E2i + sinpsihalfsq*E2e
  K2 = 2.d0 - E1i - psi/pi*E1e
  K3 = E2i - sinpsihalfsq*E2e

  mu_i_ = cosi + sini*tanbartheta * K3/K2  ! Eq. (2.39)
  mu_e_ = cose + sine*tanbartheta * K1/K2  ! Eq. (2.40)

  K1 = E2i
  K2 = 2.d0 - E1i
  K3 = E2i

  mu_i0 = cosi + sini*tanbartheta * K3/K2
  mu_e0 = cose + sine*tanbartheta * K1/K2

  Sr = mu_i_/mu_i0 * mu_e_/mu_e0 * xi/(1.d0 - f + f*xi*mu_e_/mu_e0)  ! Eq. (2.41) ???

endif

if (.false.) then
  write(*,*) 'mu_i = ', mu_i
  write(*,*) 'mu_e = ', mu_e
  write(*,*) 'alpha = ', alpha
  write(*,*) 'bartheta = ', bartheta/deg, ' deg'
  write(*,*) 'tanbartheta = ', tanbartheta
  write(*,*) 'psi = ', psi/deg, ' deg'
  write(*,*) 'xi = ', xi
  write(*,*) 'f = ', f
  write(*,*) 'E1i = ', E1i
  write(*,*) 'E1e = ', E1e
  write(*,*) 'E2i = ', E2i
  write(*,*) 'E2e = ', E2e
  write(*,*) 'mu_i_ = ', mu_i_
  write(*,*) 'mu_e_ = ', mu_e_
  write(*,*) 'mu_i0 = ', mu_i0
  write(*,*) 'mu_e0 = ', mu_e0
  write(*,*) 'Sr = ', Sr
  stop
endif

return
end function Sr

end module hapke_module


