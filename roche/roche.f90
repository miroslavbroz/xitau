! roche.f90
! Roche lobe computations.
! Reference: Leahy & Leahy (2015), Comp. Astrophys. Cosmology, 2, 4.
! Rewritten from http://www.quarknova.ca/software/RocheLobe.f90.

module roche_module

! q ... mass ratio
! F ... fill-out factor
! P ... rotation parameter (a.k.a. synchronicity)
! RL1 ... primary--L1 distance
! OmegaL1 ... Kopal potential value (for L1)
! OmegaF ... Kopal potential value (for F)

double precision :: q, F, P, RL1, OmegaL1, OmegaF
double precision :: theta_for_func1
double precision :: theta_for_func3, phi_for_func3

contains

! Kopal potential w. non-synchorouns rotation, Eq. (2).

! Note: -q**2/(2.d0*(1.d0+q)) term is indeed missing;
! as in Prsa (2011), Phoebe Sci. Ref., Eq. (3.15)

! Note: for e > 0, we should use Eq. (4):
! P'^2 = P^2(1 - e^2)/(1 + e cos(nu))^3,
! where nu is the true anomaly.

double precision function potential(r, theta, phi)

implicit none
double precision, intent(in) :: r, theta, phi
double precision :: cosphi, sintheta, term1, term2, term3
double precision :: x, y, z, r1, r2

cosphi = cos(phi)
sintheta = sin(theta)

term1 = 1.d0/r
term2 = q/sqrt(1.d0 - 2.d0*r*cosphi*sintheta + r*r)
term3 = 0.5d0*(1.d0+q)*P*P*r*r*sintheta*sintheta - q*r*cosphi*sintheta 
potential = term1 + term2 + term3

return
end function potential

! The derivative w.r.t. x of Kopal potential for y = z = 0.

double precision function deriv(x)
implicit none
double precision, intent(in) :: x
double precision :: term1, term2, term3

term1 = 1.d0/x**2
term2 = q/((1.d0-x)**2)
term3 = (q+1.d0)*P*P*x - q

if (x.lt.0.d0) then
  deriv = +term1 + term2 + term3
else if (x.lt.1.d0) then
  deriv = -term1 + term2 + term3
else
  deriv = -term1 - term2 + term3
endif

return
end function deriv

! Eggleton (1983) formula for volume-equivalent critical (L1) radius

double precision function REggleton(q)
implicit none
double precision, intent(in) :: q 

REggleton = 0.49d0/(0.6d0+q**(2.d0/3.d0)*log(1.d0+q**(-1.d0/3.d0)))

return
end function REggleton

! Secondary potential correction (see also Harmanec 2021; Eq. (14))

double precision function secondary_correction(Omega, q)
implicit none
double precision, intent(in) :: Omega, q

secondary_correction = 1.d0/q*Omega + (q-1.d0)/(2.d0*q)

return
end function secondary_correction

! Volume of the Roche surface.

double precision function RocheVolume()

use const_module
use romberg_module

implicit none

RL1 = GetDerivRoot(1)
OmegaL1 = potential(RL1, pi/2.d0, 0.d0)
OmegaF = (OmegaL1 + q*q/2.d0/(1.d0+q))/F - q*q/2.d0/(1.d0+q)
RocheVolume = 2.0d0*romberg(func2, 0.0d0, 1.d0)

!write(*,*) 'F = ', F
!write(*,*) 'RL1 = ', RL1
!write(*,*) 'OmegaL1 = ', OmegaL1
!write(*,*) 'OmegaF = ', OmegaF
!write(*,*) 'RocheVolume = ', RocheVolume

return
end function RocheVolume

! Helper function for integration over phi.

double precision function func1(phi)
implicit none
double precision, intent(in) :: phi
double precision r
r = GetPotRoot(theta_for_func1, phi)
func1 = 2.d0/3.d0*r*r*r
return
end function func1

! Helper function for integration over mu = cos(theta).

double precision function func2(mu)
use const_module
use romberg_module
implicit none
double precision, intent(in) :: mu
theta_for_func1 = acos(mu)
func2 = romberg(func1, 0.d0, pi)
return
end function func2

! Helper function to find the root for.

double precision function func3(r)
implicit none
double precision :: r
func3 = potential(r, theta_for_func3, phi_for_func3) - OmegaF
return
end function func3

! Get r of the Roche surface for given theta, phi and OmegaF

double precision function GetPotRoot(theta, phi)

use const_module
use root_module

implicit none
double precision, intent(in) :: theta, phi
double precision :: a, b, s
double precision :: Flim, alim, ang, theta2, phi2
logical :: flag

! Near L1 point Omega-OmegaF is tangent to zero so root() doesn't work...
! ... use R(theta,phi) = RL1*(1-ang/alim) + R(alim)*ang/alim instead.

Flim = 0.997d0
alim = 0.1d0*deg
ang = sqrt(phi*phi + (theta-pi/2.d0)*(theta-pi/2.d0))

if ((F.gt.Flim).and.(ang.lt.alim)) then
  theta2 = pi/2.d0; phi2 = alim
  flag = .true.
else
  theta2 = theta; phi2 = phi
  flag = .false.
endif

! initial guess
a = 0.0001d0*REggleton(q)
b = 1.0001d0*RL1

theta_for_func3 = theta2
phi_for_func3 = phi2

s = root(func3, a, b)

if (flag) then
  GetPotRoot = RL1*(1.d0-ang/alim) + s*ang/alim
else
  GetPotRoot = s
endif

return
end function GetPotRoot

! Get critical points of the Roche Surface.

double precision function GetDerivRoot(iroot)

use root_module

integer, intent(in) :: iroot
double precision :: a, b

! initial guesses for 3 cases: 0<x<1, x<0, x>1
if (iroot.eq.1) then
  a = 1.d-3; b = 0.999d0
else if (iroot.eq.2) then
  a = -1.d2; b = -0.0001d0
else if (iroot.eq.3) then
  a = 1.001d0; b = 1.d4
endif

GetDerivRoot = root(deriv, a, b)
return
end function GetDerivRoot

end module roche_module


