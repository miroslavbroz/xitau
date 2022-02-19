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

double precision function potential(r, theta, phi)

implicit none
double precision, intent(in) :: r, theta, phi
double precision :: cosphi, sintheta, term1, term2

cosphi = cos(phi)
sintheta = sin(theta)
term1 = 1.d0/r + q/sqrt(1.d0 - 2.d0*r*cosphi*sintheta + r*r)
term2 = -q*r*cosphi*sintheta + (q+1.d0)/2.d0 * P*P*r*r*sintheta*sintheta
potential = term1 + term2

end function potential

! The derivative w.r.t. x of Kopal potential for y = z = 0.

double precision function deriv(x)
implicit none
double precision, intent(in) :: x
double precision :: term1, term2, term3

term1 = 1.d0/(x*x)
term2 = q/((1.d0-x)*(1.d0-x))
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

! Eggleton (1983) formula

double precision function REggleton(q)
implicit none
double precision, intent(in) :: q 

REggleton = 0.49d0/(0.6d0+q**(2.d0/3.d0)*log(1.d0+q**(-1.d0/3.d0)))

end function REggleton

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
double precision :: Flim, alim, amax2, ang, ang2, theta2, phi2
logical :: flag2

! near L1 point the Omega-OmegaF is tangent to zero so rootfinder doesn't work
! in this case just use R at angle alim off the x axis (set alim=1degree).
! if F.gt.Flim .and. theta and phi within angle alim of x axis, 
! then use R(theta,phi)=RL1*(1-ang/alim)+R(alim)*ang/alim with ang=angle from x-axis
! in this case need to find the root for theta2=pi/2.d0;phi2=alim to get R(alim)

Flim = 0.997d0
alim = 0.1d0*pi/180.0d0
ang2 = phi*phi + (theta-pi/2.d0)*(theta-pi/2.d0)
ang = sqrt(ang2)
if ((F.gt.0.997d0).and.(ang.lt.alim)) then
  theta2 = pi/2.d0; phi2 = alim
  flag2 = .true.
else
  theta2 = theta; phi2 = phi
  flag2 = .false.
endif

! initial guess
a = 0.05d0*REggleton(q)
b = 1.0001d0*RL1
b = RL1

theta_for_func3 = theta2
phi_for_func3 = phi2

s = root(func3, a, b)

! for case ang<alim then need to calculate R from RL1 and s
! linear interpolate between s and RL1
if (flag2) then
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

! initial guesses for 3 cases 0<x<1, x<0, x>1
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


