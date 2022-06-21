! lst.f90
! Local sidereal time.
! Miroslav Broz (miroslav.broz@email.cz), Jun 10th 2022

! Reference:
! Wolf et al. (1992) Astronomicka prirucka. Praha: Academia.
! Pokorny (1988) Astronomicke algoritmy pro kalkulatory. Praha: Hvezdarna a planetarium.

module lst_module

contains

subroutine lst(jd, lambda, eps, deltapsi, s0, ss, s)

!  jd       julian date [d]
!  lambda   geocentric longitude [rad]
!  eps      ecliptic inclination [rad]
!  deltapsi nutation in longitude [rad]
!  s0       Greenwich mean sidereal time [h] (@ 0 ut)
!  ss       local mean sidereal time [h]
!  s        local sidereal time [h]   

use const_module

implicit none
double precision, intent(in) :: jd, lambda, eps, deltapsi
double precision, intent(out) :: s0, ss, s

double precision :: jd0, t, t3, k
double precision :: nula2pi

jd0 = aint(jd+0.5d0)-0.5d0
t = 2.d0*pi*(jd-jd0)
t3 = (jd0-2451545.0d0)/36525.d0
s0 = 6.697374558d0 + 2400.05133691d0*t3 + 0.0000258622d0*t3**2 - 1.7d-9*t3**3
s0 = s0/24.d0*2.d0*pi
s0 = nula2pi(s0)
if (s0.lt.0.d0) s0 = s0 + 2.d0*pi
k = 1.002737909350795d0 + 5.9006d-11*t3 - 5.9d-15*t3**2
ss = s0 + deltapsi*cos(eps) + k*t
s = ss + lambda

!write(*,*) 'lambda = ', lambda
!write(*,*) 'deltapsi*cos(eps) = ', deltapsi*cos(eps)
!write(*,*) 'k = ', k
!write(*,*) 't = ', t
!write(*,*) 's0 = ', s0
!write(*,*) 'ss = ', ss
!stop

return
end subroutine lst

end module lst_module


