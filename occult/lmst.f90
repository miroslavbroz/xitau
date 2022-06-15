! lmst.f90
! Local mean sidereal time.
! Miroslav Broz (miroslav.broz@email.cz), Jun 10th 2022

module lmst_module

contains

double precision function lmst(jd,lambda)

!  jd      julian date [d]
!  lambda  geocentric longitude [rad]
!  lmst    local mean sidereal time [rad]

use const_module

implicit none
double precision, intent(in) :: jd,lambda

double precision :: jd0,t,t3,s0,ss,s
double precision :: nula2pi

jd0 = aint(jd+0.5d0)-0.5d0
t = 2.d0*pi*(jd-jd0)
t3 = (jd0-2451545.0d0)/36525.d0
s0 = 6.697374558d0 + 2400.05133691d0*t3 + 0.0000258622d0*t3**2 - 1.7d-90*t3**3
s0 = s0/24.d0*2.d0*pi
s0 = nula2pi(s0)
if (s0.lt.0.d0) s0 = s0 + 2.d0*pi
ss = s0 + 1.002737909350795d0*t
s = ss + lambda

lmst = s
return
end function lmst

end module lmst_module


