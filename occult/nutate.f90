! nutate.f90
! Nutation of RA, DE coordinates.
! Miroslav Broz (miroslav.broz@email.cz), Jun 17th 2022

! Reference:
! Wolf et al. (1992) Astronomicka prirucka. Praha: Academia.

module nutate_module

contains

subroutine nutate_angle(t, deltapsi, deltaeps)

! nutation angles

use const_module

implicit none      
double precision, intent(in) :: t
double precision, intent(out) :: deltapsi, deltaeps

double precision :: l, l2, f, d, omega
double precision :: t2, t3

t2 = t*t
t3 = t2*t

l = 485866.733d0 + 1717915922.633d0*t + 31.310d0*t2 + 0.064d0*t3
l2 = 1287099.804d0 + 129596581.224d0*t - 0.577d0*t2 - 0.012d0*t3
f = 335778.677d0 + 1739527263.137d0*t - 13.257d0*t2 + 0.011d0*t3
d = 1072261.307d0 + 1602961601.328d0*t - 6.891d0*t2 + 0.019d0*t3
omega = 450160.280d0 - 6962890.539d0*t + 0.455d0*t2 + 0.008d0*t3

l = l*arcsec
l2 = l2*arcsec
f = f*arcsec
d = d*arcsec
omega = omega*arcsec

deltapsi = &
  - 17.20d0*sin(omega) &
  - 1.32d0*sin(2*f-2*d+2*omega) &
  - 0.23d0*sin(2*f+2*omega) &
  + 0.21d0*sin(2*omega) &
  + 0.14d0*sin(l2) &
  + 0.07d0*sin(l) &
  - 0.05d0*sin(l2+2*f-2*d+2*omega) &
  - 0.04d0*sin(2*f+omega) &
  - 0.03d0*sin(l+2*f+2*omega)

deltaeps = &
  + 9.20d0*cos(omega) &
  + 0.57d0*cos(2*f-2*d+2*omega) &
  + 0.10d0*cos(2*f+2*omega) &
  - 0.09d0*cos(2*omega) &
  + 0.01d0*cos(l2) &
  + 0.00d0*cos(l) &
  + 0.02d0*cos(l2+2*f-2*d+2*omega) &
  + 0.02d0*cos(2*f+omega) &
  + 0.01d0*cos(l+2*f+2*omega)

deltapsi = deltapsi*arcsec
deltaeps = deltaeps*arcsec
return
end subroutine nutate_angle

subroutine nutate(r, eps, deltapsi, deltaeps)

! correction for nutation

use rotateinv_module

implicit none      
double precision, dimension(3), intent(inout) :: r
double precision, intent(in) :: eps, deltapsi, deltaeps

r = rot_x(r, cos(eps), sin(eps))
r = rot_z(r, cos(-deltapsi), sin(-deltapsi))
r = rot_x(r, cos(-eps-deltaeps), sin(-eps-deltaeps))

return
end subroutine nutate

subroutine nutate2(r, eps, deltapsi, deltaeps)

! backward correction for nutation

use rotateinv_module

implicit none      
double precision, dimension(3), intent(inout) :: r
double precision, intent(in) :: eps, deltapsi, deltaeps

r = rot_x(r, cos(eps+deltaeps), sin(eps+deltaeps))
r = rot_z(r, cos(deltapsi), sin(deltapsi))
r = rot_x(r, cos(-eps), sin(-eps))

return
end subroutine nutate2

end module nutate_module


