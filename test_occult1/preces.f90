! preces.f
! Precession of RA, DE coordinates.
! Miroslav Broz (miroslav.broz@email.cz), Jun 17th 2022

! Reference:
! Wolf et al. (1992) Astronomicka prirucka. Praha: Academia.

module preces_module

contains

subroutine preces_angle(t0, t, zeta, zz, theta)

!  precession angles from epoch t1 to t2
!
!  t0 = t1-J2000 ... time from standard epoch [Julian cy]
!  t = t2-t1     ... time from t1 epoch [Julian cy]

use const_module
  
implicit none
double precision, intent(in) :: t0, t
double precision, intent(out) :: zeta, zz, theta

double precision t02, t2, t3

t02 = t0*t0
t2 = t*t
t3 = t2*t
zeta = (2306.2181d0 + 1.39656d0*t0 - 0.000139d0*t02)*t &
  + (0.30188d0 - 0.000344d0*t0)*t2 &
  + 0.017998d0*t3
zz = (2306.2181d0 + 1.39656d0*t0 - 0.000139d0*t02)*t &
  + (1.09468d0+0.000066d0*t0)*t2 &
  + 0.018203d0*t3
theta = (2004.3109d0 - 0.85330d0*t0 - 0.000217d0*t02)*t &
  - (0.42665d0 + 0.000217d0*t0)*t2 &
  - 0.041833d0*t3
zeta = zeta*arcsec
zz = zz*arcsec
theta = theta*arcsec

return
end

subroutine preces(r, zeta, zz, theta)

! correction for precession
      
use rotate_module

implicit none
double precision, dimension(3), intent(inout) :: r
double precision, intent(in) :: zeta, zz, theta

r = rot_z(r, cos(-zeta), sin(-zeta))
r = rot_y(r, cos(theta), sin(theta))
r = rot_z(r, cos(-zz), sin(-zz))

return
end subroutine preces

subroutine preces2(r, zeta, zz, theta)

! backward correction for precession
      
use rotate_module

implicit none
double precision, dimension(3), intent(inout) :: r
double precision, intent(in) :: zeta, zz, theta

r = rot_z(r, cos(zz), sin(zz))
r = rot_y(r, cos(-theta), sin(-theta))
r = rot_z(r, cos(zeta), sin(zeta))

return
end subroutine preces2

end module preces_module


