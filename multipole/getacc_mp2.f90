! getacc_mp2.f90
! Multipole accelerations (to be added).
! Miroslav Broz (miroslav.broz@email.cz), Aug 15th 2020

module getacc_mp2_module

contains

subroutine getacc_mp2(time,nbod_,mass,xb,yb,zb,axb,ayb,azb)

use multipole2_module
use rotate_module
use const_module
use read_multipole_module

implicit none

include '../chi2/chi2.inc'
include '../chi2/dependent.inc'

integer :: nbod_
double precision, dimension(nbod_) :: mass, xb, yb, zb
double precision, dimension(nbod_) :: axb, ayb, azb
double precision :: time

integer, parameter :: npole = 10
integer, save :: maxl
double precision, dimension(0:npole,0:npole), save :: Clm, Slm
double precision, save :: capm, capr, P_rot_, Tmin, pole_l_, pole_b_, phi0_

integer, save :: i1st
data i1st /0/

integer :: i, l
double precision, dimension(3) :: r, r_, a, monopole
double precision :: tmp, phi1, phi2, phi3, time_
real :: t1, t2

if (.not.use_multipole) return

if (i1st.eq.0) then

  ! read parameters
  call read_multipole('multipole.in', Clm, Slm, capm, capr, P_rot_, Tmin, pole_l_, pole_b_, phi0_, maxl)

  i1st = 1
endif

if (debug_swift) then
  call cpu_time(t1)
endif

if (is_forward) then
  time_ = T0+time
else
  time_ = T0-time
endif

! from dependent.inc
pole_l_ = pole_l(1)
pole_b_ = pole_b(1)
phi0_ = phi0(1)
P_rot_ = P_rot(1)
Clm(2,0) = C20(1)

phi1 = -2.d0*pi*(time_-Tmin)/P_rot_ - phi0_
phi2 = -(pi/2.d0-pole_b_)
phi3 = -pole_l_

!write(*,*) 'phi1 = ', phi1
!write(*,*) 'phi2 = ', phi2
!write(*,*) 'phi3 = ', phi3

do i = 2, nbod_

  r(1) = xb(i) - xb(1)
  r(2) = yb(i) - yb(1)
  r(3) = zb(i) - zb(1)
!  write(*,*) 'r = ', r, ' au'

  ! pole direction
  r = rot_z(r, cos(phi3), sin(phi3))
  r = rot_y(r, cos(phi2), sin(phi2))

  ! axis rotation
  r = rot_z(r, cos(phi1), sin(phi1))

  ! multipole acceleration
  capm = mass(1)/GM_S*M_S
  a = a_g_mp(Clm, Slm, maxl, capm, capr, r*au)
  a = a/(au/day**2)
!  write(*,*) 'a_g_mp = ', a, ' au day^-2'

  ! subtract monopole
  tmp = dot_product(r, r)
  monopole = -mass(1)/(tmp*sqrt(tmp))*r
  a = a - monopole
!  write(*,*) 'monopole = ', monopole, ' au day^-2'
!  stop

  ! back-rotation
  a = rot_z(a, cos(-phi1), sin(-phi1))
  a = rot_y(a, cos(-phi2), sin(-phi2))
  a = rot_z(a, cos(-phi3), sin(-phi3))

  axb(i) = axb(i) + a(1)
  ayb(i) = ayb(i) + a(2)
  azb(i) = azb(i) + a(3)

  tmp = mass(i)/mass(1)
  axb(1) = axb(1) - tmp*a(1)
  ayb(1) = ayb(1) - tmp*a(2)
  azb(1) = azb(1) - tmp*a(3)

enddo

if (debug_swift) then
  call cpu_time(t2)
!  write(*,*) '# cpu_time = ', t2-t1, ' s'
endif

return
end subroutine getacc_mp2

end module getacc_mp2_module


