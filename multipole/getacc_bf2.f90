! getacc_bf2.f90
! Brute-force accelerations (to be added).
! Miroslav Broz (miroslav.broz@email.cz), May 26th 2016

module getacc_bf2_module

contains

subroutine getacc_bf2(time,nbod_,mass,xb,yb,zb,axb,ayb,azb)

use bruteforce_module
use rotate_module
use volume_module
use const_module
use centre_module
use read_bruteforce_module
use read_elem_module
use read_node_module

implicit none

include '../chi2/chi2.inc'
include '../chi2/dependent.inc'

integer :: nbod_
double precision, dimension(nbod_) :: mass, xb, yb, zb
double precision, dimension(nbod_) :: axb, ayb, azb
double precision :: time

character(len=255) :: f_elem, f_face, f_node
double precision, save :: capm, unit, P_rot_, Tmin, pole_l_, pole_b_, phi0_

integer, save :: i1st
data i1st /0/

integer, dimension(:,:), pointer :: elems, faces
double precision, dimension(:,:), pointer :: nodes
double precision, dimension(:), pointer, save :: vols
double precision, dimension(:,:), pointer, save :: coms
double precision, save :: V

integer :: i, l, ierr
double precision :: rho, capr, phi1, phi2, phi3, tmp
double precision :: absr, theta, phi
double precision, dimension(3) :: r, r_, a, monopole
double precision :: time_
real :: t1, t2

if (.not.use_bruteforce) return

if (i1st.eq.0) then

  ! read parameters
  call read_bruteforce('bruteforce.in', f_elem, f_face, f_node, capm, unit, P_rot_, Tmin, pole_l_, pole_b_, phi0_)

  ! input files
  call read_elem(f_elem, elems)
  call read_node(f_node, nodes)

  write(*,*) '# nelems = ', size(elems,1)
  write(*,*) '# nnodes = ', size(nodes,1)

  allocate(vols(size(elems,1)))
  allocate(coms(size(elems,1),3))

  ! unit conversion
  nodes = nodes*unit

  ! volume
  V = volume(elems, nodes, vols)
  capr = (V/(4.d0/3.d0*pi))**(1.d0/3.d0)
  rho = capm/V

  write(*,*) '# M = ', capm, ' kg'
  write(*,*) '# V = ', V, ' m^3'
  write(*,*) '# R = ', capr, ' m = ', capr/unit, ' [unit]'
  write(*,*) '# rho = ', rho, ' kg m^-3'

  ! centre of mass
  call centre(elems, nodes, coms)

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

  ! brute-force acceleration
  rho = mass(1)/GM_S*M_S/V
  a = a_g_bf(vols, coms, rho, r*au)
  a = a/(au/day**2)
!  write(*,*) 'a_g_bf = ', a, ' au day^-2'

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
end subroutine getacc_bf2

end module getacc_bf2_module


