! spath.f90
! Shadow path.
! Miroslav Broz (miroslav.broz@email.cz), Jun 28th 2022

module spath_module

double precision, parameter, public :: NAN = -999.d0

contains

subroutine spath(t_nolite, lite, r_EA, r_AO, e, axes, ecl, ecb, silh_, has_solution)

use const_module
use read_bruteforce_module
use read_face_module
use read_node_module
use rotate_module
use uvw_nodes_module
use occult_module
use silhouette_module

implicit none

include '../simplex/simplex.inc'
include '../simplex/dependent.inc'

double precision, intent(in) :: t_nolite, lite
double precision, dimension(3), intent(in) :: r_EA, r_AO
double precision, dimension(3), intent(out) :: e
double precision, dimension(3), intent(in) :: axes
double precision, intent(in) :: ecl, ecb
double precision, dimension(:,:), pointer, intent(out) :: silh_
logical, intent(out) :: has_solution

! bruteforce.in
character(len=255) :: f_elem, f_face, f_node
double precision, save :: rho, unit, P_rot_, Tmin, pole_l_, pole_b_, phi0_

integer, dimension(:,:), pointer, save :: faces
double precision, dimension(:,:), pointer, save :: nodes, nodes_
double precision, dimension(:,:), pointer, save :: silh
logical, dimension(:), pointer, save :: masks

integer, save :: i1st = 0
integer :: j
double precision :: t_lite
double precision :: phi1, phi2, phi3
double precision :: d, ra, de, ra0, de0
double precision :: u, v
double precision, dimension(3) :: r_limb
double precision :: lambda, phi

if (i1st.eq.0) then
!
! read shape model
!
  call read_bruteforce('bruteforce.in', f_elem, f_face, f_node, rho, unit, P_rot_, Tmin, pole_l_, pole_b_, phi0_)

  call read_face(f_face, faces)
  call read_node(f_node, nodes)

  allocate(masks(size(faces,1)))
  allocate(silh(nsilh,2))
  allocate(silh_(nsilh,2))
  allocate(nodes_(size(nodes,1),3))

  nodes = nodes*unit/au

! no shadow of a shadow...
  masks = .True.

  i1st = 1
endif

! from dependent.inc
pole_l_ = pole_l(1)
pole_b_ = pole_b(1)
phi0_ = phi0(1)
P_rot_ = P_rot(1)

! axis rotation
nodes_ = nodes
t_lite = t_nolite + lite
phi1 = 2.d0*pi*(t_lite-Tmin)/P_rot_ + phi0_
call rot_z_nodes(nodes_, phi1)

! pole direction
phi2 = pi/2.d0-pole_b_
phi3 = pole_l_
call rot_y_nodes(nodes_, phi2)
call rot_z_nodes(nodes_, phi3)

! sky-plane projection
call uvw_nodes(t_lite, ecl, ecb, nodes_)

call silhouette(nodes_, faces, masks, silh)

d = sqrt(dot_product(r_EA, r_EA))
ra0 = atan2(r_EA(2), r_EA(1))
de0 = asin(r_EA(3)/d)

! sihouette -> shadow
do j = 1, size(silh,1)

  u = silh(j, 1)/d
  v = silh(j, 2)/d

  ! u .. -RA
  ! v .. +DE
  ra = ra0 - u 
  de = de0 + v
  r_limb = (/cos(ra)*cos(de), sin(ra)*cos(de), sin(de)/)
  r_limb = d*r_limb

  call occult(t_nolite, r_limb, r_AO, e, axes, lambda, phi, has_solution)

  if (has_solution) then
    silh_(j, 1) = lambda
    silh_(j, 2) = phi
  else
    silh_(j, :) = NAN
  endif
enddo

end subroutine spath

end module spath_module


