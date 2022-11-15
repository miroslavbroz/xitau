! lc_polygon1.f90
! Compute 1 lightcurve point for a general polygonal mesh.
! Miroslav Broz (miroslav.broz@email.cz), Nov 8th 2022

! Notation:
!
! I_lambda       .. monochromatic intensity (ces. intenzita), W m^-2 sr^-1 m^-1
! I2_lambda      .. monochromatic intensity, scattered
! B_lambda       .. Planck monochromatic intensity
! Phi_lambda     .. monochromatic flux, W m^-2 m^-1
! Phi_lambda_cal .. monochromatic flux, calibration
! Phi_nu_cal     .. monochromatic flux, calibration, W m^-2 Hz^-1
! Phi_i          .. monochromatic flux, incoming
! Phi_e          .. monochromatic flux, outgoing
! Phi_V          .. passband flux, total, outgoing, W m^-2
! Phi_V_cal      .. passband flux, calibration
! J_lambda       .. monochromatic luminosity (ces. zarivost), W sr^-1 m^-1
! P_lambda       .. monochromatic power, W m^-1
! P_V            .. passband power, W
! V0             .. brightness, mag
! mu_i           .. directional cosine, incoming, cos(theta)
! mu_e           .. directional cosine, outgoing
! alpha          .. phase angle, sun-target-observer
! omega          .. solid angle, sr
! lambda_eff     .. effective wavelength, m
! Delta_eff      .. effective pasband, m
! f              .. bi-directional distribution function, 1
! f_L            .. Lambert law
! f_g            .. geometric law
! f_LS           .. Lommel-Seeliger law
! A_w            .. single-scattering albedo, 1
! A_hL           .. hemispherical albedo, Lambert
! A_gL           .. geometric albedo, Lambert
! A_BL           .. Bond albedo, Lambert

! nodes          .. nodes, m
! orignodes      .. nodes, original (to be scaled, rotated, translated), m
! faces          .. triangular faces, 1
! elems          .. tetrahedral elements, 1
! poly1          .. sets of polygons, derived from triangles, m
! poly2          .. sets of polygons, transformed to line-of-sun
! poly3          .. sets of polygons, clipped (shadowing)
! poly4          .. sets of polygons, transformed to line-of-sight
! poly5          .. sets of polygons, clipped (visibility)
! normals        .. normals of polygons, 1
! centres        .. centres of polygons, m
! surf           .. surface of polygons, m^2
! vols           .. volumes of tetrahedra, m^3
! capR           .. radius, volume-equivalent, m
! capS           .. surface area, m^2
! capV           .. volume, m^3
! s              .. target->sun unitvector
! o              .. target->observer unitvector
! s_             .. ditto, 1st transformation
! o_             .. ditto, 1st transformation
! s__            .. ditto, 2nd transformation
! o__            .. ditto, 2nd transformation
! d1             .. target-sun distance, m
! d2             .. target-observer distance, m
! phi1           .. z-rotation
! phi2           .. x-rotation
! phi3           .. z-rotation
! dataset        .. nodes identification
! dataset_       .. faces identification

module lc_polygon1_module

contains

subroutine lc_polygon1(time, r, s, o, d1, d2, lambda_eff, Delta_eff, Phi_lambda_cal, V0)

use polytype_module
use const_module
use input_module
use read_input_module
use read_face_module
use read_node_module
use write_node_module
use write_face_module
use write_poly_module
use write1_module
use to_poly_module
use uvw_module
use clip_module
use to_three_module
use surface_module
use planck_module
use hapke_module
use normal_of_p_module
use centre_of_p_module
use shadowing_of_p_module
use rotate_of_p_module

include '../simplex/simplex.inc'
include '../simplex/dependent.inc'

double precision, intent(in) :: time
double precision, dimension(:,:), intent(in) :: r
double precision, dimension(3), intent(in) :: s, o
double precision, intent(in) :: d1, d2
double precision, intent(in) :: lambda_eff, Delta_eff, Phi_lambda_cal
double precision, intent(out) :: V0

integer, dimension(:,:), pointer, save :: faces
double precision, dimension(:,:), pointer, save :: nodes, orignodes
type(polystype), dimension(:), pointer, save :: polys1, polys2, polys3, polys4, polys5

double precision, dimension(:,:), pointer, save :: normals, centres
double precision, dimension(:), pointer, save :: surf
double precision, dimension(:), pointer, save :: mu_i, mu_e, f, f_L, Phi_i, Phi_e
double precision, dimension(:), pointer, save :: I_lambda

! internal variables
integer :: i, j, k
double precision :: capR, capS, capV
double precision :: A_hL, A_gL, A_BL
double precision :: alpha, omega
double precision :: B_lambda, J_lambda, P_lambda, P_V, Phi_V
double precision :: B_thermal, Phi_thermal
double precision :: tot, tmp
double precision :: t1, t2
character(len=80) :: str

integer, dimension(:), pointer, save :: dataset, dataset_
integer, dimension(:,:), pointer, save :: faces1, faces2
double precision, dimension(:,:), pointer, save :: nodes1, nodes2
double precision, dimension(:), pointer, save :: phi1, phi2, phi3

integer, save :: i1st = 0, no = 0

if (i1st.eq.0) then

  ! input parameters
  call read_input()

  ! read 2 objects...
  call read_face(f_face1, faces1)
  call read_node(f_node1, nodes1)
  call read_face(f_face2, faces2)
  call read_node(f_node2, nodes2)

  ! ...and merge them
  allocate(nodes(size(nodes1,1)+size(nodes2,1), size(nodes1,2))) 
  allocate(faces(size(faces1,1)+size(faces2,1), size(faces1,2))) 
  allocate(dataset(size(nodes,1)))
  allocate(dataset_(size(faces,1)))

  ! units
  nodes1 = unit1*nodes1
  nodes2 = unit2*nodes2

  do j = 1, size(nodes1,1)
    nodes(j,:) = nodes1(j,:)
    dataset(j) = 1
  enddo
  do j = 1, size(nodes2,1)
    k = j+size(nodes1,1)
    nodes(k,:) = nodes2(j,:)
    dataset(k) = 2
  enddo
  do j = 1, size(faces1,1)
    faces(j,:) = faces1(j,:)
    dataset_(j) = 1
  enddo
  do j = 1, size(faces2,1)
    k = j+size(faces1,1)
    faces(k,:) = faces2(j,:) + size(nodes1,1)
    dataset_(k) = 2
  enddo

  ! allocation
  allocate(orignodes(size(nodes,1), size(nodes,2)))
  allocate(phi1(size(orignodes,1)))
  allocate(phi2(size(orignodes,1)))
  allocate(phi3(size(orignodes,1)))

  allocate(polys1(size(faces,1)))
  allocate(polys2(size(polys1,1)))
  allocate(polys3(size(polys1,1)))
  allocate(polys4(size(polys1,1)))
  allocate(polys5(size(polys1,1)))
  allocate(normals(size(polys1,1),3))
  allocate(centres(size(polys1,1),3))
  allocate(mu_i(size(polys1,1)))
  allocate(mu_e(size(polys1,1)))
  allocate(surf(size(polys1,1)))
  allocate(f(size(polys1,1)))
  allocate(f_L(size(polys1,1)))
  allocate(Phi_i(size(polys1,1)))
  allocate(Phi_e(size(polys1,1)))
  allocate(I_lambda(size(polys1,1)))

  orignodes = nodes

  i1st = 1
endif  ! i1st

call cpu_time(t1)

! from dependent.inc
pole_l_ = pole_l(1:2)
pole_b_ = pole_b(1:2)
phi0_ = phi0(1:2)
P_rot_ = P_rot(1:2)
R_body = R_star(1:2)
A_w = albedo(1:2)

! stellar surface
B_lambda = planck(T_star, lambda_eff)
Phi_lambda = pi*B_lambda                ! over omega, half-space, cosine
J_lambda = pi*R_S**2 * B_lambda         ! over S, visible, cosine
P_lambda = 4.d0*pi * J_lambda           ! over omega, full-space
P_lambda = 4.d0*pi*R_S**2 * Phi_lambda  ! over S
P_V = Delta_eff*P_lambda                 ! over lambda

if (debug_polygon) then
  write(*,*) '# at stellar surface:'
  write(*,*) 'T_star = ', T_star, ' K'
  write(*,*) 'lambda_eff = ', lambda_eff, ' m'
  write(*,*) 'Delta_eff = ', Delta_eff, ' m'
  write(*,*) 'B_lambda = ', B_lambda, ' W m^-2 sr^-1 m^-1'
  write(*,*) 'Phi_lambda = ', Phi_lambda, ' W m^-2 m^-1'
  write(*,*) 'J_lambda = ', J_lambda, ' W sr^-1 m^-1'
  write(*,*) 'P_lambda = ', P_lambda, ' W m^-1'
  write(*,*) 'P_V = ', P_V, ' W'
  write(*,*) ''
endif

! asteroid surface
Phi_lambda = P_lambda/(4.d0*pi*d1**2)
Phi_V = Phi_lambda*Delta_eff

do i = 1, size(f_L,1)
  j = dataset_(i)
  f_L(i) = A_w(j)/(4.d0*pi)
enddo

A_hL = pi*f_L(1)
A_gL = 2.d0/3.d0*pi*f_L(1)
A_BL = pi*f_L(1)

alpha = acos(dot_product(s,o))
call init_hapke(alpha)

B_thermal = planck(T_eq, lambda_eff)
Phi_thermal = pi*B_thermal

if (debug_polygon) then
  write(*,*) '# at asteroid surface:'
  write(*,*) 'd1 = ', d1/au, ' au'
  write(*,*) 'Phi_lambda = ', Phi_lambda, ' W m^-2 m^-1'
  write(*,*) 'Phi_V = ', Phi_V, ' W m^-2'
  write(*,*) 'f_L = ', f_L(1), ' sr^-1'
  write(*,*) 'A_hL = ', A_hL
  write(*,*) 'A_gL = ', A_gL
  write(*,*) 'A_BL = ', A_BL
  write(*,*) 'alpha = ', alpha/deg, ' deg'
  write(*,*) ''
  write(*,*) 'T_eq = ', T_eq, ' K'
  write(*,*) 'B_thermal = ', B_thermal, ' W m^-2 sr^-1 m^-1'
  write(*,*) 'Phi_thermal = ', Phi_thermal, ' W m^-2 m^-1'
  write(*,*) ''
endif

! observer location
omega = 1.d0/(d2**2)  ! sr

! calibration
Phi_nu_cal = Phi_lambda_cal*lambda_eff**2/clight
Phi_V_cal = Delta_eff*Phi_lambda_cal

if (debug_polygon) then
  write(*,*) '# at observer location:'
  write(*,*) 'd2 = ', d2/au, ' au'
  write(*,*) 'omega = ', omega, ' sr'
  write(*,*) 'Phi_nu_cal = ', Phi_nu_cal, ' W m^-2 Hz^-1'
  write(*,*) 'Phi_lambda_cal = ', Phi_lambda_cal, ' W m^-2 m^-1'
  write(*,*) 'Phi_V_cal = ', Phi_V_cal, ' W m^-2'
  write(*,*) ''
endif

! scaling
nodes = orignodes
do i = 1, size(nodes,1)
  j = dataset(i)
  nodes(i,:) = R_body(j)*nodes(i,:)
enddo

! axis rotation
do i = 1, size(nodes,1)
  j = dataset(i)
  phi1(i) = 2.d0*pi*(time-Tmin(j))/P_rot_(j) + phi0_(j)
  phi2(i) = pi/2.d0-pole_b_(j)
  phi3(i) = pole_l_(j)
enddo
call rot_z_nodes(nodes, phi1)

! pole direction
call rot_y_nodes(nodes, phi2)
call rot_z_nodes(nodes, phi3)

! position
do i = 1, size(nodes,1)
  nodes(i,:) = nodes(i,:) + r(dataset(i),:)
enddo

! conversion
call to_poly(faces, nodes, polys1)

! geometry
call normal(polys1, normals)
call centre(polys1, centres)

call mu(normals, s, mu_i)
call mu(normals, o, mu_e)

! non-illuminated || non-visible won't be computed
call non(mu_i, mu_e, polys1)

! 1st transformation
call uvw(s, polys1, polys2)
call uvw_(nodes)
call uvw_(normals)
call uvw_(centres)

s_ = (/dot_product(hatu,s), dot_product(hatv,s), dot_product(hatw,s)/)
o_ = (/dot_product(hatu,o), dot_product(hatv,o), dot_product(hatw,o)/)

! shadowing
call clip(polys2, polys3)

! back-projecion
call to_three(normals, centres, polys3)

! 2nd transformation
call uvw(o_, polys3, polys4, equatorial=.true.)
call uvw_(nodes)
call uvw_(normals)
call uvw_(centres)

o__ = (/dot_product(hatu,o_), dot_product(hatv,o_), dot_product(hatw,o_)/)
s__ = (/dot_product(hatu,s_), dot_product(hatv,s_), dot_product(hatw,s_)/)

! shadowing
call clip(polys4, polys5)

! back-projecion
call to_three(normals, centres, polys5)

! geometry
capS = surface(polys5, normals, surf)

! integration
include 'integrate_over_S.inc'

! lightcurve
Phi_V = Delta_eff*omega*tot
V0 = 0.d0 - 2.5d0*log10(Phi_V/Phi_V_cal)

if (debug_polygon) then
  write(*,*) 'Phi_V = ', Phi_V, ' W m^-2'
  write(*,*) 'V0 = ', V0, ' mag'
endif

call cpu_time(t2)
write(*,*) 'cpu_time = ', t2-t1, ' s'  ! dbg

! debugging
if (debug_polygon) then
  no = no+1
!  if ((no.eq.78).or.(no.eq.79)) then
!  if ((no.eq.1).or.(no.eq.2).or.(no.eq.3)) then
  if ((no.eq.1).or.(no.eq.49).or.(no.eq.50)) then
    write(str,'(i0.2)') no
    call write_node("output.node." // trim(str), nodes)
    call write_face("output.face." // trim(str), faces)
    call write_node("output.normal." // trim(str), normals)
    call write_node("output.centre." // trim(str), centres)

    call write_poly("output.poly1." // trim(str), polys1)
    call write_poly("output.poly2." // trim(str), polys2)
    call write_poly("output.poly3." // trim(str), polys3)
    call write_poly("output.poly4." // trim(str), polys4)
    call write_poly("output.poly5." // trim(str), polys5)

    call write1("output.f." // trim(str), f)
    call write1("output.f_L." // trim(str), f_L)
    call write1("output.mu_i." // trim(str), mu_i)
    call write1("output.Phi_i." // trim(str), Phi_i)
    call write1("output.Phi_e." // trim(str), Phi_e)
    call write1("output.surf." // trim(str), surf)
    call write1("output.I_lambda." // trim(str), I_lambda)

! gnuplotting
    open(unit=10, file='output.gnu', status='unknown')
    write(10,*) 's1 = ', s(1)
    write(10,*) 's2 = ', s(2)
    write(10,*) 's3 = ', s(3)
    write(10,*) 'o1 = ', o(1)
    write(10,*) 'o2 = ', o(2)
    write(10,*) 'o3 = ', o(3)
    write(10,*) 's1_ = ', s_(1)
    write(10,*) 's2_ = ', s_(2)
    write(10,*) 's3_ = ', s_(3)
    write(10,*) 'o1_ = ', o_(1)
    write(10,*) 'o2_ = ', o_(2)
    write(10,*) 'o3_ = ', o_(3)
    write(10,*) 's1__ = ', s__(1)
    write(10,*) 's2__ = ', s__(2)
    write(10,*) 's3__ = ', s__(3)
    write(10,*) 'o1__ = ', o__(1)
    write(10,*) 'o2__ = ', o__(2)
    write(10,*) 'o3__ = ', o__(3)
    close(10)
  endif
endif

!if (no.eq.5) stop  ! dbg

return
end subroutine lc_polygon1

end module lc_polygon1_module


