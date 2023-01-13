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
! f              .. bi-directional distribution function, sr^-1
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
! polys1         .. sets of polygons, derived from triangles, m
! polys2         .. sets of polygons, transformed to line-of-sun
! polys3         .. sets of polygons, clipped (shadowing)
! polys4         .. sets of polygons, transformed to line-of-sight
! polys5         .. sets of polygons, clipped (visibility)
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

use polytype_module

type(polystype), dimension(:), pointer, save :: polys1, polys2, polys3, polys4, polys5, polystmp
double precision, dimension(:), pointer, save :: mu_i, mu_e, f, f_L, Phi_i, Phi_e
double precision, dimension(:,:), pointer, save :: normals, centres
double precision, dimension(3) :: photocentre

contains

subroutine lc_polygon1(t_lite, lite, r, s, o, d1, d2, lambda_eff, Delta_eff, Phi_lambda_cal, V0, i2nd)

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
use xyz_module
use clip_module
use to_three_module
use surface_module
use planck_module
use hapke_module
use normal_of_p_module
use centre_of_p_module
use shadowing_of_p_module
use rotate_of_p_module

use normalize_module
use revert_module

include '../chi2/chi2.inc'
include '../chi2/dependent.inc'

double precision, intent(in) :: t_lite, lite
double precision, dimension(:,:), intent(in) :: r
double precision, dimension(3), intent(in) :: s, o
double precision, intent(in) :: d1, d2
double precision, intent(in) :: lambda_eff, Delta_eff, Phi_lambda_cal
double precision, intent(out) :: V0
integer, intent(inout) :: i2nd

integer, dimension(:,:), pointer, save :: faces
double precision, dimension(:,:), pointer, save :: nodes, orignodes

double precision, dimension(:), pointer, save :: surf
double precision, dimension(:), pointer, save :: I_lambda
integer, dimension(:), pointer, save :: clips

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

!
! initialisation
!

if (i1st.eq.0) then

  ! input parameters
  call read_input()

  ! read 2 objects...
!  call read_face(f_face1, faces1)
!  call read_node(f_node1, nodes1)
  call read_face(f_face2, faces2)
  call read_node(f_node2, nodes2)

  ! units
  nodes2 = unit2*nodes2

  ! from dependent.inc
  allocate(nodes1(size(nodesforchi,1),size(nodesforchi,2)))
  allocate(faces1(size(facesforchi,1),size(facesforchi,2)))

  allocate(nodes(size(nodes1,1)+size(nodes2,1), size(nodes1,2))) 
  allocate(faces(size(faces1,1)+size(faces2,1), size(faces1,2))) 
  allocate(dataset(size(nodes,1)))
  allocate(dataset_(size(faces,1)))

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
  allocate(polystmp(size(polys1,1)))
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
  allocate(clips(size(polys1,1)))

  i1st = 1
endif  ! i1st

!
! 2nd initialisation, whenever the shape is modified
!

if (i2nd.eq.0) then

  nodes1 = nodesforchi
  faces1 = facesforchi

  ! units
  nodes1 = unit1*nodes1

  ! ...and merge them
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

  orignodes = nodes

endif  ! i2nd

call cpu_time(t1)

! from dependent.inc
pole_l_ = pole_l(1:2)
pole_b_ = pole_b(1:2)
phi0_ = phi0(1:2)
P_rot_ = P_rot(1:2)
R_body = R_star(1:2)
A_w = albedo(1:2)
B0 = scattering(1)
minh = scattering(2)
ming = scattering(3)
bartheta = max(scattering(4),0.d0)

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
reflectance = 1.d0 + spectral_slope*(lambda_eff/1.d-6 - 0.55d0)

do i = 1, size(f_L,1)
  j = dataset_(i)
  f_L(i) = reflectance*A_w(j)/(4.d0*pi)
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
  write(*,*) 'lite = ', lite, ' d'
  write(*,*) 'd1 = ', d1/au, ' au'
  write(*,*) 'Phi_lambda = ', Phi_lambda, ' W m^-2 m^-1'
  write(*,*) 'Phi_V = ', Phi_V, ' W m^-2'
  write(*,*) 'reflectance = ', reflectance
  write(*,*) 'f_L = ', f_L(1), ' sr^-1'
  write(*,*) 'A_w  = ', A_w(1)
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
  phi1(i) = 2.d0*pi*(t_lite-Tmin(j))/P_rot_(j) + phi0_(j)
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

! non-illuminated && non-visible won't be computed
call non(mu_i, mu_e, polys1)

! 1st transformation
call uvw(s, polys1, polys2)
call uvw_(nodes)
call uvw_(normals)
call uvw_(centres)

s_ = (/dot_product(hatu,s), dot_product(hatv,s), dot_product(hatw,s)/)
o_ = (/dot_product(hatu,o), dot_product(hatv,o), dot_product(hatw,o)/)

! shadowing
clips = 0
call clip(polys2, polys3, clips)

! back-projecion
call to_three(normals, centres, polys3)

! non-illuminated || non-visible won't be computed
call non_(mu_i, mu_e, polys3)

! back-transformation
call xyz(polys3, polystmp)
call xyz_(nodes)
call xyz_(normals)
call xyz_(centres)

! 2nd transformation
call uvw(o, polystmp, polys4, equatorial=.true.)
call uvw_(nodes)
call uvw_(normals)
call uvw_(centres)

o__ = (/dot_product(hatu,o), dot_product(hatv,o), dot_product(hatw,o)/)
s__ = (/dot_product(hatu,s), dot_product(hatv,s), dot_product(hatw,s)/)

! shadowing
call clip(polys4, polys5, clips)

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

! photocentre
call centre(polys5, centres)
photocentre = 0.d0
do i = 1, size(surf,1)
  if (dataset_(i).ne.1) exit
  if (polys5(i)%c.eq.0) cycle
  photocentre = photocentre + centres(i,:)*Phi_e(i)*surf(i)
enddo
photocentre = photocentre/tot

call cpu_time(t2)

! debugging
if (debug_polygon) then
  no = no+1
  write(*,*) 'no = ', no
  write(*,*) 'cpu_time = ', t2-t1, ' s'  ! dbg

  if ((no.eq.1).or.(no.eq.49).or.(no.eq.50)) then
!  if ((no.ge.1).and.(no.le.99)) then
    write(str,'(i0.2)') no
    call write_node("output.node." // trim(str), nodes)
    call write_face("output.face." // trim(str), faces)
    call write_node("output.normal." // trim(str), normals)
    call write_node("output.centre." // trim(str), centres)

    nodes = nodes/d2/arcsec
    call write_node("output.arcsec." // trim(str), nodes)

    call write_poly("output.poly1." // trim(str), polys1)
    call write_poly("output.poly2." // trim(str), polys2)
    call write_poly("output.poly3." // trim(str), polys3)
    call write_poly("output.poly4." // trim(str), polys4)
    call write_poly("output.poly5." // trim(str), polys5)
    call write_poly("output.polytmp." // trim(str), polystmp)

    call write1("output.f." // trim(str), f)
    call write1("output.f_L." // trim(str), f_L)
    call write1("output.mu_i." // trim(str), mu_i)
    call write1("output.mu_e." // trim(str), mu_e)
    call write1("output.surf." // trim(str), surf)
    call write1("output.Phi_i." // trim(str), Phi_i)
    call write1("output.Phi_e." // trim(str), Phi_e)
    call write1("output.I_lambda." // trim(str), I_lambda)
    call write1("output.clip." // trim(str), dble(clips))

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

  open(unit=10, file='photocentre.dat', status='unknown',access='append')
  if (no.eq.1) then
    write(10,*) '# JD(TDB,nolite) u v w no'
    write(10,*) '# day arcsec arcsec arcsec 1'
  endif
  write(10,*) t_lite-lite, photocentre/d2/arcsec, no
  close(10)

endif

!stop  ! dbg
return
end subroutine lc_polygon1

end module lc_polygon1_module


