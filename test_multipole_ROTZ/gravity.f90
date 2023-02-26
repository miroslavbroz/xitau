! gravity.f90
! Compute gravitational moments for tetrehedral mesh
! Miroslav Broz (miroslav.broz@email.cz), Nov 13th 2019

program gravity

! modules
use read_elem_module
use read_face_module
use read_node_module
use volume_module
use surface_module
use dipole_module
use inertia_module
use multipole_module
use bruteforce_module
use const_module
use centre_module
use rotate_module
use jacobi_module
use write_multipole_module
use write_bruteforce_module
use write_elem_module
use write_face_module
use write_node_module
use srtidx_module

implicit none

! constants
integer, parameter :: npole = 10
integer, parameter :: neval = 1000

! variables
character(len=255) :: f_elem, f_face, f_node
double precision :: rho, unit, P, Tmin, pole_l, pole_b, phi0
double precision, dimension(3) :: r
integer :: maxl

integer :: l, m, j, k
integer :: nrot
double precision :: V, S, capr, capm, tmp, capr_, rho_, capm_, vkepl, vesc
double precision :: A, B, C, D, E, F
double precision :: C10, C11, S11
double precision :: J2, C20, C22, C21, S21, S22
double precision :: U, absr
double precision :: alpha, beta, gamma, absL
real :: t1, t2
double precision, dimension(3) :: T, a_g
double precision, dimension(6) :: I
double precision, dimension(3) :: omega, L_, elambda, elambda_, r1
double precision, dimension(3,3) :: I_, evector, evector_, Itmp
double precision, dimension(0:npole,0:npole) :: Clm, Slm
integer, dimension(3) :: id
integer, dimension(:,:), pointer :: elems, faces
double precision, dimension(:,:), pointer :: nodes
double precision, dimension(:), pointer :: vols, surf
double precision, dimension(:,:), pointer :: coms

! read parameters
read(*,*) f_elem
read(*,*) f_face
read(*,*) f_node
read(*,*) rho
read(*,*) r(1), r(2), r(3)
read(*,*) unit
read(*,*) P
read(*,*) Tmin
read(*,*) pole_l
read(*,*) pole_b
read(*,*) phi0
read(*,*) maxl

! unit conversion
r = r*unit
P = P*day
pole_l = pole_l*deg
pole_b = pole_b*deg
phi0 = phi0*deg

! write parameters
write(*,*) 'f_elem = ', trim(f_elem)
write(*,*) 'f_face = ', trim(f_face)
write(*,*) 'f_node = ', trim(f_node)
write(*,*) 'rho = ', rho, ' kg m^-3'
write(*,*) 'r = ', r(1), r(2), r(3), ' [unit]'
write(*,*) 'unit = ', unit, ' m'
write(*,*) 'P = ', P/day, ' day'
write(*,*) 'Tmin = ', Tmin, ' JD'
write(*,*) 'pole_l = ', pole_l/deg, ' deg'
write(*,*) 'pole_b = ', pole_b/deg, ' deg'
write(*,*) 'phi0 = ', phi0/deg, ' deg'
write(*,*) 'maxl = ', maxl
write(*,*)

! input files
call read_elem(f_elem, elems)
call read_face(f_face, faces)
call read_node(f_node, nodes)

write(*,*) 'nelems = ', size(elems,1)
write(*,*) 'nfaces = ', size(faces,1)
write(*,*) 'nnodes = ', size(nodes,1)

allocate(vols(size(elems,1)))
allocate(surf(size(faces,1)))
allocate(coms(size(elems,1),3))

! unit conversion
nodes = nodes*unit

! volume
V = volume(elems, nodes, vols)
capr = (V/(4.d0/3.d0*pi))**(1.d0/3.d0)
capm = V * rho
vkepl = sqrt(G*capm/capr)
vesc = sqrt(2.d0)*vkepl

write(*,*) 'V = ', V, ' m^3'
write(*,*) 'R = ', capr, ' m = ', capr/unit, ' [unit]; volume-equivalent'
write(*,*) '2R = ', 2.d0*capr, ' m = ', 2.d0*capr/unit, ' [unit]'
write(*,*) 'M = ', capm, ' kg = ', capm/M_S, ' M_S'
write(*,*) 'vkepl = ', vkepl, ' m s^-1'
write(*,*) 'vesc = ', vesc, ' m s^-1'
write(*,*)

rho_ = 4.64d18/V
capr_ = (4.64d18/rho/(4.d0/3.d0*pi))**(1.d0/3.d0)

!write(*,*) 'M = ', 4.64d18, ' kg (Descamps etal. 2011)'
!write(*,*) 'rho_ = ', rho_, ' kg m^-3'
!write(*,*) 'R_ = ', capr_, ' m = ', capr_/capr, " R"
!write(*,*)

! surface
S = surface(faces, nodes, surf)
capr_ = sqrt(S/(4.d0*pi))

write(*,*) 'S = ', S, ' m^2'
write(*,*) 'R_ = ', capr_, ' m = ', capr_/capr, " R; surface-equivalent"
write(*,*)

! centre of mass
call centre(elems, nodes, coms)
T = dipole(vols, coms) * rho / capm

write(*,*) 'T(1) = ', T(1)
write(*,*) 'T(2) = ', T(2)
write(*,*) 'T(3) = ', T(3)
write(*,*)

! shift to c.o.m.
do j = 1, size(nodes,1)
  nodes(j,:) = nodes(j,:) - T
enddo
call centre(elems, nodes, coms)
T = dipole(vols, coms) * rho / capm

write(*,*) 'shifted to c.o.m.'
write(*,*)
write(*,*) 'T(1) = ', T(1)
write(*,*) 'T(2) = ', T(2)
write(*,*) 'T(3) = ', T(3)
write(*,*)

! moment of inertia
I = inertia(vols, coms) * rho
A = I(1)
B = I(2)
C = I(3)
D = I(4)
E = I(5)
F = I(6)

write(*,*) 'A = ', A, ' kg m^2'
write(*,*) 'B = ', B, ' kg m^2'
write(*,*) 'C = ', C, ' kg m^2'
write(*,*) 'D = Iyz = ', D, ' kg m^2'
write(*,*) 'E = Ixz = ', E, ' kg m^2'
write(*,*) 'F = Ixy = ', F, ' kg m^2'
write(*,*)

! angular momentum
I_(1,:) = [I(1),I(6),I(5)]
I_(2,:) = [I(6),I(2),I(4)]
I_(3,:) = [I(5),I(4),I(3)]
omega = [0.d0,0.d0,2.d0*pi/P]
L_ = matmul(I_,omega)
absL = sqrt(dot_product(L_, L_))
alpha = acos(dot_product(L_, [1.d0, 0.d0, 0.d0]) / absL)
beta  = acos(dot_product(L_, [0.d0, 1.d0, 0.d0]) / absL)
gamma = acos(dot_product(L_, [0.d0, 0.d0, 1.d0]) / absL)

write(*,*) 'I(1) = ', I_(1,:)
write(*,*) 'I(2) = ', I_(2,:)
write(*,*) 'I(3) = ', I_(3,:)
write(*,*) 'omega = ', omega
write(*,*) 'L = ', L_
write(*,*) 'alpha = ', alpha/deg, ' deg'
write(*,*) 'beta  = ', beta/deg, ' deg'
write(*,*) 'gamma = ', gamma/deg, ' deg'
write(*,*)

! eigenvalues & eigenvectors
Itmp = I_
call jacobi(Itmp, elambda, evector, nrot)

write(*,*) 'elambda = ', elambda
write(*,*) 'evector(1) = ', evector(1,:)
write(*,*) 'evector(2) = ', evector(2,:)
write(*,*) 'evector(3) = ', evector(3,:)

! Note: In jacobi, eigenvectors are already transposed (in columns)!
! For this check (A \times v = lambda v), non-transposed are needed.
evector_ = transpose(evector)

write(*,*) 'A \times v = lambda v:'
write(*,*) matmul(I_, evector_(1,:))
write(*,*) elambda(1)*evector_(1,:)
write(*,*) matmul(I_, evector_(2,:))
write(*,*) elambda(2)*evector_(2,:)
write(*,*) matmul(I_, evector_(3,:))
write(*,*) elambda(3)*evector_(3,:)

call srtidx(elambda, id)
do j = 1, 3
  elambda_(j) = elambda(id(j))
  evector_(j,:) = evector(id(j),:)
enddo

alpha = acos(dot_product(evector_(1,:), [1.d0, 0.d0, 0.d0]))
beta  = acos(dot_product(evector_(2,:), [0.d0, 1.d0, 0.d0]))
gamma = acos(dot_product(evector_(3,:), [0.d0, 0.d0, 1.d0]))
phi0 = phi0 + alpha
write(*,*) 'alpha = ', alpha/deg, ' deg'
write(*,*) 'beta  = ', beta/deg, ' deg'
write(*,*) 'gamma = ', gamma/deg, ' deg'
write(*,*) 'phi0 = ', phi0/deg, ' deg'
write(*,*)

! rotate to align w. moment of inertia; a simpler vers. too keep orientation of AO!
call rot_z_nodes(nodes, -alpha)

! incl. the density profile!
!r0 = rot_z(r0, cos(-alpha), sin(-alpha))

write(*,*) 'rotated to m.o.i. (about \hat z), wo. density profile'
write(*,*)

! again to verify (multiple times!)
! Note: For an ellipsoid, only 1 transformation is needed.
do k = 1, 3

  call centre(elems, nodes, coms)
  I = inertia(vols, coms) * rho
  A = I(1)
  B = I(2)
  C = I(3)
  D = I(4)
  E = I(5)
  F = I(6)

  I_(1,:) = [I(1),I(6),I(5)]
  I_(2,:) = [I(6),I(2),I(4)]
  I_(3,:) = [I(5),I(4),I(3)]

  Itmp = I_
  call jacobi(Itmp, elambda, evector, nrot)

  do j = 1, 3
    elambda_(j) = elambda(id(j))
    evector_(j,:) = evector(id(j),:)
  enddo

  write(*,*) 'elambda_ = ', elambda_
  write(*,*) 'evector_(1) = ', evector_(1,:)
  write(*,*) 'evector_(2) = ', evector_(2,:)
  write(*,*) 'evector_(3) = ', evector_(3,:)

  alpha = acos(dot_product(evector_(1,:), [1.d0, 0.d0, 0.d0]))
  beta  = acos(dot_product(evector_(2,:), [0.d0, 1.d0, 0.d0]))
  gamma = acos(dot_product(evector_(3,:), [0.d0, 0.d0, 1.d0]))

  write(*,*) 'alpha = ', alpha/deg, ' deg'
  write(*,*) 'beta  = ', beta/deg, ' deg'
  write(*,*) 'gamma = ', gamma/deg, ' deg'
  write(*,*)

  call rotate_diagonal(nodes, evector_)

  write(*,*) 'again to verify'
  write(*,*)

enddo  ! k

! relation to multipoles (see Sidlichovsky notes)
tmp = 1.d0/capr
C10 = T(3)*tmp
C11 = T(1)*tmp
S11 = T(2)*tmp

tmp = 1.d0/(capm*capr**2)
C20 = (1.d0/2.d0*(A+B)-C)*tmp
C22 = 1.d0/4.d0*(B-A)*tmp
C21 = E*tmp
S21 = D*tmp
S22 = 1.d0/2.d0*F*tmp

J2 = -C20

! high-order multipoles
call multipole(elems, nodes, capm, capr, rho, Clm, Slm)

write(*,*) 'C10 = z0/R = ', C10
write(*,*) 'Clm(1,0) = ', Clm(1,0)
write(*,*) C10/Clm(1,0), ' (may be invalid, if C10 << 1!)'
write(*,*)
write(*,*) 'C11 = x0/R = ', C11
write(*,*) 'Clm(1,1) = ', Clm(1,1)
write(*,*) C11/Clm(1,1)
write(*,*)
write(*,*) 'S11 = y0/R = ', S11
write(*,*) 'Slm(1,1) = ', Slm(1,1)
write(*,*) S11/Slm(1,1)
write(*,*)
write(*,*) 'C20 = ', C20, ' i.e., oblateness'
write(*,*) 'Clm(2,0) = ', Clm(2,0)
write(*,*) (C20/Clm(2,0))**2
write(*,*)
write(*,*) 'C21 = ', C21
write(*,*) 'Clm(2,1) = ', Clm(2,1)
write(*,*) (C21/Clm(2,1))
write(*,*)
write(*,*) 'S21 = ', S21
write(*,*) 'Slm(2,1) = ', Slm(2,1)
write(*,*) (S21/Slm(2,1))
write(*,*)
write(*,*) 'C22 = ', C22, ' i.e., triaxiality'
write(*,*) 'Clm(2,2) = ', Clm(2,2)
write(*,*) (C22/Clm(2,2))
write(*,*)
write(*,*) 'S22 = ', S22
write(*,*) 'Slm(2,2) = ', Slm(2,2)
write(*,*) (S22/Slm(2,2))
write(*,*)
write(*,*) 'J2 = -C20 = ', J2
write(*,*)

do l = 0, size(Clm,1)-1
  do m = 0, l
    write(*,*) 'Clm(', l, ',', m, ') = ', Clm(l,m)
    if (m.gt.0) then
      write(*,*) 'Slm(', l, ',', m, ') = ', Slm(l,m)
    endif
  enddo
  write(*,*)
enddo

call write_multipole('multipole.in', Clm, Slm, capm, capr, P, Tmin, pole_l, pole_b, phi0, maxl)

! write transformed mesh
nodes = nodes/unit
call write_elem('output.ele', elems)
call write_face('output.face', faces)
call write_node('output.node', nodes)

call write_bruteforce('bruteforce.in', 'output.ele', 'output.face', 'output.node', capm, unit, P, Tmin, pole_l, pole_b, phi0)

! potential
call cpu_time(t1)
absr = sqrt(dot_product(r, r))
do j = 1, neval
  U = -G*capm/absr
enddo
call cpu_time(t2)
write(*,*) 'U = ', U, ' J kg^-1 (point mass); cpu_time = ', t2-t1, ' s'

call cpu_time(t1)
do j = 1, neval
  U = potential_bf(vols, coms, rho, r)
enddo
call cpu_time(t2)
write(*,*) 'U = ', U, ' J kg^-1 (brute force); cpu_time = ', t2-t1, ' s'
write(*,*)

do l = 0, npole
  call cpu_time(t1)
  do j = 1, neval
    U = potential_mp(Clm, Slm, l, capm, capr, r)
  enddo
  call cpu_time(t2)
  write(*,*) 'U = ', U, ' J kg^-1 (multipole, ', l, '); cpu_time = ', t2-t1, ' s'
enddo
write(*,*)

! acceleration
call cpu_time(t1)
tmp = dot_product(r, r)
do j = 1, neval
  a_g = -G*capm/(tmp*sqrt(tmp))*r
enddo
call cpu_time(t2)
write(*,*) 'a_g = ', a_g, ' m s^-2 (point mass); cpu_time = ', t2-t1, ' s'

call cpu_time(t1)
do j = 1, neval
  a_g = a_g_bf(vols, coms, rho, r)
enddo
call cpu_time(t2)
write(*,*) 'a_g = ', a_g, ' m s^-2 (brute force); cpu_time = ', t2-t1, ' s'
write(*,*)

do l = 0, npole
  call cpu_time(t1)
  do j = 1, neval
    a_g = a_g_mp(Clm, Slm, l, capm, capr, r)
  enddo
  call cpu_time(t2)
  write(*,*) 'a_g = ', a_g, ' m s^-2 (multipole, ', l, '); cpu_time = ', t2-t1, ' s'
enddo
write(*,*)

stop
end


