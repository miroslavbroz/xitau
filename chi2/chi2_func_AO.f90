! chi2_func_AO.f90
! Calculate chi^2 for adaptive-optics data.
! Miroslav Broz (miroslav.broz@email.cz), Aug 20th 2020

module chi2_func_AO_module

contains

subroutine chi2_func_AO(chi2, n)

use const_module
use read_AO_module
use read_face_module
use read_node_module
use read_bruteforce_module
use read_ephemeris_module
use read_pnm_module
use write_pnm_module
use rotate_module
use write_node_module
use write_silh_module
use write_poles_module
use uvw_nodes_module
use normal_module
use shadowing_module
use silhouette_module

implicit none
include 'chi2.inc'
include 'dependent.inc'

! input

! output
double precision, intent(out) :: chi2
integer, intent(out) :: n

! observational data
integer, save :: m_AO = 0
integer, dimension(AOMAX), save :: dataset
double precision, dimension(AOMAX), save :: t_AO, sigma, pixel_scale, vardist, ecl, ecb
character(len=255), dimension(AOMAX), save :: file_OBS

integer, save :: N_s
double precision, dimension(OUTMAX), save :: t_s, vardist_s, ecl_s, ecb_s

! internal variables
character(len=255) :: f_elem, f_face, f_node
double precision, save :: rho, unit, P_rot_, Tmin, pole_l_, pole_b_, phi0_, R_body

integer, dimension(:,:), pointer, save :: faces
double precision, dimension(:,:), pointer, save :: nodes
double precision, dimension(:,:), pointer, save :: nodes_
double precision, dimension(:,:), pointer, save :: normals
double precision, dimension(:,:), pointer, save :: silh, silh_OBS
double precision, dimension(:,:), pointer :: pnm
logical, dimension(:), pointer, save :: masks
double precision, dimension(2) :: c_
double precision, dimension(3) :: n_to, n_ts

integer :: i, k
integer, save :: j, i1st = 0, iu = 20, iub = 25
double precision :: phi1, phi2, phi3, tmp, phase, dx, dy
double precision :: t_interp, lite
double precision :: l, b
double precision :: chi2_
character(len=80) :: str, str_

! functions
double precision, external :: nula2pi, interp, interp2

!
! read adaptive-optics observations (only 1st time!)
!
if (i1st.eq.0) then

  call read_AO(file_AO, m_AO, t_AO, sigma, pixel_scale, vardist, ecl, ecb, dataset, file_OBS)

  if (debug) then
    write(*,*) "# m_AO = ", m_AO
  endif

! read Sun ephemeris
  if (m_AO.gt.0) then
    call read_ephemeris("ephemeris_S.dat", N_s, t_s, vardist_s, ecl_s, ecb_s)

! read shape model
    call read_bruteforce('bruteforce.in', f_elem, f_face, f_node, rho, unit, P_rot_, Tmin, pole_l_, pole_b_, phi0_)

! allocation
    allocate(nodes(size(nodesforchi,1),size(nodesforchi,2)))
    allocate(faces(size(facesforchi,1),size(facesforchi,2)))
    allocate(nodes_(size(nodes,1),3))
    allocate(normals(size(faces,1),3))
    allocate(masks(size(faces,1)))
    allocate(silh(nsilh,2))
    allocate(silh_OBS(nsilh,2))
  endif

  j = 2
  i1st = 1

endif  ! i1st

!
! calculate the chi^2 value (adaptive-optics data)
!

chi2 = 0.d0
n = 0

if (debug) then
  open(unit=iu,file="chi2_AO.dat",status="unknown")
  write(iu,*) "# t_AO+lite & u    [arcsec] & v    [arcsec] & sigma [arcsec] & phase [] & dataset & file & chi^2"
  write(iu,*) "# t_AO      & u_AO [arcsec] & v_AO [arcsec] & sigma [arcsec] & phase [] & dataset & file & chi^2"
endif

do i = 1, m_AO

! light-time effect
  if (use_vardist) then
    lite = -vardist(i)/clight * AU/day 
  else
    lite = 0.d0
  endif
  t_interp = t_AO(i) + lite

! from dependent.inc
  pole_l_ = pole_l(1)
  pole_b_ = pole_b(1)
  phi0_ = phi0(1)
  P_rot_ = P_rot(1)
  R_body = R_star(1)

  nodes = nodesforchi
  faces = facesforchi

! unit conversion
  nodes = nodes*unit/au

! scaling
  nodes_ = R_body*nodes

! axis rotation
  phi1 = 2.d0*pi*(t_interp-Tmin)/P_rot_ + phi0_
  phi2 = pi/2.d0-pole_b_
  phi3 = pole_l_
  phase = nula2pi(phi1)/(2.d0*pi)
  call rot_z_nodes(nodes_, phi1)

! pole direction
  call rot_y_nodes(nodes_, phi2)
  call rot_z_nodes(nodes_, phi3)

! save shape (in ecliptic coordinates; au)
  if (debug_swift) then
    write(str,'(i4.4)') i
    str = 'nodes' // trim(str) // '.ecl'
!    call write_node(str, nodes_)
  endif

! sky-plane projection
  call uvw_nodes(t_interp, ecl(i), ecb(i), nodes_)

  if (use_vardist) then
    tmp = vardist(i)*AU/pc
  else
    tmp = d_pc
  endif
  nodes_ = nodes_*(au/(tmp*pc)/pi*180.d0*3600.d0)  ! au -> arcsec

! save shape (in uvw coordinates; arcsec)
  if (debug_swift) then
    write(str,'(i4.4)') i
    str = 'nodes' // trim(str) // '.dat'
    call write_node(str, nodes_)

    open(unit=iub, file=str, status='old', access='append')
    write(iub,*) '# i [] & u [arcsec] & v [arcsec] & w [arcsec]'
    write(iub,*) '# t_AO = ', t_AO(i), ' JD'
    write(iub,*) '# vardist = ', vardist(i), ' au'
    write(iub,*) '# phase = ', phase
    close(iub)
  endif

! shadowing (of faces)
  do while ((j.gt.2).and.(t_s(j-1).gt.t_interp))
    j = j-1
  enddo
  do while ((j.lt.N_s).and.(t_s(j).le.t_interp))
    j = j+1
  enddo

  l = interp2(t_s(j-1), t_s(j), ecl_s(j-1), ecl_s(j), t_interp)
  b = interp(t_s(j-1), t_s(j), ecb_s(j-1), ecb_s(j), t_interp)

  n_ts = -(/cos(l)*cos(b), sin(l)*cos(b), sin(b)/)  ! target-sun

  call uvw(t_interp, ecl(i), ecb(i), n_ts(1), n_ts(2), n_ts(3), n_ts(1), n_ts(2), n_ts(3))

  n_to = -(/0.d0, 0.d0, 1.d0/)  ! target-observer

  call normal(faces, nodes_, normals)
  call shadowing(normals, n_to, n_ts, masks)

! synthetic silhouette
  call silhouette(nodes_, faces, masks, silh)

  call write_poles(t_interp, ecl(i), ecb(i), pole_l(1), pole_b(1), vardist(i), dataset(i))

! observed silhouette
  call read_pnm(file_OBS(i), pnm)
  c_ = 0.d0

  call silhouette2(pnm, silh_factor, c_/pixel_scale(i), silh_OBS, use_multipoint=.true.)

  silh_OBS = silh_OBS*pixel_scale(i)  ! pxl -> arcsec
  c_ = center_silh(silh_OBS) - center_silh(silh)

  if (debug) then
    write(*,*) '# c  = ', c_/pixel_scale(i), ' pxl'
  endif

! re-centering
  call silhouette2(pnm, silh_factor, c_/pixel_scale(i), silh_OBS, use_multipoint=.true.)
  silh_OBS = silh_OBS*pixel_scale(i)
  c_ = center_silh(silh_OBS) - center_silh(silh)

  deallocate(pnm)

  if (debug) then
    write(*,*) '# c_ = ', c_/pixel_scale(i), ' pxl'
  endif

! chi^2
  do k = 1, size(silh,1)
    dx = silh(k,1)-silh_OBS(k,1)
    dy = silh(k,2)-silh_OBS(k,2)
    chi2_ = (dx/sigma(i))**2 + (dy/sigma(i))**2

    lns = lns + 2.d0*log(sigma(i))
    chi2 = chi2 + chi2_
    n = n+1

    if (debug) then
      write(iu,*) t_interp, silh(k,1), silh(k,2), sigma(i), phase, dataset(i), trim(file_OBS(i)), chi2_
      write(iu,*) t_AO(i), silh_OBS(k,1), silh_OBS(k,2), sigma(i), phase, dataset(i), trim(file_OBS(i)), chi2_
      write(iu,*)
    endif

  enddo

  if (debug_swift) then
    write(str_,'(i4.4)') i
    str = 'nodes' // trim(str_) // '.silh'
    call write_silh(str, silh)

    str = 'nodes' // trim(str_) // '.silh_'
    call write_silh(str, silh_OBS)

!  str = 'nodes' // trim(str_) // '.pnm'
!  call write_pnm(str, pnm)
  endif

enddo

if (debug) then
  close(iu)
endif

return
end subroutine chi2_func_AO

end module chi2_func_AO_module


