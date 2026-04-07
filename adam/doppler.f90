! doppler.f90
! Doppler-delay computed for polygons.
! Miroslav Broz (miroslav.broz@email.cz), Apr 4th 2026

! Note: polys5 has coordinates u, v, w (i.e. sky-plane)
! Note: polys6 has coordinates u .. doppler [Hz], v .. delay [mus], w .. 0.0

module doppler_module

contains

subroutine doppler(polys5, polys6, freq, rb, vb)

use polytype_module
use lc_polygon_module, only: normals, bodyset_
use uvw_module, only: hatv, hatu, hatw
use const_module
use input_module
use write_poly_module
use distance_AB_C_module
use vector_product_module
use surface2_module

implicit none
include '../chi2/chi2.inc'
include '../chi2/dependent.inc'

type(polystype), dimension(:), pointer, intent(in) :: polys5
type(polystype), dimension(:), pointer, intent(out) :: polys6
double precision, intent(in) :: freq
double precision, dimension(:,:), intent(in) :: rb, vb

! internal variables
integer :: i, j, k
double precision :: ecl, ecb, t, absomega, tmp, delta_f, delay
double precision, dimension(3) :: A, B, C, D, axis, omega
double precision, dimension(3) :: r_of_j, v_of_j, v
logical :: extra
character(len=80) :: str

integer, save :: no = 0

!
! initialisation
!

! from dependent.inc
pole_l_ = pole_l(1:2)
pole_b_ = pole_b(1:2)
phi0_ = phi0(1:2)
P_rot_ = P_rot(1:2)*day

do i = 1, size(polys5,1)

  polys6(i)%c = polys5(i)%c

  ! rotation axis
  j = bodyset_(i)
  ecl = pole_l(j)
  ecb = pole_b(j)
  axis = (/cos(ecl)*cos(ecb), sin(ecl)*cos(ecb), sin(ecb)/)
  axis = (/dot_product(hatu,axis), dot_product(hatv,axis), dot_product(hatw,axis)/)
  absomega = 2.d0*pi/P_rot_(j)
  omega = absomega*axis
  r_of_j = rb(j,:)
  v_of_j = vb(j,:)

  A = r_of_j
  B = axis

  do j = 1, polys5(i)%c

    polys6(i)%s(j)%c = polys5(i)%s(j)%c

    do k = 1, polys5(i)%s(j)%c

      C = polys5(i)%s(j)%p(k,:)

      ! distance from axis
      tmp = distance_AB_C(A, B, C, t, extra)

      D = A + (B-A)*t
      v = vector_product(omega, C-D)
      v = v + v_of_j

      delta_f = freq * v(3)/clight
      delay = C(3)/clight

      polys6(i)%s(j)%p(k,:) = (/delta_f, delay/1.d-6, 0.d0/)

    enddo
  enddo
enddo

if (debug_polygon) then
  no = no+1
  if ((no.ge.1).and.(no.le.99)) then
    write(str,'(i0.2)') no

    call write_poly("output.poly6." // trim(str), polys6)

  endif
endif

return
end subroutine doppler

end module doppler_module


