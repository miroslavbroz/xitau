! center_pnm.f90
! Photocentre of a PNM image.
! Miroslav Broz (miroslav.broz@email.cz), Jan 6th 2023

module center_pnm_module

contains

function center_pnm(pnm, pixel_scale)

use const_module

implicit none
double precision, dimension(:,:), intent(in) :: pnm
double precision, intent(in) :: pixel_scale
double precision, dimension(2) :: center_pnm

integer :: i, j, w, h
double precision :: u, v, du, dv
double precision, dimension(2) :: hatu, hatv
double precision, dimension(2) :: r, c

du = pixel_scale*arcsec
dv = du
hatu = (/1.d0, 0.d0/)
hatv = (/0.d0, 1.d0/)

c = 0.d0
w = size(pnm,1)
h = size(pnm,2)

do i = 1, w
  do j = 1, h

    u = +(dble(j-1)/(w-1)-0.5d0)*w * du
    v = -(dble(i-1)/(h-1)-0.5d0)*h * dv
    r = u*hatu + v*hatv
!    r = (/i, j/)

    c = c + r*pnm(i,j)
  enddo
enddo

c = c/sum(pnm)

!write(*,*) 'c = ', c
!write(*,*) 'sum(pnm) = ', sum(pnm)
!stop

center_pnm = c
return
end function center_pnm

end module center_pnm_module


