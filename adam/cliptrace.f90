! cliptrace.f90
! Cliptracing of synthetic image.
! Miroslav Broz (miroslav.broz@email.cz), Jan 10th 2023

module cliptrace_module

contains

subroutine cliptrace(polys, Phi_e, normals, d_to, pixel_scale, c, w, h, pnm)

! Notation:
!
! polys  .. sets of polygons, clipped (visibility), m
! Phi_e   .. monochromatic flux, outgoing
! normals .. normals of polygons, 1
! d_to    .. target-observer distance, m
! c       .. centre of image, rad
! w       .. width, pxl
! h       .. height, pxl
! pnm     .. PNM image, adu
! u       .. centre of pixel, m
! v       .. centre of pixel, m
! du      .. 1-pixel, m
! dv      .. 1-pixel, m

use iso_c_binding
use const_module
use polytype_module
use boundingbox_module
use surface_module

implicit none

interface
  subroutine crop_in_c(poly_i, poly_j, poly_k) bind(c, name='crop_in_c')
  use iso_c_binding
  use polytype_module
  type(polystype), bind(c) :: poly_i, poly_j, poly_k
  end subroutine crop_in_c
end interface

type(polystype), dimension(:), pointer, intent(in) :: polys
double precision, dimension(:), pointer, intent(in) :: Phi_e
double precision, dimension(:,:), pointer, intent(in) :: normals
double precision, intent(in) :: d_to, pixel_scale
double precision, dimension(2), intent(in) :: c
integer, intent(in) :: w, h
double precision, dimension(:,:), pointer, intent(out) :: pnm 

type(polystype) :: poly1
type(polystype), dimension(:), pointer :: polystmp
double precision, dimension(:), pointer :: surf
double precision, dimension(:,:), pointer :: normalstmp

integer :: i, j, k, l
double precision :: u, v, du, dv
double precision, dimension(3) :: hatu, hatv, r
double precision, dimension(:,:), pointer :: boxes
double precision, dimension(6) :: pxl, box1, box2
double precision :: S, tot

! Note: Length-1 arrays a'd because some subroutines 'd arrays.

allocate(polystmp(1))
allocate(surf(1))
allocate(normalstmp(1,3))
allocate(pnm(w,h))
pnm = 0.d0

! bounding box
call boundingbox(polys, boxes)

! of-pnm
du = pixel_scale*arcsec*d_to
dv = du
box2(1) = -0.5d0*(w+1)*du
box2(2) = +0.5d0*(w+1)*du
box2(3) = -0.5d0*(h+1)*dv
box2(4) = +0.5d0*(h+1)*dv

! over-all
box1 = (/+INF, -INF, +INF, -INF, +INF, -INF/)
do i = 1, size(boxes,1)
  if (boxes(i,1).gt.box2(2)) cycle
  if (boxes(i,2).lt.box2(1)) cycle
  if (boxes(i,3).gt.box2(4)) cycle
  if (boxes(i,4).lt.box2(3)) cycle
  box1(1) = min(box1(1),boxes(i,1))
  box1(2) = max(box1(2),boxes(i,2))
  box1(3) = min(box1(3),boxes(i,3))
  box1(4) = max(box1(4),boxes(i,4))
enddo

hatu = (/1.d0, 0.d0, 0.d0/)
hatv = (/0.d0, 1.d0, 0.d0/)

do i = 1, h
  do j = 1, w

    u = +(dble(j-1)/(w-1)-0.5d0)*w * du + c(1)*d_to
    v = -(dble(i-1)/(h-1)-0.5d0)*h * dv + c(2)*d_to
    r = u*hatu + v*hatv
    u = r(1)
    v = r(2)

    ! 1 pixel as a box
    pxl = (/u-0.5d0*du, u+0.5d0*du, v-0.5d0*dv, v+0.5d0*dv, 0.d0, 0.d0/)

    if ((pxl(2).lt.box1(1)).or.(pxl(1).gt.box1(2))) cycle
    if ((pxl(4).lt.box1(3)).or.(pxl(3).gt.box1(4))) cycle

    ! 1 pixel as a set of polygons
    poly1%c = 1
    poly1%s(1)%c = 4
    poly1%s(1)%p(1,:) = (/pxl(1), pxl(3), 0.d0/)
    poly1%s(1)%p(2,:) = (/pxl(2), pxl(3), 0.d0/)
    poly1%s(1)%p(3,:) = (/pxl(2), pxl(4), 0.d0/)
    poly1%s(1)%p(4,:) = (/pxl(1), pxl(4), 0.d0/)

    tot = 0.d0
    do k = 1, size(polys,1)
      if (polys(k)%c.eq.0) cycle
      if ((pxl(2).lt.boxes(k,1)).or.(pxl(1).gt.boxes(k,2))) cycle
      if ((pxl(4).lt.boxes(k,3)).or.(pxl(3).gt.boxes(k,4))) cycle

      call crop_in_c(polys(k), poly1, polystmp(1))

      if (polystmp(1)%c.eq.0) cycle

      ! i.e., contribution to 1 pixel
      normalstmp(1,:) = normals(k,:)
      S = surface(polystmp, normalstmp, surf)
      tot = tot + Phi_e(k)*S
    enddo

    pnm(i,j) = tot

  enddo ! j
enddo ! i

deallocate(polystmp)
deallocate(surf)
deallocate(boxes)

end subroutine cliptrace

end module cliptrace_module


