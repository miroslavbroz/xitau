! raytrace.f90
! Raytracing of synthetic image.
! Miroslav Broz (miroslav.broz@email.cz), Dec 24th 2022

! inside-polygon-2d test
! bounding-box test
! triangle-line intersection?
! centre-of-polygon distance, weighting?
! clipping of polygons by sq. pixels, sum of? (i.e., the most precise)

module raytrace_module

contains

subroutine raytrace(polys, Phi_e, mu_e, d_to, pixel_scale, c, w, h, pnm)

use const_module
use polytype_module
use boundingbox_module
use inside_polygon_module

implicit none
type(polystype), dimension(:), pointer, intent(in) :: polys
double precision, dimension(:), pointer, intent(in) :: Phi_e, mu_e
double precision, intent(in) :: d_to, pixel_scale
double precision, dimension(2), intent(in) :: c
integer, intent(in) :: w, h
double precision, dimension(:,:), pointer, intent(out) :: pnm 

logical :: is_inside
integer :: i, j, k, l
double precision :: u, v, du, dv
double precision, dimension(3) :: hatu, hatv, r
double precision, dimension(:,:), pointer :: boxes
double precision, dimension(6) :: box1, box2

allocate(pnm(w,h))
pnm = 0.d0

! bounding box
call boundingbox(polys, boxes)

! u, v .. centres of pixels
! if w, h odd 256
! 127/255 = 0.498
! 128/255 = 0.502
! -0.5w .. -0.002w, +0.002w .. +0.5w
! +0.5h .. -0.002h, +0.002h .. -0.5h

! of-pnm
du = pixel_scale*arcsec*d_to
dv = du
box2(1) = -0.5d0*w*du
box2(2) = +0.5d0*w*du
box2(3) = -0.5d0*h*dv
box2(4) = +0.5d0*h*dv

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

!open(unit=10, file="uv.dat", status='unknown')  ! dbg

hatu = (/1.d0, 0.d0, 0.d0/)
hatv = (/0.d0, 1.d0, 0.d0/)

do i = 1, h
  do j = 1, w

    u = +(dble(j-1)/(w-1)-0.5d0)*w * du + c(1)*d_to
    v = -(dble(i-1)/(h-1)-0.5d0)*h * dv + c(2)*d_to
    r = u*hatu + v*hatv
    u = r(1)
    v = r(2)

    if ((u.lt.box1(1)).or.(u.gt.box1(2))) cycle
    if ((v.lt.box1(3)).or.(v.gt.box1(4))) cycle

    is_inside = .false.
    do k = 1, size(polys,1)
      if (polys(k)%c.eq.0) cycle
      if ((u.lt.boxes(k,1)).or.(u.gt.boxes(k,2))) cycle
      if ((v.lt.boxes(k,3)).or.(v.gt.boxes(k,4))) cycle
 
      call inside_polygon(polys(k), (/u, v/), is_inside)
 
      if (is_inside) exit
    enddo

! Note: pixel's S is already projected => division by mu_e!

    if (is_inside) then
      pnm(i,j) = Phi_e(k)/mu_e(k)
    endif

!    write(10,*) u, v  ! dbg

  enddo ! j
enddo ! i

deallocate(boxes)

!close(10)  ! dbg

end subroutine raytrace

end module raytrace_module


