! silhouette.f90
! Compute silhouette(s).
! Miroslav Broz (miroslav.broz@email.cz), Sep 27th 2020

module silhouette_module

integer, parameter :: nsilh = 360

contains

! Compute a silhouette from nodes/faces; convex version.

subroutine silhouette(nodes, faces, masks, silh)

use const_module
use shadowing_module

implicit none

integer, dimension(:,:), pointer, intent(in) :: faces
logical, dimension(:), pointer, intent(in) :: masks
double precision, dimension(:,:), pointer, intent(in) :: nodes
double precision, dimension(:,:), pointer, intent(out) :: silh

integer :: i, j, k, l
double precision :: tmp, absmaxe, phi
double precision, dimension(2) :: a, b, c, d, e, maxe

do i = 1, nsilh

  ! a "ray" from origin, c + s*d
  phi = dble(i-1)/nsilh * 2.d0*pi
  c = (/0.d0, 0.d0/)
  d = (/cos(phi), sin(phi)/)

  ! find the most distant intersection w. faces/edges
  maxe = (/0.d0, 0.d0/)
  absmaxe = 0.d0
  do j = 1, size(faces,1)
    if (masks(j)) then
      do k = 1, 3
        l = faces(j, k)
        a = nodes(l, 1:2)
        l = faces(j, mod(k,3)+1)
        b = nodes(l, 1:2) - a
     
        e = intersect(a, b, c, d)
     
        if (.not.isnan(e(1))) then
          tmp = dot_product(e, e)
          if (tmp.gt.absmaxe) then
            absmaxe = tmp
            maxe = e
          endif
        endif
      enddo
    endif
  enddo

  silh(i, :) = (/maxe(1), maxe(2)/)
enddo

end subroutine silhouette

! Intersection of 2 line segments, a + t*b = c + s*d.

function intersect(a, b, c, d)

implicit none

double precision, dimension(2), intent(in) :: a, b, c, d
double precision, dimension(2) :: intersect

double precision :: t, s

t = (c(1)*d(2) - c(2)*d(1) - a(1)*d(2) + a(2)*d(1)) / ((b(1)*d(2) - b(2)*d(1)))
s = (a(1) + t*b(1) - c(1)) / d(1)

if ((t.ge.0.d0).and.(t.le.1.d0).and.(s.gt.0.d0)) then
  intersect = a + t*b
else
  intersect = 0.d0
endif
return

end function intersect

! Compute a silhouette for a PNM image

subroutine silhouette2(pnm, silh_factor, center, silh)

use const_module

implicit none

double precision, dimension(:,:), pointer, intent(in) :: pnm
double precision, intent(in) :: silh_factor
double precision, dimension(2), intent(in) :: center
double precision, dimension(:,:), pointer :: silh

integer :: i, j, k, l, w, h, maxl
double precision :: phi, u, v, x0, f0, minf_
double precision, dimension(:), allocatable :: x, x_, f, f_

! i ... row
! j ... column

h = size(pnm, 1)
w = size(pnm, 2)

maxl = int(sqrt(dble(w*w+h*h)/4.d0))+1
allocate(x(maxl))
allocate(f(maxl))
allocate(x_(maxl))
allocate(f_(maxl))

do k = 1, nsilh

  phi = dble(k-1)/nsilh * 2.d0*pi

  ! find radial profile
  do l = 1, maxl
    u = center(1) + l*cos(phi)
    v = center(2) + l*sin(phi)
    i = int(h/2.d0-v+0.5)
    j = int(w/2.d0+u+0.5)
    x(l) = l
    if ((i.ge.1).and.(i.le.h).and.(j.ge.1).and.(j.le.w)) then
      f(l) = pnm(i, j)
    else
      f(l) = 0.d0
    endif
  enddo

  ! find the respective level
  f0 = silh_factor*max_(f)
  x0 = interp_(f, x, f0)
  u = x0*cos(phi)
  v = x0*sin(phi)

  silh(k, :) = (/u, v/)
enddo

deallocate(x)
deallocate(f)
deallocate(x_)
deallocate(f_)

end subroutine silhouette2

! Non-monotonic interpolation; we use is for x(f), NOT f(x)!

double precision function interp_(x, y, x0)

implicit none

double precision, dimension(:) :: x, y
double precision :: x0

integer :: i, n
double precision :: y0

n = size(x)
interp_ = -1.d0

do i = 2, n
  if (((x(i-1).lt.x0).and.(x(i).gt.x0)).or.((x(i).lt.x0).and.(x(i-1).gt.x0))) then
    interp_ = y(i-1) + (y(i)-y(i-1)) * (x0-x(i-1))/(x(i)-x(i-1))
  endif
enddo

return
end function interp_


double precision function max_(x)

double precision, dimension(:) :: x

integer i

max_ = x(1)
do i = 1, size(x)
  max_ = max(x(i), max_)
enddo

return
end function max_

! Silhouette center as triangles centre-of-area (weigthed by area).

function center_silh(silh)

double precision, dimension(:,:), pointer, intent(out) :: silh
double precision, dimension(2) :: center_silh

integer :: i, n
double precision :: tmp, s
double precision, dimension(2) :: a, b, c

n = size(silh,1)
s = 0.d0
c = (/0.d0, 0.d0/)
do i = 1, n-1
  a = silh(i,:)
  b = silh(i+1,:)
  tmp = abs(a(1)*b(2)-a(2)*b(1))
  s = s + tmp
  c = c + tmp*(a+b)/3.d0
enddo
c = c/s

center_silh = c
return
end function center_silh

end module silhouette_module



