! silhouette.f90
! Compute silhouette(s).
! Miroslav Broz (miroslav.broz@email.cz), Sep 27th 2020

module silhouette_module

integer, parameter :: nsilh = 360

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Compute an observed silhouette for a PNM image

subroutine silhouette2(pnm, silh_factor, center, silh, use_multipoint)

use const_module

implicit none

double precision, dimension(:,:), pointer, intent(in) :: pnm
double precision, intent(in) :: silh_factor
double precision, dimension(2), intent(in) :: center
double precision, dimension(:,:), pointer, intent(out) :: silh
logical, intent(in), optional :: use_multipoint
logical :: use_, debug_

integer :: i, j, k, l, w, h, maxl
double precision :: phi, u, v, du, dv, u_inpxl, v_inpxl, r, dr
double precision :: x0, f0, minf_
double precision, dimension(:), allocatable :: x, x_, f, f_
integer, parameter :: step = 1
logical :: debug = .false.

if (present(use_multipoint)) then
  use_ = use_multipoint
else
  use_ = .false.
endif

!debug = .true.  ! dbg

! i .. row, up -> down, v
! j .. column, left -> right, u

h = size(pnm, 1)
w = size(pnm, 2)

maxl = step*int(sqrt(dble(w*w+h*h)/4.d0))+1

allocate(x(maxl))
allocate(f(maxl))
allocate(x_(maxl))
allocate(f_(maxl))

do k = 1, nsilh
!do k = 1, 2

  phi = dble(k-1)/nsilh * 2.d0*pi
  du = cos(phi)
  dv = sin(phi)
  dr = 1.d0/step/max(abs(du),abs(dv))

  ! find radial profile
  do l = 1, maxl
    r = (float(l)-0.5d0)*dr
    u = center(1) + r*du
    v = center(2) + r*dv
    x(l) = r
    f(l) = 0.d0

    v_inpxl = 0.5d0*h - v + 1.0d0
    u_inpxl = 0.5d0*w + u + 1.0d0
    i = int(v_inpxl)
    j = int(u_inpxl)

    if (use_) then

      if ((i.gt.1).and.(i.lt.h).and.(j.gt.1).and.(j.lt.w)) then
        f(l) = multipoint(pnm, i, j, u_inpxl, v_inpxl)
      endif

    else

      if ((i.ge.1).and.(i.le.h).and.(j.ge.1).and.(j.le.w)) then
        f(l) = pnm(i,j)
        if (debug) then
          write(20,*) u, v, i, j, pnm(i,j)
        endif
      endif

    endif

    if (debug) then
      write(10,*) u, v
      write(30,*) u, v, l, x(l), f(l)
    endif
  enddo

  ! find the respective level
  f0 = silh_factor*maxval(f)
  x0 = nonmonotonic(f, x, f0)

  u = x0*cos(phi)
  v = x0*sin(phi)

! Note: This would align silhouette again w. PNM, but it's offset wrt. shape in chi2_AO.f90!
!  if (use_) then
!    u = u + 0.5d0
!    v = v - 0.5d0
!  endif

  silh(k, :) = (/u, v/)
enddo

deallocate(x)
deallocate(f)
deallocate(x_)
deallocate(f_)

end subroutine silhouette2

! Multi-point interpolation.

double precision function multipoint(pnm, i, j, u, v)

implicit none
double precision, dimension(:,:), pointer, intent(in) :: pnm
integer, intent(in) :: i, j
double precision, intent(in) :: u, v

double precision :: v00, v01, v10, v11, v0, v1, val

v00 = pnm(i+0,j+0)
v01 = pnm(i+0,j+1)
v10 = pnm(i+1,j+0)
v11 = pnm(i+1,j+1)

v0 = v00 + (v01-v00)*(u-j)/(j+1-j)
v1 = v10 + (v11-v10)*(u-j)/(j+1-j)

val = v0 + (v1-v0)*(v-i)/(i+1-i)

!write(40,*) v00, v01
!write(40,*) v10, v11
!write(40,*)
!write(40,*) v0, v1
!write(40,*)
!write(40,*) val
!write(40,*) '--'

multipoint = val
return
end function multipoint

! Non-monotonic interpolation; we use it for x(f), NOT f(x)!

double precision function nonmonotonic(x, y, x0)

implicit none
double precision, dimension(:) :: x, y
double precision :: x0

integer :: i, n
double precision :: y0

n = size(x)
y0 = 0.d0

do i = 2, n
  if (((x(i-1).le.x0).and.(x(i).ge.x0)).or.((x(i-1).ge.x0).and.(x(i).le.x0))) then
    y0 = y(i-1) + (y(i)-y(i-1)) * (x0-x(i-1))/(x(i)-x(i-1))
  endif
enddo

nonmonotonic = y0
return
end function nonmonotonic

! Silhouette center as triangles centre-of-area (weigthed by area).

function center_silh(silh)

implicit none
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Compute a synthetic silhouette from nodes/faces; convex version.

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

end module silhouette_module



