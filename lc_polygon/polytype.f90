! polytype.f90
! Polygon and set of polygons.
! Miroslav Broz (mirolav.broz@email.cz), Nov 6th 2022

! Note: Here we use interoperable static structures.

! Notation:
!
! c .. count
! s .. set of polygons
! p .. polygon
!
! size(polys1)          .. count of sets
! polys1(1)%c           .. count of polygons
! polys1(1)%s(1)%c      .. count of nodes
! polys1(1)%s(1)        .. 1st polygon in a set
! polys1(1)%s(1)%p(1,:) .. 1st node in a polygon
! polys1(1)%s(1)%p(1,1) .. 1st node, x-coordinate

module polytype_module

use iso_c_binding

include 'polytype.inc'

type, bind(c) :: polytype
  integer(c_int) :: c = 0
  real(c_double), dimension(MAXPOLY,3) :: p
end type polytype

type, bind(c) :: polystype
  integer(c_int) :: c = 0
  type(polytype), dimension(MAXPOLYS) :: s
end type polystype

end module polytype_module


