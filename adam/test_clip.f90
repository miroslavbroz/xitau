
program test_clip

use iso_c_binding
use polytype_module
use write_poly_module

implicit none

interface
  subroutine crop_in_c(poly_i, poly_j, poly_k) bind(c, name='crop_in_c')
  use iso_c_binding
  use polytype_module
  type(polystype), bind(c) :: poly_i, poly_j, poly_k
  end subroutine crop_in_c
end interface

type(polystype), dimension(:), pointer :: polys1, polys2, polys3

allocate(polys1(1))
allocate(polys2(1))
allocate(polys3(1))

polys1(1)%c = 1
polys1(1)%s(1)%c = 3
polys1(1)%s(1)%p(1,:) = (/0.0d0, 0.0d0, 0.0d0/)
polys1(1)%s(1)%p(2,:) = (/1.0d0, 0.0d0, 0.0d0/)
polys1(1)%s(1)%p(3,:) = (/0.0d0, 1.0d0, 0.0d0/)

polys2(1)%c = 1
polys2(1)%s(1)%c = 4
polys2(1)%s(1)%p(1,:) = (/0.1d0-0.5d0, -0.5d0, 0.0d0/)
polys2(1)%s(1)%p(2,:) = (/0.1d0+0.5d0, -0.5d0, 0.0d0/)
polys2(1)%s(1)%p(3,:) = (/0.1d0+0.5d0, +0.5d0, 0.0d0/)
polys2(1)%s(1)%p(4,:) = (/0.1d0-0.5d0, +0.5d0, 0.0d0/)

call crop_in_c(polys2(1), polys1(1), polys3(1))

call write_poly("output.poly1", polys1)
call write_poly("output.poly2", polys2)
call write_poly("output.poly3", polys3)

end program test_clip


