! inside_polygon.f90
! Inside-polygon test, 2-dimensional, using ray casting.
! Miroslav Broz (miroslav.broz@email.cz), Dec 25th 2022

! Note: It is done for the SET of polygons (cf. annular eclipses).

module inside_polygon_module

contains

subroutine inside_polygon(polys, A, is_inside)

use polytype_module
use intersect_AB_l_module

implicit none
type(polystype), intent(in) :: polys
double precision, dimension(2), intent(in) :: A
logical, intent(out) :: is_inside

integer :: i, j, k, l
double precision, dimension(2), parameter :: B = (/1.d0, 0.d0/)
double precision, dimension(2,2) :: line
logical :: has_solution

is_inside = .false.

k = 0
do i = 1, polys%c

  l = polys%s(i)%c
  do j = 1, l
    has_solution = .false.
    line(1,:) = polys%s(i)%p(j,1:2)
    line(2,:) = polys%s(i)%p(1+mod(j,l),1:2)

    call intersect_AB_l(A, B, line, has_solution)

    if (has_solution) k = k+1
  enddo

enddo

if (mod(k,2).eq.1) is_inside = .true.

end subroutine inside_polygon

end module inside_polygon_module


