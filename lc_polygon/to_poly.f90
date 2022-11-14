! to_poly.f90
! Convert triangles to polygons.
! Miroslav Broz (miroslav.broz@email.cz), Nov 5th 2022

module to_poly_module

contains

subroutine to_poly(faces, nodes, polys)

use polytype_module

implicit none
integer, dimension(:,:), pointer, intent(in) :: faces
double precision, dimension(:,:), pointer, intent(in) :: nodes
type(polystype), dimension(:), pointer, intent(out) :: polys

integer :: i, j

do i = 1, size(faces,1)
  do j = 1, size(faces,2)
    polys(i)%c = 1
    polys(i)%s(1)%c = 3
    polys(i)%s(1)%p(j,:) = nodes(faces(i,j),:)
  enddo
enddo

end subroutine to_poly

end module to_poly_module

