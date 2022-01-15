! normal.f90
! Compute normals of faces.
! Miroslav Broz (miroslav.broz@email.cz), Aug 7th 2020

module normal_module

contains

subroutine normal(faces, nodes, normals)

use vector_product_module
use normalize_module

implicit none

integer, dimension(:,:), pointer, intent(in) :: faces
double precision, dimension(:,:), pointer, intent(in) :: nodes
double precision, dimension(:,:), pointer, intent(out) :: normals

integer :: i
double precision, dimension(3) :: a, b, c, d

do i = 1,size(faces,1)
  a = nodes(faces(i,1),:)
  b = nodes(faces(i,2),:)
  c = nodes(faces(i,3),:)

!  d = -vector_product(b-a, c-b)
  d = vector_product(b-a, c-b)
  d = normalize(d)
  normals(i,:) = d
enddo

end subroutine normal

end module normal_module


