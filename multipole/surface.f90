! surface.f90
! Compute surface area.
! Miroslav Broz (miroslav.broz@email.cz), Mar 31st 2021

module surface_module

contains

double precision function surface(faces, nodes, surf)

implicit none

integer, dimension(:,:), pointer, intent(in) :: faces
double precision, dimension(:,:), pointer, intent(in) :: nodes
double precision, dimension(:), pointer, intent(out) :: surf

integer :: i
double precision :: S, tmp
double precision, dimension(3) :: a, b, c

S = 0.d0

do i = 1, size(faces,1)
  a = nodes(faces(i,1),:)
  b = nodes(faces(i,2),:)
  c = nodes(faces(i,3),:)

  surf(i) = s1(a, b, c)
  S = S + surf(i)
enddo

surface = S
return

end function surface

double precision function s1(a, b, c)

use vector_product_module

implicit none

double precision, dimension(3) :: a, b, c, tmp

tmp = vector_product(b-a, c-a)
s1 = 0.5d0*sqrt(dot_product(tmp, tmp))

end function s1

end module surface_module

