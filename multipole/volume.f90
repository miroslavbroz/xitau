! volume.f90
! Compute volume.
! Miroslav Broz (miroslav.broz@email.cz), Nov 13th 2019

module volume_module

contains

double precision function volume(elems, nodes, vols)

implicit none

integer, dimension(:,:), pointer, intent(in) :: elems
double precision, dimension(:,:), pointer, intent(in) :: nodes
double precision, dimension(:), pointer, intent(out) :: vols

integer :: i
double precision :: V, tmp
double precision, dimension(3) :: a, b, c, d

V = 0.d0

do i = 1, size(elems,1)
  a = nodes(elems(i,1),:)
  b = nodes(elems(i,2),:)
  c = nodes(elems(i,3),:)
  d = nodes(elems(i,4),:)

  vols(i) = v1(a, b, c, d)
  V = V + vols(i)
enddo

volume = V
return

end function volume

double precision function v1(a, b, c, d)

use vector_product_module

implicit none

double precision, dimension(3) :: a, b, c, d
double precision :: tmp

tmp = dot_product(vector_product(b-d, c-d), a-d)
v1 = abs(tmp)/6.d0

end function v1

end module volume_module

