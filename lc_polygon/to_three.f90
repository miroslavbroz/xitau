! to_three.f90
! From 2D to 3D polygons.
! Miroslav Broz (miroslav.broz@email.cz), Nov 6th 2022

module to_three_module

contains

subroutine to_three(normals, centres, polys)

use polytype_module

implicit none
double precision, dimension(:,:), pointer, intent(in) :: normals, centres
type(polystype), dimension(:), pointer, intent(inout) :: polys

integer :: i, j, k
double precision :: a, b, c, d, x, y, z

! Eq. of a plane: ax + by + cz + d = 0

!$omp parallel do private(i,j,k,a,b,c,d,x,y,z) shared(polys,normals,centres)
do i = 1, size(polys,1)

  a = normals(i,1)
  b = normals(i,2)
  c = normals(i,3)
  d = dot_product(normals(i,:), centres(i,:))

  do j = 1, polys(i)%c
    do k = 1, polys(i)%s(j)%c
      x = polys(i)%s(j)%p(k,1)
      y = polys(i)%s(j)%p(k,2)
      z = (d - a*x - b*y)/c
      polys(i)%s(j)%p(k,3) = z
    enddo
  enddo
enddo
!$omp end parallel do

return
end subroutine to_three

end module to_three_module


