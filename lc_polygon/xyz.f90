! xyz.f90
! Compute (x, y, z) coordinates back from (u, v, w).
! Miroslav Broz (miroslav.broz@email.cz), Jan 5th 2023

! Note: hatx_, haty_, hatz_ are uvw_module variables.

module xyz_module

contains

subroutine xyz(polys1, polys2)

use polytype_module
use uvw_module

implicit none
type(polystype), dimension(:), pointer, intent(in) :: polys1
type(polystype), dimension(:), pointer, intent(out) :: polys2

integer :: i, j, k

!$omp parallel do private(i,j,k) shared(polys1,polys2,hatx_,haty_,hatz_)
do i = 1, size(polys1,1)

  polys2(i)%c = polys1(i)%c

  do j = 1, polys1(i)%c

    polys2(i)%s(j)%c = polys1(i)%s(j)%c

    do k = 1, polys1(i)%s(j)%c

      polys2(i)%s(j)%p(k,1) = dot_product(hatx_, polys1(i)%s(j)%p(k,:))
      polys2(i)%s(j)%p(k,2) = dot_product(haty_, polys1(i)%s(j)%p(k,:))
      polys2(i)%s(j)%p(k,3) = dot_product(hatz_, polys1(i)%s(j)%p(k,:))

    enddo
  enddo
enddo
!$omp end parallel do

end subroutine xyz

subroutine xyz_(normals)

use uvw_module

implicit none
double precision, dimension(:,:), pointer, intent(inout) :: normals

integer :: i
double precision :: x, y, z

!$omp parallel do private(i,x,y,z) shared(normals,hatx_,haty_,hatz_)
do i = 1, size(normals,1)
  x = dot_product(hatx_, normals(i,:))
  y = dot_product(haty_, normals(i,:))
  z = dot_product(hatz_, normals(i,:))
  normals(i,:) = (/x, y, z/)
enddo
!$omp end parallel do

return
end subroutine xyz_

end module xyz_module


