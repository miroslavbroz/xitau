! clip.f90
! Clip polygons using Vatti (1992) algorithm; from Clipper2 library.
! Miroslav Broz (miroslav.broz@email.cz), Nov 5th 2022

! Note: The EPS parameter is necessary to perform at least one clip!
! Otherwise, a non-clipped polygon (polys4->polys5) has an incorrect
! orientation, if it was clipped previously (polys2->polys3).

module clip_module

contains

subroutine clip(polys2, polys3)

use iso_c_binding
use polytype_module
use boundingbox_module

implicit none

interface
  subroutine clip_in_c(poly_i, poly_j, poly_k) bind(c, name='clip_in_c')
  use iso_c_binding
  use polytype_module
  type(polystype), bind(c) :: poly_i, poly_j, poly_k
  end subroutine clip_in_c
end interface

type(polystype), dimension(:), pointer, intent(in) :: polys2
type(polystype), dimension(:), pointer, intent(out) :: polys3

integer :: i, j, k, l
type(polystype) :: poly_i, poly_j, poly_k

double precision, dimension(:,:), pointer, save :: boxes
double precision, parameter :: EPS = 1.0d-6

call boundingbox(polys2, boxes)

polys3(:)%c = 0

!$omp parallel do private(i,j,poly_i,poly_j,poly_k) shared(polys2,polys3,boxes)
do i = 1, size(polys2,1)
  if (polys2(i)%c.eq.0) cycle
  poly_i = polys2(i)

  do j = 1, size(polys2,1)
    if (j.ne.i) then

      if (poly_i%c.eq.0) exit
      if (polys2(j)%c.eq.0) cycle
      if ((boxes(j,2).lt.boxes(i,1)).or.(boxes(j,1).gt.boxes(i,2))) cycle
      if ((boxes(j,4).lt.boxes(i,3)).or.(boxes(j,3).gt.boxes(i,4))) cycle
      if (boxes(j,6).lt.boxes(i,6)-EPS) cycle

      call clip_in_c(poly_i, polys2(j), poly_k)

      include 'c1.inc'
    endif
  enddo

  polys3(i) = poly_i
enddo
!$omp end parallel do

end subroutine clip

end module clip_module


