! clip.f90
! Clip polygons using Vatti (1992) algorithm; from Clipper2 library.
! Miroslav Broz (miroslav.broz@email.cz), Dec 20th 2022

! Note: If there is no clip, it is necessary to revert the polygon!
! Otherwise, a non-clipped polygon (polys4->polys5) has an incorrect
! orientation, if it was clipped previously (polys2->polys3).

module clip_module

contains

subroutine clip(polys2, polys3, clips)

use iso_c_binding
use polytype_module
use boundingbox_module
use revert_module

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
integer, dimension(:), pointer, intent(inout) :: clips

integer :: i, j, k, l, c, m
type(polystype) :: poly_i, poly_j, poly_k

double precision, dimension(:,:), pointer, save :: boxes
double precision, parameter :: EPS = 1.0d-6

call boundingbox(polys2, boxes)

polys3(:)%c = 0

!$omp parallel do private(i,j,poly_i,poly_j,poly_k) shared(polys2,polys3,boxes)
do i = 1, size(polys2,1)
  if (polys2(i)%c.eq.0) cycle
  poly_i = polys2(i)
  c = 0

  do j = 1, size(polys2,1)
    if (j.eq.i) cycle                                                    ! self-shadowing
    if (poly_i%c.eq.0) exit                                              ! no-polygons-in-set
    if (polys2(j)%c.eq.0) cycle                                          ! no-points-in-polygon
    if ((boxes(j,2).lt.boxes(i,1)).or.(boxes(j,1).gt.boxes(i,2))) cycle  ! bounding-box-in-u
    if ((boxes(j,4).lt.boxes(i,3)).or.(boxes(j,3).gt.boxes(i,4))) cycle  ! bounding-box-in-v
    if (boxes(j,6).lt.boxes(i,6)) cycle                                  ! is-in-front

    call clip_in_c(poly_i, polys2(j), poly_k)

    c = c+1

    include 'c1.inc'
  enddo

  if ((c.eq.0).and.(clips(i).gt.0)) then
    call revert(poly_i)
    c = 999
  endif

  polys3(i) = poly_i
  clips(i) = c
enddo
!$omp end parallel do

deallocate(boxes)

end subroutine clip

end module clip_module


