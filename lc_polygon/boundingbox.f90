! bbox.f90
! Bounding boxes of polygons.
! Miroslav Broz (miroslav.broz@email.cz), Nov 6th 2022

module boundingbox_module

contains

subroutine boundingbox(polys, boxes)

use polytype_module

implicit none
type(polystype), dimension(:), pointer, intent(in) :: polys
double precision, dimension(:,:), pointer, intent(out) :: boxes

double precision, parameter :: INF = 1.d38
integer :: i, j, k
double precision :: minu, minv, minw, maxu, maxv, maxw

allocate(boxes(size(polys,1),6))

!$omp parallel do private(i,j,k) shared(polys,boxes)
do i = 1, size(polys,1)
  minu = +INF
  maxu = -INF
  minv = +INF
  maxv = -INF
  minw = +INF
  maxw = -INF
  do j = 1, polys(i)%c
    k = polys(i)%s(j)%c
    minu = min(minu, minval(polys(i)%s(j)%p(1:k,1)))
    maxu = max(maxu, maxval(polys(i)%s(j)%p(1:k,1)))
    minv = min(minv, minval(polys(i)%s(j)%p(1:k,2)))
    maxv = max(maxv, maxval(polys(i)%s(j)%p(1:k,2)))
    minw = min(minw, minval(polys(i)%s(j)%p(1:k,3)))
    maxw = max(maxw, maxval(polys(i)%s(j)%p(1:k,3)))
  enddo
  boxes(i,1) = minu
  boxes(i,2) = maxu
  boxes(i,3) = minv
  boxes(i,4) = maxv
  boxes(i,5) = minw
  boxes(i,6) = maxw
enddo
!$omp end parallel do

return
end subroutine boundingbox

end module boundingbox_module


