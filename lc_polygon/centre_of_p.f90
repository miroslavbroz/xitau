! centre_of_p.f90
! Compute centres of polygons.
! Miroslav Broz (miroslav.broz@email.cz), Nov 9th 2022

module centre_of_p_module

contains

subroutine centre(polys, centres)

use polytype_module

implicit none
type(polystype), dimension(:), pointer, intent(in) :: polys
double precision, dimension(:,:), pointer, intent(out) :: centres

integer :: i, j, k, n
double precision, dimension(3) :: s

!$omp parallel do private(i,j,k,n,s) shared(polys,centres)
do i = 1, size(polys,1)
  if (polys(i)%c.eq.0) then
    centres(i,:) = 0.d0
    cycle
  endif
  s = 0.d0
  n = 0
  do j = 1, polys(i)%c
    k = polys(i)%s(j)%c
    s = s + sum(polys(i)%s(j)%p(1:k,:),1)
    n = n + k
  enddo
  centres(i,:) = s/n
enddo
!$omp end parallel do

return
end subroutine centre

end module centre_of_p_module


