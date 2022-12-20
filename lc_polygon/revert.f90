! revert.f90
! Revert polygon, i.e., different signed area!
! Miroslav Broz (miroslav.broz@email.cz), Dec 19th 2022

module revert_module

contains

subroutine revert(poly_i)

use polytype_module

implicit none
type(polystype), intent(inout) :: poly_i

type(polystype) :: poly_k
integer i, j, k

poly_k = poly_i

poly_i%c = poly_k%c
do i = 1, poly_k%c
  k = poly_k%s(i)%c
  poly_i%s(i)%c = k
  do j = 1,k
    poly_i%s(i)%p(j,:) = poly_k%s(i)%p(k+1-j,:)
  enddo
enddo

end subroutine revert

end module revert_module


