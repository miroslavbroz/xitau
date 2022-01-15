! centre.f90
! Compute centres of mass for all tetrahedra.
! Miroslav Broz (miroslav.broz@email.cz), Nov 13th 2019

module centre_module

contains

subroutine centre(elems, nodes, coms)

implicit none

integer, dimension(:,:), pointer, intent(in) :: elems
double precision, dimension(:,:), pointer, intent(in) :: nodes
double precision, dimension(:,:), pointer, intent(out) :: coms

integer :: i, j
double precision, dimension(3) :: s

do i = 1, size(elems,1)

!  a = nodes(elems(i,1),:)
!  b = nodes(elems(i,2),:)
!  c = nodes(elems(i,3),:)
!  d = nodes(elems(i,4),:)
!  coms(i,:) = (a+b+c+d)/4.d0

  coms(i,:) = sum(nodes(elems(i,:),:),1)/size(elems,2)
enddo

return

end subroutine centre

end module centre_module


