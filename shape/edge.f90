! edge.f90
! Get edges of faces.
! Miroslav Broz (miroslav.broz@email.cz), Dec 16th 2022

! Note: Assuming closed three-dimensional mesh.

module edge_module

contains

subroutine edge(nodes, faces, edges)

use srtint_module
use write_edge_module

implicit none
double precision, dimension(:,:), pointer, intent(in) :: nodes
integer, dimension(:,:), pointer, intent(in) :: faces
integer, dimension(:,:), pointer, intent(out) :: edges

integer :: i, j, k, n
integer, dimension(:,:), pointer :: twice
integer, dimension(:), pointer :: id

! nodes - edges + faces = 2 .. Euler formula

n = size(nodes,1) + size(faces,1) - 2
allocate(edges(n,2))
allocate(twice(2*n,2))
allocate(id(2*n))

! include all edges twice
k = 0
do i = 1, size(faces,1)
  do j = 1, 3
    k = k+1
    twice(k,:) = (/faces(i,j), faces(i,1+mod(j,3))/)
  enddo
enddo

! order of nodes
do i = 1, size(twice,1)
  if (twice(i,1).gt.twice(i,2)) then
    j = twice(i,2)
    twice(i,2) = twice(i,1)
    twice(i,1) = j
  endif
enddo

! sort indices
call srtint(n*twice(:,1)+twice(:,2), id)

!do i = 1, size(twice,1)
!  write(*,*) 'twice(', i, ') = ', twice(id(i),:)  ! dbg
!enddo

! include all edges once
do i = 1, size(edges,1)
  edges(i,:) = twice(id(2*i-1),:)
enddo

call write_edge("output.twice", twice)  ! dbg
call write_edge("output.edge", edges)  ! dbg

deallocate(twice)
deallocate(id)

end subroutine edge

end module edge_module


