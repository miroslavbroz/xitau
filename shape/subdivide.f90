! subdivide.f90
! Subdivide mesh, "sqrt(3)" scheme.
! Miroslav Broz (miroslav.broz@email.cz), Dec 14th 2022

! Reference: Kobbelt (2000), Proc. 27th Comp. Graphics, 103

module subdivide_module

contains

subroutine subdivide(nodes, faces, nodes2, faces3)

use const_module
use edge_module

implicit none
double precision, dimension(:,:), pointer, intent(in) :: nodes
integer, dimension(:,:), pointer, intent(in) :: faces
double precision, dimension(:,:), pointer, intent(out) :: nodes2
integer, dimension(:,:), pointer, intent(out) :: faces3

integer :: i, j, k, l, n, m
double precision :: alpha
double precision, dimension(3) :: a, b, c, d
integer, dimension(:,:), pointer :: faces2
integer, dimension(32) :: neighs
integer, dimension(:,:), pointer :: edges

allocate(nodes2(size(nodes,1)+size(faces,1),3))
allocate(faces2(3*size(faces,1),3))
allocate(faces3(size(faces2,1),3))

! add new nodes (centres of faces)
k = size(nodes,1)
l = 0
do i = 1, size(faces,1)
  a = nodes(faces(i,1),:)
  b = nodes(faces(i,2),:)
  c = nodes(faces(i,3),:)
  d = (a + b + c)/3.d0  ! Eq. (1)
  k = k+1
  nodes2(k,:) = d

! three new faces
  do j = 1, 3
    l = l+1
    faces2(l,1) = faces(i,1+mod(j-1,3))
    faces2(l,2) = faces(i,1+mod(j,3))
    faces2(l,3) = k
  enddo
enddo

! adjust old nodes
do i = 1, size(nodes,1)

  call neigh(faces, i, neighs, n)

  alpha = (4.d0 - 2.d0*cos(2.d0*pi/n))/9.d0  ! Eq. (6)
!  alpha = 0.d0  ! dbg
!  write(*,*) 'alpha = ', alpha  ! dbg

  d = (/0.d0, 0.d0, 0.d0/)
  do j = 1, n
    d = d + nodes(neighs(j),:)
  enddo

  nodes2(i,:) = (1.d0-alpha)*nodes(i,:) + alpha*1.d0/n*d  ! Eq. (2)

enddo  ! i

call edge(nodes, faces, edges)
call flip(edges, faces2, faces3)

deallocate(edges)
deallocate(faces2)

end subroutine subdivide

! Find neighboring nodes of a node.

subroutine neigh(faces, index_of_node, neighs, number_of_neighs)

implicit none
integer, dimension(:,:), pointer, intent(in) :: faces
integer, intent(in) :: index_of_node
integer, dimension(:), intent(out) :: neighs
integer, intent(out) :: number_of_neighs

integer :: i, j, k, l

neighs = 0
l = 0

do i = 1, size(faces,1)
  if (any(faces(i,:).eq.index_of_node)) then
    do j = 1, 3
      k = faces(i,j)
      if (k.ne.index_of_node) then
        if (any(k.eq.neighs(1:max(1,l)))) then
        else
          l = l+1
          neighs(l) = faces(i,j)
        endif
      endif
    enddo  ! j
  endif
enddo  ! i

number_of_neighs = l

!write(*,*) 'neighs of ', i, ' = ', neighs(1:n)  ! dbg

end subroutine neigh

! Flip faces along edges.

!  i1-----i3     i1-----i3
!    \   / \       \ -__ \
!     \ /   \       \   - \
!     i2-----i4     i2-----i4
 
subroutine flip(edges, faces, faces2)

implicit none
integer, dimension(:,:), pointer, intent(in) :: edges
integer, dimension(:,:), pointer, intent(in) :: faces
integer, dimension(:,:), pointer, intent(out) :: faces2

integer i, j, k, l, j1, j2, i1, i2, i3, i4
logical, dimension(3) :: test

do i = 1, size(edges,1)

! find faces corresponding to the (old) edge
  k = 0
  do j = 1, size(faces)
    if ((any(faces(j,:).eq.edges(i,1))).and.(any(faces(j,:).eq.edges(i,2)))) then
      k = k+1
      if (k.eq.1) j1 = j
      if (k.eq.2) j2 = j
      if (k.eq.2) exit
    endif
  enddo

! flip faces (triangles)
  test = (faces(j1,:).ne.edges(i,1)).and.(faces(j1,:).ne.edges(i,2))
  do j = 1, 3
    if (test(j)) k = j
  enddo
  i1 = faces(j1,k)
  i2 = faces(j1,1+mod(k,3))
  i3 = faces(j1,1+mod(k+1,3))

  test = (faces(j2,:).ne.edges(i,1)).and.(faces(j2,:).ne.edges(i,2))
  do j = 1, 3
    if (test(j)) k = j
  enddo
  i4 = faces(j2,k)
  
  faces2(j1,1) = i1
  faces2(j1,2) = i2
  faces2(j1,3) = i4
  faces2(j2,1) = i4
  faces2(j2,2) = i3
  faces2(j2,3) = i1

enddo  ! i

end subroutine flip

end module subdivide_module


