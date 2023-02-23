! subdivide_n.f90
! Subdivide n times.
! Miroslav Broz (miroslav.broz@email.cz), Jan 26th 2023

module subdivide_n_module

contains

subroutine subdivide_n(n, nodes2, faces2, nodesforchi, facesforchi)

use subdivide_module

implicit none
integer, intent(in) :: n
double precision, dimension(:,:), pointer, intent(in) :: nodes2
integer, dimension(:,:), pointer, intent(in) :: faces2
double precision, dimension(:,:), pointer, intent(out) :: nodesforchi
integer, dimension(:,:), pointer, intent(out) :: facesforchi

double precision, dimension(:,:), pointer :: nodes3, nodes4, nodes5
integer, dimension(:,:), pointer :: faces3, faces4, faces5

if (n.eq.0) then
  allocate(nodesforchi(size(nodes2,1),size(nodes2,2)))
  allocate(facesforchi(size(faces2,1),size(faces2,2)))
  nodesforchi = nodes2
  facesforchi = faces2
else if (n.eq.1) then
  call subdivide(nodes2, faces2, nodesforchi, facesforchi)
else if (n.eq.2) then
  call subdivide(nodes2, faces2, nodes3, faces3)
  call subdivide(nodes3, faces3, nodesforchi, facesforchi)
else if (n.eq.3) then
  call subdivide(nodes2, faces2, nodes3, faces3)
  call subdivide(nodes3, faces3, nodes4, faces4)
  call subdivide(nodes4, faces4, nodesforchi, facesforchi)
else if (n.eq.4) then
  call subdivide(nodes2, faces2, nodes3, faces3)
  call subdivide(nodes3, faces3, nodes4, faces4)
  call subdivide(nodes4, faces4, nodes5, faces5)
  call subdivide(nodes5, faces5, nodesforchi, facesforchi)
else
  write(*,*) 'Error: number of subdivisions nsub = ', n, '.gt.4'
  stop
endif

if (n.ge.2) deallocate(nodes3)
if (n.ge.2) deallocate(faces3)
if (n.ge.3) deallocate(nodes4)
if (n.ge.3) deallocate(faces4)
if (n.ge.4) deallocate(nodes5)
if (n.ge.4) deallocate(faces5)

end subroutine subdivide_n

end module subdivide_n_module


