! uvw_nodes.f90
! Sky-plane coordinates for nodes.
! Miroslav Broz (miroslav.broz@email.cz), Aug 20th 2020

module uvw_nodes_module

contains

subroutine uvw_nodes(t, l, b, nodes)

implicit none
real*8 t, l, b
real*8 hatu(3), hatv(3), hatw(3)
double precision, dimension(:,:), pointer :: nodes

integer i
real*8 u, v, w

call uvw1(t, l, b, hatu, hatv, hatw)

do i = 1, size(nodes,1)
  call uvw2(hatu, hatv, hatw, nodes(i,1), nodes(i,2), nodes(i,3), u, v, w)
  nodes(i,:) = (/u, v, w/)
enddo

return
end subroutine uvw_nodes

end module uvw_nodes_module

