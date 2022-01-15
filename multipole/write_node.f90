! write_node.f90
! Read file with list of nodes (produced by tetgen) into memory.
! Miroslav Broz (miroslav.broz@email.cz), Jul 31st 2007

module write_node_module

contains

subroutine write_node(f_node,nodes)

implicit none

character(len=*) :: f_node
double precision, dimension(:,:), pointer :: nodes

integer :: i,ierr,nnodes,ndim,dummy

open(unit=10, file=f_node, status='unknown', iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file ', f_node 
  stop
endif

nnodes = size(nodes,1)
ndim = 3
dummy = 0
write(10,*) nnodes, ndim, dummy, dummy

do i = 1, nnodes
  write(10,*,err=900) i, nodes(i,1), nodes(i,2), nodes(i,3)
enddo

close(unit=10)

return

! error handlers

900 write(*,'(a,a)') '# Error: writing file ', f_node 
stop

end subroutine write_node

end module write_node_module


