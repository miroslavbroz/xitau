! read_node.f90
! Read file with list of nodes (produced by tetgen) into memory.
! Miroslav Broz (miroslav.broz@email.cz), Jul 31st 2007

module read_node_module

contains

subroutine read_node(f_node,nodes)

implicit none

character(len=*) :: f_node
double precision, dimension(:,:), pointer :: nodes

integer :: i,ierr,nnodes,ndim,dummy

open(unit=10, file=f_node, status='old', iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file ', f_node 
  stop
endif

read(10,*,err=900,end=900) nnodes, ndim, dummy, dummy
if (ndim.ne.3) then
  write(*,'(a,a)') '# Error: Only 3D nodes are supported in file ', f_node 
  stop
endif

allocate(nodes(nnodes,ndim))

do i = 1, nnodes
  read(10,*,err=900,end=900) dummy, nodes(i,1), nodes(i,2), nodes(i,3)
enddo

close(unit=10, iostat=ierr)
if (ierr.ne.0) then
  write(*,'(a,a)') '# Warning: not closing file ', f_node 
endif

return

! error handlers

900 write(*,'(a,a)') '# Error: reading file ', f_node 
stop

end subroutine read_node

end module read_node_module


