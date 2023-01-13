! write_edge.f90
! Write file with list of edges.
! Miroslav Broz (miroslav.broz@email.cz), Dec 16th 2022

module write_edge_module

contains

subroutine write_edge(f_edge,edges)

implicit none

character(len=*), intent(in) :: f_edge
integer, dimension(:,:), pointer, intent(in) :: edges

integer :: i,ierr,nedges,dummy

open(unit=10, file=f_edge, status='unknown', iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file ', f_edge 
  stop
endif

nedges = size(edges,1)

dummy = 0
write(10,*) nedges, dummy

do i = 1,size(edges,1)
  write(10,*,err=900) i, edges(i,1), edges(i,2)
enddo

close(unit=10)

return

! error handlers

900 write(*,'(a,a)') '# Error: writing file ', f_edge 
stop

end subroutine write_edge

end module write_edge_module


