! read_elem.f90
! Read file with list of tetrahedral elements (produced by tetgen) into memory.
! Miroslav Broz (miroslav.broz@email.cz), Jul 31st 2007

module read_elem_module

contains

subroutine read_elem(f_elem,elems)

implicit none

character(len=*) :: f_elem
integer, dimension(:,:), pointer :: elems

integer :: i,ierr,nelems,ndim,dummy

open(unit=10, file=f_elem, status='old', iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file ', f_elem 
  stop
endif

read(10,*) nelems, ndim, dummy
if (ndim.ne.4) then
  write(*,'(a,a)') '# Error: Only tetrahedral elements are supported in file ', f_elem 
  stop
endif

allocate(elems(nelems,ndim))

do i = 1, nelems
  read(10,*,err=900,end=900) dummy, elems(i,1), elems(i,2), elems(i,3), elems(i,4)
enddo

close(unit=10, iostat=ierr)
if (ierr.ne.0) then
  write(*,'(a,a)') '# Warning: not closing file ', f_elem 
endif

return

! error handlers

900 write(*,'(a,a)') '# Error: reading file ', f_elem 
stop

end subroutine read_elem

end module read_elem_module


