! write_elem.f90
! Write file with list of tetrahedral elements.
! Miroslav Broz (miroslav.broz@email.cz), Aug 20th 2020

module write_elem_module

contains

subroutine write_elem(f_elem,elems)

implicit none

character(len=*) :: f_elem
integer, dimension(:,:), pointer :: elems

integer :: i,ierr,nelems,ndim,dummy

open(unit=10, file=f_elem, status='unknown', iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file ', f_elem 
  stop
endif

nelems = size(elems,1)
ndim = 4
dummy = 0
write(10,*) nelems, ndim, dummy

do i = 1, nelems
  write(10,*,err=900) i, elems(i,1), elems(i,2), elems(i,3), elems(i,4)
enddo

close(unit=10)

return

! error handlers

900 write(*,'(a,a)') '# Error: writing file ', f_elem 
stop

end subroutine write_elem

end module write_elem_module


