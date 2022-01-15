! write_silh.f90
! Write silhouette to file.
! Miroslav Broz (miroslav.broz@email.cz), Aug 28th 2020

module write_silh_module

contains

subroutine write_silh(f, silh)

use const_module

implicit none

character(len=*) :: f
double precision, dimension(:,:), pointer :: silh

integer :: i, ierr

open(unit=10, file=f, status="unknown", iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file ', f
  stop
endif

write(10,*) '# u [arcsec] & v [arcsec]'
do i = 1, size(silh,1)
  write(10,*) silh(i,:)
enddo

close(10)

return

end subroutine write_silh

end module write_silh_module


