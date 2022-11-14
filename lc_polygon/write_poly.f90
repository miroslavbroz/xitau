! write_poly.f90
! Write polygons.
! Miroslav Broz (miroslav.broz@email.cz), Nov 5th 2022

module write_poly_module

contains

subroutine write_poly(f_poly, polys)

use polytype_module

implicit none
character(len=*), intent(in) :: f_poly
type(polystype), dimension(:), pointer, intent(in) :: polys

integer :: i, j, k, ierr

open(unit=10, file=f_poly, status='unknown', iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file ', f_poly 
  stop
endif

write(10,*) size(polys,1)

do i = 1, size(polys,1)
  do j = 1, polys(i)%c
    do k = 1, polys(i)%s(j)%c
      write(10,*,err=900) i, j, k, polys(i)%s(j)%p(k,:)
    enddo
    write(10,*)
    write(10,*)
  enddo
enddo

close(unit=10)

return

900 write(*,'(a,a)') '# Error: writing file ', f_poly 
stop

end subroutine write_poly

end module write_poly_module

