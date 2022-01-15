! write_pnm.f90
! Write PNM file.
! Miroslav Broz (miroslav.broz@email.cz), Aug 28th 2020

module write_pnm_module

contains

subroutine write_pnm(f, pnm)

implicit none

character(len=*) :: f
double precision, dimension(:,:), pointer :: pnm

integer :: i, j, ierr, w, h
character(len=255) :: str

open(unit=10, file=f, status="unknown", iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file ', f
  stop
endif

h = size(pnm, 1)
w = size(pnm, 2)

write(10,'(a)') 'P2'
write(10,'(a)') '# Created by xitau'
write(10,*) w, h
write(10,*) 65535

! i ... row
! j ... column

do i = 1, h
  do j = 1, w
    write(10,*) int(pnm(i, j))
  enddo
enddo

close(10)

write(*,*) '# f = ', trim(f), ' (', w, 'x', h, ')'

return

! error handlers

900 write(*,'(a,a)') '# Error: reading file ', f
stop

end subroutine write_pnm

end module write_pnm_module


