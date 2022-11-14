! write1.f90
! Write 1-dimensional array.
! Miroslav Broz (miroslav.broz@email.cz), Oct 28th 2022

module write1_module

contains

subroutine write1(filename, vals)

implicit none
character(len=*) :: filename
double precision, dimension(:) :: vals
integer :: i

open(unit=10, file=filename, status='unknown')
write(10,*) size(vals)
do i = 1,size(vals)
  write(10,*) i, vals(i)
enddo
close(10)

end subroutine write1

end module write1_module


