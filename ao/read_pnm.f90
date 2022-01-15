! read_pnm.f90
! Read PNM file.
! Miroslav Broz (miroslav.broz@email.cz), Aug 28th 2020

module read_pnm_module

contains

subroutine read_pnm(f, pnm)

implicit none

character(len=*) :: f
double precision, dimension(:,:), pointer :: pnm

integer :: i, j, k, l, maxl, w, h, ierr, dummy
double precision :: val
character(len=255) :: str
character(len=65535) :: line

open(unit=10, file=f, status="unknown", iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') "# Error: opening file '", trim(f), "'."
  stop
endif

read(10,*,err=900,end=900) str

if (trim(str).ne.'P2') then
  write(*,*) 'Error: only P2 PNM format is supported.'
  stop
endif

read(10,*,err=900,end=900) str
read(10,*,err=900,end=900) w, h
read(10,*,err=900,end=900) dummy

allocate(pnm(w, h))

! i ... row
! j ... column

i = 1
j = 1
do k = 1, w*h
  if (j.gt.w) then
    i = i+1
    j = 1
  endif
  read(10,*,err=900,end=900) pnm(i, j)
  j = j+1
enddo

close(10)

write(*,*) '# f = ', trim(f), ' (', w, 'x', h, ')'

return

! error handlers

900 write(*,'(a,a)') '# Error: reading file ', f
stop

end subroutine read_pnm

end module read_pnm_module


