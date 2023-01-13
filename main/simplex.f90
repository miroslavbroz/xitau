! shapesimplex.f90
! Simplex for shape optimisation.
! Miroslav Broz (miroslav.broz@email.cz), Dec 16th 2022

program shapesimplex

use chi2_func_module
use read_dependent_module
use srtidx_module

implicit none
include '../chi2/chi2.inc'
include '../chi2/dependent.inc'

double precision :: ftol
integer :: itmax
double precision, dimension(NDIMMAX) :: x, e, e_param
double precision, dimension(NDIMMAX+1) :: y
double precision, dimension(NDIMMAX+1,NDIMMAX) :: p

integer :: i, j, iter, mp, np
integer, dimension(NDIMMAX+1) :: id
double precision, dimension(NDIMMAX) :: xtry
character(len=255) :: str

!
! read input parameters
!
write(*,*) "# nparam : "
str = "#"
do while (str(1:1).eq.'#')
  read(*,"(a)",err=990,end=990) str 
enddo

read(str,*,err=990,end=990) nparam
write(*,*) "# nparam = ", nparam
if (nparam.gt.NDIMMAX) then
  write(*,*) 'Error: nparam > NDIMMAX = ', NDIMMAX
  stop
endif

write(*,*) "# x_param() : "
read(*,*,err=990,end=990) (x_param(i), i = 1,nparam)
do i = 1,nparam
  write(*,*) "# x_param(", i, ") = ", x_param(i)
enddo

write(*,*) "# e_param() : "
read(*,*,err=990,end=990) (e_param(i), i = 1,nparam)
do i = 1,nparam
  write(*,*) "# e_param(", i, ") = ", e_param(i)
enddo

write(*,*) "# variable() : "
read(*,*,err=990,end=990) (variable(i), i = 1,nparam)
do i = 1,nparam
  write(*,*) "# variable(", i, ") = ", variable(i)
enddo

!
! read dependent parameters
!
call read_dependent()

write(*,*) "# ftol : "
read(*,*,err=990,end=990) ftol
write(*,*) "# ftol = ", ftol

write(*,*) "# itmax : "
read(*,*,err=990,end=990) itmax
write(*,*) "# itmax = ", itmax

! resolve variable/fixed parameters
ndim = 0
do i = 1,nparam
  if (variable(i)) then
    ndim = ndim + 1
    x(ndim) = x_param(i)
    e(ndim) = e_param(i)
  endif
enddo

if (debug) then
  write(*,*) '# ndim = ', ndim
endif

! pass the rest in common block /dependent/ 

! initialise the simplex (p array)

do i = 1,ndim+1
  do j = 1,ndim
    if (j.eq.i) then
      p(i,j) = x(j) + e(i)
    else
      p(i,j) = x(j)
    endif
  enddo
enddo

if (debug) then
  write(*,*) "# initial p() array:"
  do i = 1,ndim+1
    do j = 1,ndim
      write(*,20) p(i,j)
20    format(f16.8,1x,$)
    enddo
    write(*,*)
  enddo
  write(*,*)
endif

! y() array
do i = 1,ndim+1
  do j = 1,ndim
    xtry(j) = p(i,j)
  enddo
  y(i) = chi2_func(xtry)
enddo

if (debug) then
  write(*,*) "# initial y() array:"
  do i = 1,ndim+1
    write(*,*) y(i)
  enddo
  write(*,*)
endif

mp = NDIMMAX+1
np = NDIMMAX
iter = 0

write(*,*) 'itmax = ', itmax

call amoeba(p,y,mp,np,ndim,ftol,chi2_func,iter,itmax)

! write the result of minimalisation

call srtidx(y,id)

if (debug) then
  write(*,*) "# iter = ", iter

  write(*,*) "# p() array:"
  do i = 1,ndim+1
    do j = 1,ndim
      write(*,20) p(id(i),j)
    enddo
    write(*,*)
  enddo

  write(*,*) "# y() array:"
  do i = 1,ndim+1
    write(*,*) y(id(i))
  enddo
  write(*,*)
endif

write(*,*) (p(id(1),j), j = 1,ndim), y(id(1))

stop

! error handlers

990 continue
write(*,*) "Error reading input."
stop

end program shapesimplex


