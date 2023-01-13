! simann.f
! Simmulated annealing for the xi Tau problem.
! Miroslav Broz (miroslav.broz@email.cz), May 24th 2017

program simann

use chi2_func_module
use read_dependent_module
use srtidx_module

implicit none
include '../chi2/chi2.inc'
include '../chi2/dependent.inc'

double precision :: ftol
integer :: itmax
integer :: itertot, iter_at_temp
double precision, dimension(NDIMMAX) :: x, e, e_param
double precision, dimension(NDIMMAX+1) :: y
double precision, dimension(NDIMMAX+1,NDIMMAX) :: p

integer i, j, iter, mp, np
integer, dimension(NDIMMAX+1) :: id
double precision, dimension(NDIMMAX) :: xtry, pb
double precision :: yb
double precision :: temptr, eps_temptr
character(len=255) :: str

integer length

call util_version

!
! read input parameters
!
write(*,*) "# nparam : "
str = "#"
do while (str(1:1).eq.'#')
  read(*,"(a)",err=990,end=990) str 
enddo

read(str,*,err=990,end=990) nparam
if (nparam.gt.NDIMMAX) then
  write(*,*) 'Error: nparam > NDIMMAX = ', NDIMMAX
  stop
endif
write(*,*) "# nparam = ", nparam

write(*,*) "# x_param() : "
read(*,*,err=990,end=990) (x_param(i), i=1,nparam)
do i = 1, nparam
  write(*,*) "# x_param(", i, ") = ", x_param(i)
enddo

write(*,*) "# e_param() : "
read(*,*,err=990,end=990) (e_param(i), i=1,nparam)
do i = 1,nparam
  write(*,*) "# e_param(", i, ") = ", e_param(i)
enddo

write(*,*) "# variable() : "
read(*,*,err=990,end=990) (variable(i), i=1,nparam)
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

write(*,*) "# ITMAX : "
read(*,*,err=990,end=990) ITMAX
write(*,*) "# ITMAX = ", ITMAX

write(*,*) "# iter_at_temp : "
read(*,*,err=990,end=990) iter_at_temp
write(*,*) "# iter_at_temp = ", iter_at_temp

write(*,*) "# temptr : "
read(*,*,err=990,end=990) temptr
write(*,*) "# temptr = ", temptr

write(*,*) "# eps_temptr : "
read(*,*,err=990,end=990) eps_temptr
write(*,*) "# eps_temptr = ", eps_temptr

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

yb = 1.d38
do i = 1,ndim
  pb(i) = 0.d0
enddo

! run it!
itertot = 0
iter = 0
do while ((iter.le.0).and.(itertot.le.ITMAX))
  iter = iter_at_temp
  itertot = itertot + iter

  call amebsa(p,y,mp,np,ndim,pb,yb,ftol,chi2_func,iter,temptr)

  temptr = (1.d0-eps_temptr)*temptr  ! annealing schedule
  itertot = itertot - iter

  write(*,*) "# iter = ", iter
  write(*,*) "# itertot = ", itertot
  write(*,*) "# temptr = ", temptr
enddo

! sort the output according to chi^2
call srtidx(y,id)

! write the result of minimalisation

if (debug) then
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

write(*,*) (pb(j), j = 1,ndim), yb

stop

! error handlers
990 continue
write(*,*) 'simplex: Error reading standard input.'

end program simann


