! shapesubplex.f90
! Subplex for shape optimisation.
! Miroslav Broz (miroslav.broz@email.cz), Dec 23rd 2022

program shapesubplex

use read_dependent_module

implicit none
include '../chi2/chi2.inc'
include '../chi2/dependent.inc'

double precision, dimension(NDIMMAX) :: x, e, e_param
double precision :: tol1, tol2, tolfact
integer :: iter1, iter2, iterincr

! internal
integer :: i
integer, parameter :: nsmin = min(2,NDIMMAX)
integer, parameter :: nsmax = min(5,NDIMMAX)
integer, dimension(NDIMMAX+int(NDIMMAX/nsmin)) :: iwork
double precision, dimension(2*NDIMMAX+nsmax*(nsmax+4)+1) :: work
integer :: iflag, mode, mdcont, mduser, mdsing, maxiter, numiter
double precision :: tol, y
character(len=255) :: str

! functions
double precision, external :: func

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

write(*,*) "# tol1 : "
read(*,*,err=990,end=990) tol1
write(*,*) "# tol1 = ", tol1

write(*,*) "# tol2 : "
read(*,*,err=990,end=990) tol2
write(*,*) "# tol2 = ", tol2

write(*,*) "# tolfact : "
read(*,*,err=990,end=990) tolfact
write(*,*) "# tolfact = ", tolfact

write(*,*) "# iter1 : "
read(*,*,err=990,end=990) iter1
write(*,*) "# iter1 = ", iter1

write(*,*) "# iter2 : "
read(*,*,err=990,end=990) iter2
write(*,*) "# iter2 = ", iter2

write(*,*) "# iterincr : "
read(*,*,err=990,end=990) iterincr
write(*,*) "# iterincr = ", iterincr

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

mdcont = 0  ! continuation
mduser = 0  ! user-defined
mdsing = 0  ! single-step
tol = tol1
maxiter = iter1

do while ((maxiter.lt.iter2).and.(tol.gt.tol2).and.(iflag.ne.-2).and.(iflag.ne.1))

  mode = 4*mdsing + 2*mduser + mdcont

  call subplx(func,ndim,tol,maxiter,mode,e,x,y,numiter,work,iwork,iflag)

  mdcont = 1
  if (iflag.eq.-1) then
    maxiter = maxiter + iterincr
  else if (iflag.eq.0) then
    tol = tol*tolfact
  endif

  ! write intermediate results
  write(*,*) "# iflag = ", iflag, " # -1 .. maxiter, 0 .. tol"
  write(*,*) "# numiter = ", numiter
  write(*,*) "# maxiter = ", maxiter
  write(*,*) "# tol = ", tol

enddo

! write the result of minimalisation
write(*,*) "x = ", x(1:ndim)
write(*,*) "y = ", y

stop

! error handlers
990 continue
write(*,*) 'subplex: Error reading standard input.'
stop

end program shapesubplex

double precision function func(n,x)
use chi2_func_module
implicit none
integer :: n
double precision, dimension(n) :: x
func = chi2_func(x)
return
end function func


