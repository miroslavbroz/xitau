! map2.f
! Calculate a chi^2 map (2-dimensional).
! Miroslav Broz (miroslav.broz@email.cz), Nov 16th 2022

program map2

use chi2_func_module
use read_dependent_module

implicit none
include '../chi2/chi2.inc'
include '../chi2/dependent.inc'

integer :: whichparam, whichparam_
double precision :: val1, val1_, val2, val2_, step, step_

integer :: i
double precision, dimension(NDIMMAX) :: x
double precision :: chi2, val, val_
character(len=255) :: str
double precision, parameter :: eps = 1.0d-8

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

!
! read dependent parameters
!
call read_dependent()

write(*,*) "# whichparam :"
read(*,*,err=990,end=990) whichparam

write(*,*) "# val1 val2 step :"
read(*,*,err=990,end=990) val1, val2, step 

write(*,*) "# whichparam_ :"
read(*,*,err=990,end=990) whichparam_

write(*,*) "# val1_ val2_ step_ :"
read(*,*,err=990,end=990) val1_, val2_, step_

ndim = nparam
do i = 1, nparam
  x(i) = x_param(i)
  variable(i) = .TRUE.
enddo

!
!  calculate chi^2 values
!
val_ = val1_
do while (val_.le.val2_+eps)
  val = val1
  do while (val.le.val2+eps)
    x(whichparam) = val
    x(whichparam_) = val_
    chi2 = chi2_func(x)
    val = val+step
  enddo
  val_ = val_+step_
enddo

stop

! error handlers
990 continue
write(*,*) 'map1: Error reading standard input.'

end program map2


