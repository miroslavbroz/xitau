! chi2.f90
! Chi^2 for shape optimisation.
! Miroslav Broz (miroslav.broz@email.cz), Dec 16th 2022

program chi

use chi2_func_module
use read_dependent_module

implicit none
include '../chi2/chi2.inc'
include '../chi2/dependent.inc'

double precision :: ftol, itmax
double precision, dimension(NDIMMAX) :: x, e, e_param
double precision, dimension(NDIMMAX+1) :: y
double precision, dimension(NDIMMAX+1,NDIMMAX) :: p

integer :: i, j, iter, mp, np, nu
integer, dimension(NDIMMAX+1) :: id
double precision, dimension(NDIMMAX) :: xtry
character(len=255) :: str
double precision :: chi2, probp, probq
double precision, external :: gammp, gammq
double precision :: t1, t2

call cpu_time(t1)

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

!
! read dependent parameters
!
call read_dependent()

! gnuplot
if (debug) then
  open(unit=10,file="T0.plt",status="unknown")
  write(10,*) "T0 = ", T0
  do i = 1, nparam
    write(str,*) i
    write(10,*) "x_param", adjustl(trim(str)) ," = ", x_param(i)
  enddo
  close(10)
endif

! calculate chi^2 and the corresponding probability
ndim = nparam
do i = 1, nparam
  x(i) = x_param(i)
  variable(i) = .TRUE.
enddo

chi2 = chi2_func(x)
nu = n_fit - nparam
probp = gammp(nu/2.d0,chi2/2.d0)
probq = gammq(nu/2.d0,chi2/2.d0)

! resolve variable/fixed parameters
ndim = 0
do i = 1,nparam
  if (variable(i)) then
    ndim = ndim + 1
    x(ndim) = x_param(i)
    e(ndim) = e_param(i)
  endif
enddo

! write output
write(*,*) '# chi2_SKY  = ', chi2_SKY
write(*,*) '# chi2_RV   = ', chi2_RV
write(*,*) '# chi2_TTV  = ', chi2_TTV
write(*,*) '# chi2_ECL  = ', chi2_ECL
write(*,*) '# chi2_VIS  = ', chi2_VIS
write(*,*) '# chi2_CLO  = ', chi2_CLO
write(*,*) '# chi2_T3   = ', chi2_T3
write(*,*) '# chi2_LC   = ', chi2_LC
write(*,*) '# chi2_SYN  = ', chi2_SYN
write(*,*) '# chi2_SED  = ', chi2_SED
write(*,*) '# chi2_AO   = ', chi2_AO
write(*,*) '# chi2_AO2  = ', chi2_AO2
write(*,*) '# chi2_SKY2 = ', chi2_SKY2
write(*,*) '# chi2_SKY3 = ', chi2_SKY3
write(*,*) '# chi2_OCC  = ', chi2_OCC
write(*,*) '# chi2_MASS = ', chi2_MASS
write(*,*) '# chi2 = ', chi2
write(*,*) '# lns = ', lns
write(*,*) '# n_fit = ', n_fit
write(*,*) '# nparam = ', nparam
write(*,*) '# nu = ', int(nu)
write(*,*) '# probp = ', probp
write(*,*) '# probq = ', probq

call cpu_time(t2)

write(*,*) '# cpu_time = ', t2-t1, ' s'

stop

! error handlers

990 continue
write(*,*) "Error reading input."
stop

end program chi


