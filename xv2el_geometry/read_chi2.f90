! read_chi2.f
! Read chi2.in file and extract some data.
! Miroslav Broz (miroslav.broz@email.cz), Jul 30th 2015

subroutine read_chi2(m)

use const_module
use read_dependent_module

include '../chi2/chi2.inc'
include '../chi2/dependent.inc'
! input
! output
real*8 m(NBODMAX), y(6,NBODMAX)

! internal
integer i, j, k, iu, ierr

real*8 msum
real*8 q(NBODMAX)
real*8 elmts(NBODMAX,6)
character*255 str

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

! parameters -> variables, arrays for easy manipulation

j = 0

!do i = 1, nbod
!  j = j+1
!  m(i) = x_param(j)*GM_S
!enddo

j = j+1
msum = x_param(j)*GM_S
do i = 2, nbod
  j = j+1
  q(i) = x_param(j)
enddo

do i = 2, nbod
  do k = 1, 6
    j = j+1
    elmts(i,k) = x_param(j)
  enddo
enddo

do i = 1, nbod
  j = j+1
  T_eff(i) = x_param(j)
enddo

!do i = 1, nbod
!  j = j+1
!  R_star(i) = x_param(j)
!enddo
do i = 1, nbod
  j = j+1
  log_g(i) = x_param(j)
enddo

do i = 1, nbod
  j = j+1
  v_rot(i) = x_param(j)
enddo
!do i = 1, nbod
!  j = j+1
!  P_rot(i) = x_param(j)
!enddo

do i = 1, nbod
  j = j+1
  metal(i) = x_param(j)
enddo

do i = 1, nbod
  j = j+1
  Delta_t(i) = x_param(j)/day
enddo

do i = 1, nbod
  j = j+1
  C20(i) = x_param(j)
enddo

do i = 1, nbod
  j = j+1
  pole_l(i) = x_param(j)*deg
enddo

do i = 1, nbod
  j = j+1
  pole_b(i) = x_param(j)*deg
enddo

do i = 1, nbod
  j = j+1
  phi0(i) = x_param(j)*deg
enddo

do i = 1, nbod
  j = j+1
  albedo(i) = x_param(j)
enddo

do i = 1, 4
  j = j+1
  scattering(i) = x_param(j)
enddo
scattering(4) = scattering(4)*deg  ! bartheta

do i = 1, 2
  j = j+1
  psf_param(i) = x_param(j)
enddo

do i = 1, nband
  j = j+1
  zero(i) = x_param(j)
enddo

j = j+1
gamma = x_param(j)
j = j+1
d_pc = x_param(j)

if (j.ne.nparam) then
  write(*,*) "chi2_func.f: Error number of parameters is ", j, ".ne.nparam = ", nparam
  stop
endif

! convert ratios to masses
m(1) = msum
do i = 2, nbod
  m(1) = m(1)/(1.d0+q(i))
  m(i) = 0.d0
enddo
do i = 2, nbod
  do j = 1, i-1
    m(i) = m(i)+m(j)
  enddo
  m(i) = q(i)*m(i)
enddo

if (debug) then
  do i = 1, nbod
    write(*,*) '# m(', i, ') = ', m(i)/GM_S, ' M_S'
  enddo
endif

! ordering...
!do i = 1, nbod
!  do j = 1, 6
!    y(j,i) = elmts(i,j)
!  enddo
!enddo

return

990 continue
write(*,*) 'Error reading chi2.in file.'
stop
end


