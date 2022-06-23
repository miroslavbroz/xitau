! test_preces.f90
! Test precession and nutation.
! Miroslav Broz (miroslav.broz@email.cz), Jun 22nd 2022

subroutine test_preces(t_UT1, ra_j2000, de_j2000)

use const_module
use hhms_module
use preces_module
use nutate_module

implicit none
double precision, intent(in) :: t_UT1, ra_j2000, de_j2000

double precision :: h, m, s, absk
double precision :: t1, t2, t0, t, zeta, zz, theta
double precision :: eps, deltapsi, deltaeps
double precision, dimension(3) :: k, k_
double precision :: eps_earth
double precision :: ra_ofdate, de_ofdate

write(*,*) '--'
write(*,*) '# t_UT1 = ', t_UT1, ' = ', t_UT1-J2000, " d from J2000"
call hhms(ra_j2000/pi*12.d0, h, m, s)
write(*,*) '# ra_j2000  = ', ra_j2000/pi*12.d0, ' h   = ', int(h), int(m), s
call hhms(de_j2000/deg, h, m, s)
write(*,*) '# de_j2000  = ', de_j2000/deg, ' deg = ', int(h), int(m), s
write(*,*)

k = (/cos(ra_j2000)*cos(de_j2000), sin(ra_j2000)*cos(de_j2000), sin(de_j2000)/)
k_ = k

t1 = J2000
t2 = t_UT1
t0 = (t1-J2000)/36525.d0
t = (t2-t1)/36525.d0
eps = eps_earth(t2)
call preces_angle(t0,t,zeta,zz,theta)
call nutate_angle(t,deltapsi,deltaeps)
call preces(k_,zeta,zz,theta)
call nutate(k_,eps,deltapsi,deltaeps)

call hhms(eps/deg, h, m, s)
write(*,*) '# eps       = ', eps/deg, ' deg = ', int(h), int(m), s
call hhms(zeta/deg, h, m, s)
write(*,*) '# zeta      = ', zeta/deg, ' deg = ', int(h), int(m), s
call hhms(theta/deg, h, m, s)
write(*,*) '# theta     = ', theta/deg, ' deg = ', int(h), int(m), s
call hhms(zz/deg, h, m, s)
write(*,*) '# zz        = ', zz/deg, ' deg = ', int(h), int(m), s
call hhms(deltapsi/deg, h, m, s)
write(*,*) '# deltapsi  = ', deltapsi/deg, ' deg = ', int(h), int(m), s
call hhms(deltaeps/deg, h, m, s)
write(*,*) '# deltaeps  = ', deltaeps/deg, ' deg = ', int(h), int(m), s
write(*,*)

absk = sqrt(dot_product(k_, k_))
ra_ofdate = atan2(k_(2),k_(1))
de_ofdate = asin(k_(3)/absk)

call hhms(ra_ofdate/pi*12.d0, h, m, s)
write(*,*) '# ra_ofdate = ', ra_ofdate/pi*12.d0, ' h   = ', int(h), int(m), s
call hhms(de_ofdate/deg, h, m, s)
write(*,*) '# de_ofdate = ', de_ofdate/deg, ' deg = ', int(h), int(m), s
write(*,*) '--'

end subroutine test_preces

program main

use const_module

implicit none

call test_preces(J2000 + 365.25d0, 0.d0, 0.d0)
call test_preces(2459875.589308d0, 6.98692617821836d0/12.d0*pi, 29.2732734669007d0*deg)

end program main


