! getacc_test.f90
! Test acceleration computation.
! Miroslav Broz (miroslav.broz@email.cz), Aug 15th 2020

program getacc_test

use const_module
use getacc_bf_module
use getacc_bf2_module
use getacc_mp_module
use getacc_mp2_module

implicit none

include '../chi2/chi2.inc'
include '../chi2/dependent.inc'

!integer, parameter :: NBODMAX = 2

integer :: i, l, m, nbod_
double precision, dimension(NBODMAX) :: mass
double precision, dimension(NBODMAX) :: xb, yb, zb
double precision, dimension(NBODMAX) :: axb, ayb, azb

double precision, dimension(3) :: r, monopole
double precision :: capm, P, time, Tmin, tmp

nbod_ = 2
capm = 4.64d18  ! kg, Descamps etal. (2011)
P = 0.224386764166667d0  ! day

mass(1) = capm/M_S*GM_S
mass(2) = 0.d0

write(*,*) 'mass(1) = ', mass(1)
write(*,*) 'mass(2) = ', mass(2)

xb(1) = 0.d0
yb(1) = 0.d0
zb(1) = 0.d0
xb(2) = 500.d3/au
yb(2) = 0.d0
zb(2) = 0.d0

axb(1) = 0.d0
ayb(1) = 0.d0
azb(1) = 0.d0
axb(2) = 0.d0
ayb(2) = 0.d0
azb(2) = 0.d0

Tmin = 2444502.76914d0
time = Tmin+0.1*P
!time = Tmin
!write(*,*) 'time = ', time

T0 = 0.d0
is_forward = .true.
use_bruteforce = .true.
use_multipole = .true.

write(*,*) 'monopole:'

r(1) = xb(2) - xb(1)
r(2) = yb(2) - yb(1)
r(3) = zb(2) - zb(1)
tmp = dot_product(r, r)
monopole = -mass(1)/(tmp*sqrt(tmp))*r

write(*,*) 'monopole = ', monopole, ' au day^-2'

write(*,*) 'getacc_bf:'

call getacc_bf(time,nbod_,mass,xb,yb,zb,axb,ayb,azb)

write(*,*) 'axb(2) = ', axb(2), ' au d^-2'
write(*,*) 'ayb(2) = ', ayb(2), ' au d^-2'
write(*,*) 'azb(2) = ', azb(2), ' au d^-2'
write(*,*)

axb(1) = 0.d0
ayb(1) = 0.d0
azb(1) = 0.d0
axb(2) = 0.d0
ayb(2) = 0.d0
azb(2) = 0.d0

write(*,*) 'getacc_bf2:'

call getacc_bf2(time,nbod_,mass,xb,yb,zb,axb,ayb,azb)

write(*,*) 'axb(2) = ', axb(2), ' au d^-2'
write(*,*) 'ayb(2) = ', ayb(2), ' au d^-2'
write(*,*) 'azb(2) = ', azb(2), ' au d^-2'
write(*,*)

axb(1) = 0.d0
ayb(1) = 0.d0
azb(1) = 0.d0
axb(2) = 0.d0
ayb(2) = 0.d0
azb(2) = 0.d0

write(*,*) 'getacc_mp:'

call getacc_mp(time,nbod_,mass,xb,yb,zb,axb,ayb,azb)

write(*,*) 'axb(2) = ', axb(2), ' au d^-2'
write(*,*) 'ayb(2) = ', ayb(2), ' au d^-2'
write(*,*) 'azb(2) = ', azb(2), ' au d^-2'
write(*,*)

axb(1) = 0.d0
ayb(1) = 0.d0
azb(1) = 0.d0
axb(2) = 0.d0
ayb(2) = 0.d0
azb(2) = 0.d0

write(*,*) 'getacc_mp2:'

call getacc_mp2(time,nbod_,mass,xb,yb,zb,axb,ayb,azb)

write(*,*) 'axb(2) = ', axb(2), ' au d^-2'
write(*,*) 'ayb(2) = ', ayb(2), ' au d^-2'
write(*,*) 'azb(2) = ', azb(2), ' au d^-2'
write(*,*)

stop

end program getacc_test


