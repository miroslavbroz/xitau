! test_roche.f90
! Test Roche lobe computations.
! Miroslav Broz (miroslav.broz@email.cz), Feb 15th 2022

program test_roche

use const_module
use roche_module

implicit none
integer :: i, n
double precision :: V, R

open(20, file='test_roche.out', status='unknown')
write(20,*) '# q [1] & F [1] & P [1] & OmegaL1 [1] & OmegaF [1] & V [a=1] & R [a=1]'

n = 18
do i = 0, n
  q = 1.3d0
  F = 0.10d0+(1.0d0-0.10d0)*i/n
  P = 1.0d0

  V = RocheVolume()
  R = (V/(4.d0/3.d0*pi))**(1.d0/3.d0)

  write(*,*) q, F, P, OmegaL1, OmegaF, V, R
  write(20,*) q, F, P, OmegaL1, OmegaF, V, R
enddo

close(20)

stop
end program test_roche


