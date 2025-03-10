! test_roche.f90
! Test Roche lobe computations.
! Miroslav Broz (miroslav.broz@email.cz), Feb 15th 2022

program test_roche

use const_module
use roche_module

implicit none
integer :: i, n
double precision :: V, R
double precision, parameter :: F1 = 0.1d0, F2 = 1.0d0

open(20, file='test_roche.out', status='unknown')
write(20,*) '# q [1] & F [1] & P [1] & OmegaL1 [1] & OmegaF [1] & V [a=1] & R [a=1]'

! primary
q = 1.3d0
P = 1.0d0

n = 46
do i = 1, n
  F = F1 + (F2-F1)*dble(i-1)/(n-1)
  V = RocheVolume()
  R = (V/(4.d0/3.d0*pi))**(1.d0/3.d0)

! as in kopal3.plt
!  OmegaL1 = OmegaL1 + q**2/(2.d0*(1.d0+q))
!  OmegaF  = OmegaF  + q**2/(2.d0*(1.d0+q))

  write(20,*) q, F, P, RL1, OmegaL1, OmegaF, V, R
enddo

write(20,*)

! secondary
q = 1./q

do i = 0, n
  F = 0.10d0+(1.0d0-0.10d0)*i/n
  V = RocheVolume()
  R = (V/(4.d0/3.d0*pi))**(1.d0/3.d0)

! as in kopal5.plt
!  OmegaL1 = 1.d0/q*(OmegaL1 + q**2/(2.d0*(1.d0+q)))
!  OmegaF  = 1.d0/q*(OmegaF  + q**2/(2.d0*(1.d0+q)))

  OmegaL1 = secondary_correction(OmegaL1, q)
  OmegaF  = secondary_correction(OmegaF, q)

  write(20,*) q, F, P, RL1, OmegaL1, OmegaF, V, R
enddo

close(20)

stop
end program test_roche


