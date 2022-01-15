! test_legendre.f90
! Test Legendre polynomials computation.
! Miroslav Broz (miroslav.broz@email.cz), Aug 21st 2020

program test_legendre

use legendre_module
use legendre2_module, Plm2 => Plm, dPlm_dx2 => dPlm_dx

implicit none

! constants
integer, parameter :: npole = 10
integer, parameter :: neval = 10000

integer :: j, l, m
double precision :: costheta, Plm_, dPlm_dx_, Plm2_, dPlm_dx2_
real :: t1, t2

costheta = 0.5d0

write(*,*) 'legendre:'
call cpu_time(t1)

do j = 1, neval
  do l = 0, npole
    do m = 0, l
      Plm_ = Plm(l,m,costheta)
      dPlm_dx_ = dPlm_dx(l,m,costheta,Plm_)
    enddo
  enddo
enddo

call cpu_time(t2)
write(*,*) 'cpu_time = ', t2-t1, ' s'

write(*,*) 'legendre2:'
call cpu_time(t1)

call Plm_init(costheta)

do j = 1, neval
  do l = 0, npole
    do m = 0, l
      Plm2_ = Plm2(l,m,costheta)
      dPlm_dx2_ = dPlm_dx2(l,m,costheta,Plm_)
    enddo
  enddo
enddo

call cpu_time(t2)
write(*,*) 'cpu_time = ', t2-t1, ' s'

do l = 0, npole
  do m = 0, l
    Plm_ = Plm(l,m,costheta)
    Plm2_ = Plm2(l,m,costheta)
    write(*,*) 'l = ', l, ' m = ', m, ' Plm/Plm2 = ', Plm_/Plm2_
  enddo
enddo

write(*,*)

do l = 0, npole
  do m = 0, l
    Plm_ = Plm(l,m,costheta)
    Plm2_ = Plm2(l,m,costheta)
    dPlm_dx_ = dPlm_dx(l,m,costheta,Plm_)
    dPlm_dx2_ = dPlm_dx2(l,m,costheta,Plm2_)
    write(*,*) 'l = ', l, ' m = ', m, ' dPlm_dx/dPlm_dx2 = ', dPlm_dx_/dPlm_dx2_
  enddo
enddo

end program test_legendre


