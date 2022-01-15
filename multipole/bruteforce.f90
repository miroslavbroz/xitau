! bruteforce.f90
! Compute gravitational potential by a brute-force algorithm.
! Miroslav Broz (miroslav.broz@email.cz), Nov 18th 2019

module bruteforce_module

contains

double precision function potential_bf(vols, coms, rho, r)

use const_module

implicit none

double precision, dimension(:), pointer :: vols
double precision, dimension(:,:), pointer :: coms
double precision :: rho
double precision, dimension(3) :: r

integer :: i
double precision :: tmp, U
double precision, dimension(3) :: r_

U = 0.d0

!$omp parallel do private(i,r_,tmp) shared(vols,coms,r) reduction(+:U)
do i = 1, size(vols)
  r_ = r - coms(i,:)
  tmp = sqrt(dot_product(r_, r_))
  U = U + vols(i)/tmp
enddo
!$omp end parallel do

potential_bf = -U*G*rho
return

end function potential_bf


function a_g_bf(vols, coms, rho, r)

use const_module
use omp_lib

implicit none

double precision, dimension(3) :: a_g_bf
double precision, dimension(:), pointer :: vols
double precision, dimension(:,:), pointer :: coms
double precision :: rho
double precision, dimension(3) :: r

integer :: i
double precision :: tmp
double precision, dimension(3) :: r_, a_g

a_g = 0.d0

!$omp parallel do private(i,r_,tmp) shared(vols,coms,r) reduction(+:a_g)
do i = 1, size(vols)
  r_ = r - coms(i,:)
  tmp = dot_product(r_, r_)
  a_g = a_g - vols(i)/(tmp*sqrt(tmp))*r_
enddo
!$omp end parallel do

a_g_bf = a_g*G*rho
return

end function a_g_bf

end module bruteforce_module


