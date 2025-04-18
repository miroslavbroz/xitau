! test_hapke.f90
! Test Hapke scattering law.
! Miroslav Broz (miroslav.broz@email.cz), Nov 18th 2022
 
program test_hapke

use const_module
use input_module
use hapke_module

implicit none
integer :: i, j, n, m
double precision :: mu_i, mu_e, alpha, val, f_L, A_w_
double precision :: cospsi, psi, cosi, cose, sini, sine

! Spjuth (2009), Fig. 2.10
A_w_ = 0.23d0
B0 = 1.32d0
minh = 0.20d0
ming = -0.35d0

mu_i = 0.3d0; mu_e = 0.3d0
!mu_i = 0.8d0; mu_e = 0.8d0

f_L = A_w_/(4.d0*pi)

write(*,*) '# mu_i mu_e alpha psi bartheta f_hapke Sr mu_i_ mu_e_'
write(*,*) '# 1 1 deg deg deg 1'

m = 180
do i = 1, 3

  if (i.eq.1) bartheta = 5.d0*deg
  if (i.eq.2) bartheta = 20.d0*deg
  if (i.eq.3) bartheta = 40.d0*deg

  do j = 0, m

    alpha = dble(j)/m * pi
    cosi = mu_i
    cose = mu_e
    sini = sqrt(1.d0-cosi**2)
    sine = sqrt(1.d0-cose**2)
    cospsi = (cos(alpha)-cosi*cose)/(sini*sine)
    psi = acos(min(cospsi, 1.d0))

    call init_hapke(alpha)

    val = f_hapke(f_L, mu_i, mu_e, alpha)

    write(*,*) mu_i, mu_e, alpha/deg, psi/deg, bartheta/deg, val, Stmp, mu_i_, mu_e_

  enddo
  write(*,*)
enddo

end program test_hapke

