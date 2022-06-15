! test_occult.f90
! Test occultation subroutine (in RA, DE coordinates).
! Miroslav Broz (miroslav.broz@email.cz), Jun 9th 2022

program test_occult

use occult_module
use const_module
use rotate_module
use hhms_module

implicit none
double precision :: t0
double precision :: vardist, ecl, ecb, dvardist, decl, decb
double precision :: R_A
double precision :: ra, de, dra, dde
double precision :: ra_A, de_A
double precision :: ra_S, de_S

integer :: i, n
double precision :: t, lambda, phi, ht, eps
double precision, dimension(3) :: r, r_, r_EA, r_AS, k
double precision :: l, b, d
double precision :: dummy
logical :: has_solution
double precision :: eps_earth
double precision :: h, m, s
character(128) :: fmt

read(*,*) t0
read(*,*) dummy
read(*,*) dummy
read(*,*) dummy
read(*,*) vardist
read(*,*) ra_A
read(*,*) de_A
read(*,*) dvardist
read(*,*) dra
read(*,*) dde
read(*,*) R_A
read(*,*) ra_S
read(*,*) de_S

ra_A = ra_A/12.d0*pi
de_A = de_A*deg
dra = dra/12.d0*pi  ! hours or degrees? cos(de) or NOT?
dde = dde*deg
ra_S = ra_S/12.d0*pi
de_S = de_S*deg

r_AS = (/cos(ra_S)*cos(de_S), sin(ra_S)*cos(de_S), sin(de_S)/)

write(*,*) '# t [jd(TDB)] & lambda [deg] & phi [deg]'

open(unit=10, file='test_occult.tmp', status='unknown')

n = 300
do i = 0, n

  t = t0 + 0.03d0*(dble(i)/n-0.5d0)

  d = vardist + dvardist*(t-t0)
  ra = ra_A + dra*(t-t0)
  de = de_A + dde*(t-t0)

  r_EA = d*(/cos(ra)*cos(de), sin(ra)*cos(de), sin(de)/)

! centre line
! upper edge
! lower edge

  call occult(t, r_EA, r_AS, k, R_E/au, lambda, phi, has_solution)

  if (has_solution) then
    write(*,*) t, lambda/deg, phi/deg
  else
    write(*,*) t, ' ?', ' ?'
  endif

  write(10,*) t, 0.d0, 0.d0, 0.d0, 1
  write(10,*) t, r_EA, 1
  write(10,*)
  write(10,*)

  write(10,*) t, 0.d0, 0.d0, 0.d0, 2
  write(10,*) t, r_AS, 2
  write(10,*)
  write(10,*)

enddo

close(10)

end program test_occult


