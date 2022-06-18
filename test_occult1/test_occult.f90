! test_occult.f90
! Test occultation subroutine (in RA, DE coordinates).
! Miroslav Broz (miroslav.broz@email.cz), Jun 9th 2022

program test_occult

use occult_module
use const_module
use rotate_module
use paralax_module
use hhms_module

implicit none
double precision :: t0
double precision :: vardist, ecl, ecb, dvardist, decl, decb
double precision :: R_A
double precision :: ra, de, dra, dde
double precision :: ra_A, de_A
double precision :: ra_O, de_O, prlx
double precision :: ra_S, de_S
double precision :: ra_offset, de_offset

integer :: i, n
double precision :: t, lambda, phi, ht, eps
double precision, dimension(3) :: r, r_, r_EA, r_AS, e, axes
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
read(*,*) ra_O
read(*,*) de_O
read(*,*) ra_S
read(*,*) de_S
read(*,*) prlx
read(*,*) ra_offset
read(*,*) de_offset

ra_A = ra_A/12.d0*pi
de_A = de_A*deg
dra = dra/12.d0*pi
dde = dde*deg
ra_O = ra_O/12.d0*pi
de_O = de_O*deg
ra_S = ra_S/12.d0*pi
de_S = de_S*deg
prlx = prlx*mas
ra_offset = ra_offset*mas
de_offset = de_offset*mas

write(*,*) '# t0 = ', t0
call hhms(ra_A/pi*12.d0, h, m, s)
write(*,*) '# ra_A = ', ra_A/pi*12.d0, " h = ", int(h), int(m), s
call hhms(ra_O/pi*12.d0, h, m, s)
write(*,*) '# ra_O = ', ra_O/pi*12.d0, " h = ", int(h), int(m), s
call hhms(dra/pi*12.d0, h, m, s)
write(*,*) '# dra  = ', dra/pi*12.d0, " h = ", int(h), int(m), s
call hhms(de_A/deg, h, m, s)
write(*,*) '# de_A = ', de_A/deg, " h = ", int(h), int(m), s
call hhms(de_O/deg, h, m, s)
write(*,*) '# de_O = ', de_O/deg, " h = ", int(h), int(m), s
call hhms(dde/deg, h, m, s)
write(*,*) '# dde  = ', dde/deg, " h = ", int(h), int(m), s

call paralax(ra_O, de_O, prlx, ra_S, de_S)

r_AS = (/cos(ra_O)*cos(de_O), sin(ra_O)*cos(de_O), sin(de_O)/)

write(*,*) '# t_TDB [jd] & lambda [deg] & phi [deg]'

open(unit=10, file='test_occult.tmp', status='unknown')

n = 300
do i = 0, n

  t = t0 + 0.03d0*(dble(i)/n-0.5d0)

  d = vardist + dvardist*(t-t0)
  ra = ra_A + dra*(t-t0) + ra_offset
  de = de_A + dde*(t-t0) + de_offset

  r_EA = d*(/cos(ra)*cos(de), sin(ra)*cos(de), sin(de)/)

  axes = (/R_E, R_E, R_P/)/au

  call occult(t, r_EA, r_AS, e, axes, lambda, phi, dummy, has_solution)

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


