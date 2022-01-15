! read_multipole.f90
! Read parameters from multipole.in.
! Miroslav Broz (miroslav.broz@email.cz), Aug 19th 2020

module read_multipole_module

contains

subroutine read_multipole(f, Clm, Slm, capm, capr, P, Tmin, pole_l, pole_b, phi0, maxl)

use const_module

implicit none

character(len=*) :: f
integer, parameter :: npole = 10
integer :: maxl
double precision, dimension(0:npole,0:npole) :: Clm, Slm
double precision :: capm, capr, P, Tmin, pole_l, pole_b, phi0

integer :: iu, ierr
integer :: l, m, l_, m_

open(unit=10, file=f, status='old', iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file ', trim(f)
  stop
endif

do l = 0, size(Clm,1)-1
  do m = 0, l
    read(10,*,err=900,end=900) l_, m_, Clm(l,m)
    if ((l_.ne.l).or.(m_.ne.m)) goto 900
    if (m.gt.0) then
      read(10,*,err=900,end=900) l_, m_, Slm(l,m)
      if ((l_.ne.l).or.(m_.ne.m)) goto 900
    endif
  enddo
enddo

read(10,*,err=900,end=900) capm
read(10,*,err=900,end=900) capr
read(10,*,err=900,end=900) P
read(10,*,err=900,end=900) Tmin
read(10,*,err=900,end=900) pole_l
read(10,*,err=900,end=900) pole_b
read(10,*,err=900,end=900) phi0
read(10,*,err=900,end=900) maxl

close(10)

! unit conversion
pole_l = pole_l*deg
pole_b = pole_b*deg
phi0 = phi0*deg

! write parameters
write(*,*) '# f = ', trim(f)
write(*,*) '# npole = ', npole
write(*,*) '# M = ', capm, ' kg'
write(*,*) '# R = ', capr, ' m'
write(*,*) '# P = ', P, ' day'
write(*,*) '# Tmin = ', Tmin, ' JD'
write(*,*) '# pole_l = ', pole_l/deg, ' deg'
write(*,*) '# pole_b = ', pole_b/deg, ' deg'
write(*,*) '# phi0 = ', phi0/deg, ' deg'
write(*,*) '# maxl = ', maxl

return

900 continue
write(*,'(a,a,a)') '# Error: reading file "', f, '".'
write(*,*) 'l = ', l, ', m = ', m
stop

end subroutine read_multipole

end module read_multipole_module


