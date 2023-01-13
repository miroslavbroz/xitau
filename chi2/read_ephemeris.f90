! read_ephemeris.f
! Read ephemeris.
! Miroslav Broz (miroslav.broz@email.cz), Aug 24th 2020

module read_ephemeris_module

contains

subroutine read_ephemeris(filename, n, t, vardist, l, b)

use const_module
use rotate_module

implicit none
include 'chi2.inc'
include 'dependent.inc'

character(len=*), intent(in) :: filename
integer, intent(out) :: n
double precision, dimension(:), intent(out) :: t, vardist, l, b

integer :: i, ierr, iu=10 
character(len=255) :: str
double precision :: eps, alpha, delta, tmp
double precision, dimension(3) :: r, r_

integer, external :: length
double precision, external :: eps_earth

if (filename(1:1).eq.'-') then
  n = 0
  return
endif

i = 0
open(unit=iu,file=filename,status="old",form="formatted", iostat=ierr)
if (ierr.ne.0) then
  write(*,*) "read_ephemeris.f: Error opening file '", trim(filename), "'."
  stop
endif

i = 0
ierr = 0
do while (ierr.eq.0)
  read(iu, '(a)', iostat=ierr) str
  if (ierr.eq.0) then
    if ((str(1:1).ne.'#').and.(length(str).gt.0)) then
      i = i+1
      if (i.gt.OBSMAX) then
        write(*,*) "read_ephemeris.f: Error number of data .gt. OBSMAX = ", OBSMAX
        stop
      endif
      read(str,*,iostat=ierr) t(i), vardist(i), alpha, delta

      alpha = alpha*deg
      delta = delta*deg

! equatorial J2000 -> ecliptic J2000
      eps = eps_earth(j2000)
      r(1) = cos(alpha)*cos(delta)
      r(2) = sin(alpha)*cos(delta)
      r(3) = sin(delta)
      r_ = rot_x(r, cos(-eps), sin(-eps))
      tmp = atan2(r_(2),r_(1))
      if (tmp.lt.0.d0) tmp = tmp+2.d0*pi
      l(i) = tmp
      b(i) = asin(r_(3))
    endif
  endif
enddo
close(iu)

n = i

if (debug_swift) then
  open(unit=iu, file='ephemeris.tmp', access='append')
  write(iu,*) '# file = ', trim(filename)
  write(iu,*) '# JD [TDB] & d [au] & l_j2000 [deg] & b_j2000 [deg]'
  do i = 1, n
    write(iu,*) t(i), vardist(i), l(i)/deg, b(i)/deg
  enddo
  write(iu,*)
  close(iu)
endif

return
end subroutine read_ephemeris

end module read_ephemeris_module


