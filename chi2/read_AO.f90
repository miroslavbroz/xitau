! read_AO.f
! Read adaptive-optics data.
! Miroslav Broz (miroslav.broz@email.cz), Aug 20th 2022

module read_AO_module

contains

subroutine read_AO(filename, n, t, sigma, pixel_scale, vardist, l, b, dataset, file_OBS)

use const_module
use read_ephemeris_module

implicit none
include 'chi2.inc'
include 'dependent.inc'

character(len=*), intent(in) :: filename
integer, intent(out) :: n
integer, dimension(AOMAX), intent(out) :: dataset
double precision, dimension(AOMAX), intent(out) :: t, sigma, pixel_scale, vardist, l, b
character(len=255), dimension(AOMAX), intent(out) :: file_OBS

double precision, dimension(AOMAX) :: t_
double precision :: rho,theta
integer :: i, length, ierr
character(len=255) :: str

if (filename(1:1).eq.'-') then
  n = 0
  return
endif

i = 0
open(unit=10,file=filename,status="old",form="formatted", iostat=ierr)
if (ierr.ne.0) then
  write(*,*) "read_AO.f: Error opening file '", trim(filename), "'."
  stop
endif

i = 0
ierr = 0
do while (ierr.eq.0)
  read(10, '(a)', iostat=ierr) str
  if (ierr.eq.0) then
    if ((str(1:1).ne."#").and.(length(str).gt.0)) then
      i = i + 1
      if (i.gt.AOMAX) then
        write(*,*) "read_AO.f: Error number of observations .gt. AOMAX = ", AOMAX
        stop
      endif
      read(str, *, iostat=ierr) t(i), sigma(i), pixel_scale(i), dataset(i), file_OBS(i)
    endif
  endif
enddo

close(10)

n = i

if (use_vardist) then
  str = filename(1:length(filename)-4) // '.eph'
  call read_ephemeris(str, i, t_, vardist, l, b)
endif
if (i.ne.n) then
  write(*,*) "read_AO.f: Error number of ephmerides .ne. ", n
  stop
endif

return
end subroutine read_AO

end module read_AO_module


