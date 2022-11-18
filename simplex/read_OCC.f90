! read_OCC.f90
! Read occultation data.
! Miroslav Broz (miroslav.broz@email.cz), Jun 22nd 2022

module read_OCC_module

contains

subroutine read_OCC(filename, n, t, sigma, lambda, phi, alt, &
  alpha, delta, prlx, pmra, pmde, epoch, offra, offde, contact, dataset)

use const_module

implicit none
include 'simplex.inc'

character(len=*), intent(in) :: filename
integer, intent(out) :: n
double precision, dimension(OCCMAX), intent(out) :: t, sigma, lambda, phi, alt, &
  alpha, delta, prlx, pmra, pmde, epoch, offra, offde
integer, dimension(OCCMAX), intent(out) :: contact, dataset

integer :: i,length,ierr
character(len=255) :: str

if (filename(1:1).eq.'-') then
  n = 0
  return
endif

open(unit=10,file=filename,status="old",form="formatted", iostat=ierr)
if (ierr.ne.0) then
  write(*,*) "read_OCC.f: Error opening file '", trim(filename), "'."
  stop
endif

i = 0
ierr = 0
do while (ierr.eq.0)
  read(10, '(a)', iostat=ierr) str
  if (ierr.eq.0) then
    if ((str(1:1).ne."#").and.(length(str).gt.0)) then
      i = i + 1
      if (i.gt.OCCMAX) then
        write(*,*) "read_OCC.f: Error number of observations .gt. OCCMAX = ", OCCMAX
        stop
      endif
      read(str, *, iostat=ierr) t(i), sigma(i), lambda(i), phi(i), alt(i), &
        contact(i), alpha(i), delta(i), prlx(i), pmra(i), pmde(i), epoch(i), &
        offra(i), offde(i), dataset(i)

      lambda(i) = lambda(i)*deg
      phi(i) = phi(i)*deg
      alt(i) = alt(i)*1.d3/au
      alpha(i) = alpha(i)*deg
      delta(i) = delta(i)*deg
      prlx(i) = prlx(i)*mas
      pmra(i) = pmra(i)*mas/365.25d0
      pmde(i) = pmde(i)*mas/365.25d0
      offra(i) = offra(i)*mas
      offde(i) = offde(i)*mas
    endif
  endif
enddo

close(10)

n = i

return
end subroutine read_OCC

end module read_OCC_module


