! read_LC2.f90
! Read lightcurve data (incl. variable geometry).
! Miroslav Broz (miroslav.broz@email.cz), Nov 18th 2022

module read_LC2_module

contains

subroutine read_LC2(filename, n, t, mag, sigma_mag, vardist, l, b, dataset)

implicit none
include 'simplex.inc'
include 'dependent.inc'

character(len=*), intent(in) :: filename
integer, intent(out) :: n
double precision, dimension(OBSMAX), intent(out) :: t, mag, sigma_mag, vardist, l, b
integer, dimension(OBSMAX), intent(out) :: dataset

! temporary
double precision, dimension(OBSMAX) :: t_
integer :: i, ierr
character(len=255) :: str

! functions
integer, external :: length

if (filename(1:1).eq.'-') then
  n = 0
  return
endif

open(unit=10, file=filename, status="old", form="formatted", iostat=ierr)
if (ierr.ne.0) then
  write(*,*) "read_LC.f: Error opening file '", trim(filename), "'."
  stop
endif

i = 0
ierr = 0
do while (ierr.eq.0)
  read(10, '(a)', iostat=ierr) str
  if (ierr.eq.0) then
    if ((str(1:1).ne."#").and.(length(str).gt.0)) then
      i = i + 1
      if (i.gt.OBSMAX) then
        write(*,*) "read_LC2.f: Error number of observations .gt. OBSMAX = ", OBSMAX
        stop
      endif
      read(str, *, iostat=ierr) t(i), mag(i), sigma_mag(i), dataset(i)
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
  write(*,*) n
  write(*,*) "read_LC2.f: Error number of ephemerides .ne. ", n
  stop
endif

return
end subroutine read_LC2

end module read_LC2_module


