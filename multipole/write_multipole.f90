
module write_multipole_module

contains

subroutine write_multipole(f, Clm, Slm, capm, capr, P, Tmin, pole_l, pole_b)

use const_module

implicit none

character(*) :: f
double precision, dimension(0:,0:) :: Clm, Slm
double precision :: capm, capr, P, Tmin, pole_l, pole_b

integer :: iu, ierr
integer :: l, m

open(unit=10, file=f, status='unknown', iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file ', f
  stop
endif

do l = 0, size(Clm,1)-1
  do m = 0, l
    write(10,*,err=900) l, m, Clm(l,m)
    if (m.gt.0) then
      write(10,*,err=900) l, m, Slm(l,m)
    endif
  enddo
  write(10,*,err=900)
enddo

write(10,*,err=900) capm, '  ! M [kg]'
write(10,*,err=900) capr, '  ! R [m]'
write(10,*,err=900) P/day, '  ! P [day]'
write(10,*,err=900) Tmin, '  ! Tmin [JD]'
write(10,*,err=900) pole_l/deg, '  ! pole_l [deg]'
write(10,*,err=900) pole_b/deg, '  ! pole_b [deg]'

close(unit=10, iostat=ierr)
if (ierr.ne.0) then
  write(*,'(a,a)') '# Warning: not closing file ', f
endif

return

900 write(*,'(a,a)') '# Error: writing file ', f
stop

end subroutine write_multipole

end module write_multipole_module


