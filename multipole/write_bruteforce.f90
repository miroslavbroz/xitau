! write_bruteforce.f90
! Write parameters to bruteforce.in.
! Miroslav Broz (miroslav.broz@email.cz), Aug 19th 2020

module write_bruteforce_module

contains

subroutine write_bruteforce(f, f_elem, f_face, f_node, capm, unit, P, Tmin, pole_l, pole_b, phi0)

use const_module

implicit none

character(len=*) :: f
character(len=*) :: f_elem, f_face, f_node
double precision :: capm, unit, P, Tmin, pole_l, pole_b, phi0

integer :: iu, ierr

open(unit=10, file=f, status='unknown', iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file "', trim(f), '".'
  stop
endif

write(10,*,err=900) f_elem
write(10,*,err=900) f_face
write(10,*,err=900) f_node
write(10,*,err=900) capm, '  ! M [kg]'
write(10,*,err=900) unit, '  ! unit [m]'
write(10,*,err=900) P/day, '  ! P [day]'
write(10,*,err=900) Tmin, '  ! Tmin [JD]; TDB (not UTC)'
write(10,*,err=900) pole_l/deg, '  ! pole_l [deg]'
write(10,*,err=900) pole_b/deg, '  ! pole_b [deg]'
write(10,*,err=900) phi0/deg, '  ! phi0 [deg]'

close(10)

return

900 continue
write(*,'(a,a,a)') '# Error: writing file "', trim(f), '".'
stop

end subroutine write_bruteforce

end module write_bruteforce_module


