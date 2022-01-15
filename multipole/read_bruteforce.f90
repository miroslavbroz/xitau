! read_bruteforce.f90
! Read parameters from bruteforce.in.
! Miroslav Broz (miroslav.broz@email.cz), Aug 19th 2020

module read_bruteforce_module

contains

subroutine read_bruteforce(f, f_elem, f_face, f_node, capm, unit, P, Tmin, pole_l, pole_b, phi0)

use const_module

implicit none

character(len=*) :: f
character(len=255) :: f_elem, f_face, f_node
double precision :: capm, unit, P, Tmin, pole_l, pole_b, phi0

integer :: iu, ierr

open(unit=10, file=f, status='old', iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file "', trim(f), '".'
  stop
endif

read(10,*,err=900,end=900) f_elem
read(10,*,err=900,end=900) f_face
read(10,*,err=900,end=900) f_node
read(10,*,err=900,end=900) capm
read(10,*,err=900,end=900) unit
read(10,*,err=900,end=900) P
read(10,*,err=900,end=900) Tmin
read(10,*,err=900,end=900) pole_l
read(10,*,err=900,end=900) pole_b
read(10,*,err=900,end=900) phi0

close(10)

! unit conversion
pole_l = pole_l*deg
pole_b = pole_b*deg
phi0 = phi0*deg

! write parameters
write(*,*) '# f = ', trim(f)
write(*,*) '# f_elem = ', trim(f_elem)
write(*,*) '# f_face = ', trim(f_face)
write(*,*) '# f_elem = ', trim(f_elem)
write(*,*) '# capm = ', capm, ' kg'
write(*,*) '# unit = ', unit, ' m'
write(*,*) '# P = ', P, ' day'
write(*,*) '# Tmin = ', Tmin, ' JD'
write(*,*) '# pole_l = ', pole_l/deg, ' deg'
write(*,*) '# pole_b = ', pole_b/deg, ' deg'
write(*,*) '# phi0 = ', phi0/deg, ' deg'

return

900 continue
write(*,'(a,a,a)') '# Error: reading file "', trim(f), '".'
stop

end subroutine read_bruteforce

end module read_bruteforce_module


