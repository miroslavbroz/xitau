! read_input.f90
! Read input.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

module read_input_module

contains

subroutine read_input()

use const_module
use input_module
use normalize_module
use lambert_module
use lommel_module
use hapke_module

implicit none
integer :: ierr
character(len=255), parameter :: nml='input.polygon.nml'

open(unit=10, file=nml, status='old', iostat=ierr)
if (ierr.ne.0) then
  write(*,*) "Error opening file '", trim(nml), "'."
  stop
endif
read(10, nml=input)
close(10)

! units
pole_l = pole_l*deg
pole_b = pole_b*deg
phi0 = phi0*deg

! output
write(*,*) '# input parameters:'
write(*,*) 'f_node1 = ', trim(f_node1)
write(*,*) 'f_face1 = ', trim(f_face1)
write(*,*) 'f_node2 = ', trim(f_node2)
write(*,*) 'f_face2 = ', trim(f_face2)
write(*,*) 'unit1 = ', unit1, ' m'
write(*,*) 'unit2 = ', unit2, ' m'
write(*,*) 'pole_l = ', pole_l/deg, ' deg'
write(*,*) 'pole_b = ', pole_b/deg, ' deg'
write(*,*) 'Prot = ', Prot, ' d'
write(*,*) 'Tmin = ', Tmin, ' d'
write(*,*) 'phi0 = ', phi0/deg, ' deg'
write(*,*) 'T_star = ', T_star, ' K'
write(*,*) 'T_eq = ', T_eq, ' K'
write(*,*) 'A_w = ', A_w
write(*,*) 'B0 = ', B0
write(*,*) 'minh = ', minh
write(*,*) 'ming = ', ming
write(*,*) 'bartheta = ', bartheta
write(*,*) 'use_shadowing = ', use_shadowing
write(*,*) 'use_scattering = ', use_scattering
write(*,*) 'use_thermal = ', use_thermal
write(*,*) 'debug = ', debug

if (law(1:2).eq.'La') then
  f_ptr => f_lambert
  write(*,*) 'scattering is Lambert'
elseif (law(1:2).eq.'Lo') then
  f_ptr => f_lommel
  write(*,*) 'scattering is Lommel'
elseif (law(1:2).eq.'Ha') then
  f_ptr => f_hapke
  write(*,*) 'scattering is Hapke'
else
  write(*,*) 'Error: scattering is undefined!'
  stop
endif

write(*,*) ''

end subroutine read_input

end module read_input_module



