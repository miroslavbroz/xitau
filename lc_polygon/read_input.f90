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
pole_l_ = pole_l_*deg
pole_b_ = pole_b_*deg
phi0_ = phi0_*deg
bartheta = bartheta*deg

! output
write(*,*) '# f_node1 = ', trim(f_node1)
write(*,*) '# f_face1 = ', trim(f_face1)
write(*,*) '# f_node2 = ', trim(f_node2)
write(*,*) '# f_face2 = ', trim(f_face2)
write(*,*) '# unit1 = ', unit1, ' m'
write(*,*) '# unit2 = ', unit2, ' m'
write(*,*) '# pole_l_ = ', pole_l_/deg, ' deg'
write(*,*) '# pole_b_ = ', pole_b_/deg, ' deg'
write(*,*) '# phi0_ = ', phi0_/deg, ' deg'
write(*,*) '# P_rot = ', P_rot_, ' d'
write(*,*) '# Tmin = ', Tmin, ' d'
write(*,*) '# R_body = ', R_body, ' m'
write(*,*) '# A_w = ', A_w
write(*,*) '# T_star = ', T_star, ' K'
write(*,*) '# T_eq = ', T_eq, ' K'
write(*,*) '# B0 = ', B0
write(*,*) '# minh = ', minh
write(*,*) '# ming = ', ming
write(*,*) '# bartheta = ', bartheta/deg, ' deg'
write(*,*) '# use_shadowing = ', use_shadowing
write(*,*) '# use_scattering = ', use_scattering
write(*,*) '# use_thermal = ', use_thermal
write(*,*) '# debug_polygon = ', debug_polygon

write(*,*) '# Warning: pole_l_ will be overwritten by pole_l!'
write(*,*) '# Warning: pole_b_ will be overwritten by pole_b!'
write(*,*) '# Warning: phi0_ will be overwritten by phi0!'
write(*,*) '# Warning: P_rot_ will be overwritten by P_rot!'
write(*,*) '# Warning: R_body will be overwritten by R_star!'
write(*,*) '# Warning: A_w will be overwritten by albedo!'

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



