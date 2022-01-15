! dipole.f90
! Compute dipole (centre of mass).
! Miroslav Broz (miroslav.broz@email.cz), Nov 13th 2019

module dipole_module

contains

function dipole(vols, coms)

implicit none

double precision, dimension(3) :: dipole
double precision, dimension(:), pointer :: vols
double precision, dimension(:,:), pointer :: coms

integer :: i

dipole = 0.d0

do i = 1, size(vols,1)
  dipole = dipole + coms(i,:)*vols(i)
enddo

return

end function dipole

end module dipole_module

