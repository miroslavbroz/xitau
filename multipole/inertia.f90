! inertia.f90
! Compute moments of inertia.
! Miroslav Broz (miroslav.broz@email.cz), Nov 13th 2019

module inertia_module

contains

function inertia(vols, coms)

implicit none

double precision, dimension(6) :: inertia
double precision, dimension(:), pointer :: vols
double precision, dimension(:,:), pointer :: coms

integer :: i
double precision :: dV, Ixx, Iyy, Izz, Iyz, Ixz, Ixy
double precision, dimension(3) :: a, b, c, d, r

Ixx = 0.d0
Iyy = 0.d0
Izz = 0.d0
Iyz = 0.d0
Ixz = 0.d0
Ixy = 0.d0

do i = 1, size(vols)
  r = coms(i,:)
  dV = vols(i)

  Ixx = Ixx + (r(2)*r(2)+r(3)*r(3))*dV
  Iyy = Iyy + (r(1)*r(1)+r(3)*r(3))*dV
  Izz = Izz + (r(1)*r(1)+r(2)*r(2))*dV
  Iyz = Iyz + r(2)*r(3)*dV
  Ixz = Ixz + r(1)*r(3)*dV
  Ixy = Ixy + r(1)*r(2)*dV
enddo

inertia = [Ixx, Iyy, Izz, Iyz, Ixz, Ixy]
return

end function inertia

end module inertia_module

