! rotate.f90
! Rotation matrices (as defined in Wolf et al. 1992).
! Miroslav Broz (miroslav.broz@email.cz), Jun 17th 2022

! Note: in ../multipole/rotate.f90 the sign is opposite!!

module rotateinv_module

contains

function rot_x(r, c, s)

implicit none
double precision, dimension(3) :: rot_x
double precision, dimension(3), intent(in) :: r
double precision, intent(in) :: c, s

double precision, dimension(3) :: r_

r_(1) =  r(1)
r_(2) =  r(2)*c + r(3)*s
r_(3) = -r(2)*s + r(3)*c

rot_x = r_
return

end function rot_x

function rot_y(r, c, s)

implicit none
double precision, dimension(3) :: rot_y
double precision, dimension(3), intent(in) :: r
double precision, intent(in) :: c, s

double precision, dimension(3) :: r_

r_(1) = r(1)*c - r(3)*s
r_(2) = r(2)
r_(3) = r(1)*s + r(3)*c

rot_y = r_
return

end function rot_y

function rot_z(r, c, s)

implicit none
double precision, dimension(3) :: rot_z
double precision, dimension(3), intent(in) :: r
double precision, intent(in) :: c, s

double precision, dimension(3) :: r_

r_(1) =  r(1)*c + r(2)*s
r_(2) = -r(1)*s + r(2)*c
r_(3) =  r(3)

rot_z = r_
return

end function rot_z

end module rotateinv_module


