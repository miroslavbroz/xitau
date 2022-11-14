! rotate_of_p.f90
! Rotate nodes of polygons; angle a is variable!
! Miroslav Broz (miroslav.broz@email.cz), Nov 11th 2022

module rotate_of_p_module

implicit none

contains

subroutine rot_x_nodes(nodes, a)

double precision, dimension(:,:) :: nodes
double precision, dimension(:) :: a

integer :: i
double precision, dimension(3) :: r, r_
double precision :: c, s

!$omp parallel do private(i,c,s,r,r_) shared(nodes)
do i = 1, size(nodes,1)
  c = cos(a(i))
  s = sin(a(i))
  r = nodes(i,:)
  r_ = rot_x(r, c, s)
  nodes(i,:) = r_
enddo
!$omp end parallel do

end subroutine rot_x_nodes

subroutine rot_y_nodes(nodes, a)

double precision, dimension(:,:) :: nodes
double precision, dimension(:) :: a

integer :: i
double precision, dimension(3) :: r, r_
double precision :: c, s

!$omp parallel do private(i,c,s,r,r_) shared(nodes)
do i = 1, size(nodes,1)
  c = cos(a(i))
  s = sin(a(i))
  r = nodes(i,:)
  r_ = rot_y(r, c, s)
  nodes(i,:) = r_
enddo
!$omp end parallel do

end subroutine rot_y_nodes

subroutine rot_z_nodes(nodes, a)

double precision, dimension(:,:) :: nodes
double precision, dimension(:) :: a

integer :: i
double precision, dimension(3) :: r, r_
double precision :: c, s

!$omp parallel do private(i,c,s,r,r_) shared(nodes)
do i = 1, size(nodes,1)
  c = cos(a(i))
  s = sin(a(i))
  r = nodes(i,:)
  r_ = rot_z(r, c, s)
  nodes(i,:) = r_
enddo
!$omp end parallel do

end subroutine rot_z_nodes

! Reference: https://en.wikipedia.org/wiki/Rotation_matrix
!
! right-handed
! counter-clockwise
! vector (NOT base)

function rot_x(r, c, s)

implicit none
double precision, dimension(3) :: rot_x
double precision, dimension(3), intent(in) :: r
double precision, intent(in) :: c, s

double precision, dimension(3) :: r_

r_(1) = r(1)
r_(2) = r(2)*c - r(3)*s
r_(3) = r(2)*s + r(3)*c

rot_x = r_
return

end function rot_x

function rot_y(r, c, s)

implicit none
double precision, dimension(3) :: rot_y
double precision, dimension(3), intent(in) :: r
double precision, intent(in) :: c, s

double precision, dimension(3) :: r_

r_(1) =  r(1)*c + r(3)*s
r_(2) =  r(2)
r_(3) = -r(1)*s + r(3)*c

rot_y = r_
return

end function rot_y

function rot_z(r, c, s)

implicit none
double precision, dimension(3) :: rot_z
double precision, dimension(3), intent(in) :: r
double precision, intent(in) :: c, s

double precision, dimension(3) :: r_

r_(1) = r(1)*c - r(2)*s
r_(2) = r(1)*s + r(2)*c
r_(3) = r(3)

rot_z = r_
return

end function rot_z



end module rotate_of_p_module

