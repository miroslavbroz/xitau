! rotate.f90
! Rotate vectors and shape models.
! Miroslav Broz (miroslav.broz@email.cz), Nov 19th 2019

module rotate_module

implicit none

contains

subroutine rotate_diagonal(nodes, evector)

! Rotate to align moments of inertia (m.o.i.) tensor w. axes.
! nodes ... shape model nodes
! evector ... 3 eigenvectors (from jacobi.f90)

double precision, dimension(:,:) :: nodes, evector

integer :: i, j
double precision, dimension(3) :: r, r_, v

do i = 1, size(nodes,1)
  r = nodes(i,:)

  do j = 1, size(evector,1)
    v = evector(j,:)
    r_(j) = dot_product(r,v)
  enddo

  nodes(i,:) = r_
enddo

return

end subroutine rotate_diagonal


subroutine rot_x_nodes(nodes, a)

double precision, dimension(:,:) :: nodes
double precision :: a

integer :: i
double precision, dimension(3) :: r, r_
double precision :: c, s

c = cos(a)
s = sin(a)
!$omp parallel do private(i,r,r_) shared(nodes,c,s)
do i = 1, size(nodes,1)
  r = nodes(i,:)
  r_ = rot_x(r, c, s)
  nodes(i,:) = r_
enddo
!$omp end parallel do

end subroutine rot_x_nodes

subroutine rot_y_nodes(nodes, a)

double precision, dimension(:,:) :: nodes
double precision :: a

integer :: i
double precision, dimension(3) :: r, r_
double precision :: c, s

c = cos(a)
s = sin(a)
!$omp parallel do private(i,r,r_) shared(nodes,c,s)
do i = 1, size(nodes,1)
  r = nodes(i,:)
  r_ = rot_y(r, c, s)
  nodes(i,:) = r_
enddo
!$omp end parallel do

end subroutine rot_y_nodes

subroutine rot_z_nodes(nodes, a)

double precision, dimension(:,:) :: nodes
double precision :: a

integer :: i
double precision, dimension(3) :: r, r_
double precision :: c, s

c = cos(a)
s = sin(a)
!$omp parallel do private(i,r,r_) shared(nodes,c,s)
do i = 1, size(nodes,1)
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

double precision, dimension(3) :: rot_x
double precision, dimension(3) :: r
double precision :: c, s

double precision, dimension(3) :: r_

r_(1) = r(1)
r_(2) = r(2)*c - r(3)*s
r_(3) = r(2)*s + r(3)*c

rot_x = r_
return

end function rot_x

function rot_y(r, c, s)

double precision, dimension(3) :: rot_y
double precision, dimension(3) :: r
double precision :: c, s

double precision, dimension(3) :: r_

r_(1) =  r(1)*c + r(3)*s
r_(2) =  r(2)
r_(3) = -r(1)*s + r(3)*c

rot_y = r_
return

end function rot_y

function rot_z(r, c, s)

double precision, dimension(3) :: rot_z
double precision, dimension(3) :: r
double precision :: c, s

double precision, dimension(3) :: r_

r_(1) = r(1)*c - r(2)*s
r_(2) = r(1)*s + r(2)*c
r_(3) = r(3)

rot_z = r_
return

end function rot_z

function vaxis_rotate(a, b, c)

! Rotate a about b by c.

use const_module

implicit none
double precision, dimension(3) :: vaxis_rotate
double precision, dimension(3) :: a, b
double precision :: c

double precision :: phi, theta

phi = atan2(b(2),b(1))
theta = acos(b(3))

!write(*,*) 'phi = ', phi/deg, ' deg'
!write(*,*) 'theta = ', theta/deg, ' deg'

!write(*,*) 'a = ', a, ' BEGIN'
a = rot_z(a, cos(-phi), sin(-phi))
!write(*,*) 'a = ', a, ' phi'
a = rot_y(a, cos(-theta), sin(-theta))
!write(*,*) 'a = ', a, ' theta'
a = rot_z(a, cos(c), sin(c))
!write(*,*) 'a = ', a, ' c'
a = rot_y(a, cos(theta), sin(theta))
!write(*,*) 'a = ', a, ' -theta'
a = rot_z(a, cos(phi), sin(phi))
!write(*,*) 'a = ', a, ' -phi'

vaxis_rotate = a
return

end function vaxis_rotate

end module rotate_module

