! dobrovolskis.f90
! Moments of inertia of tetrahedral mesh; exact vers.
! Miroslav Broz (miroslav.broz@email.cz), Feb 25th 2023

! Reference: Dobrovolskis (1995), Icarus 124, 698-704.

module dobrovolskis_module

contains

function dobrovolskis(faces, nodes)

use volume_module

implicit none
double precision, dimension(6) :: dobrovolskis
integer, dimension(:,:), pointer, intent(in) :: faces
double precision, dimension(:,:), pointer, intent(in) :: nodes

integer :: i, j, k
double precision :: dV, tmp
double precision :: Ixx, Iyy, Izz, Iyz, Ixz, Ixy
double precision, dimension(3) :: c, d, e, f
double precision, dimension(3,3) :: P

do i = 1, size(faces,1)

  c = (/0.d0, 0.d0, 0.d0/)
  d = nodes(faces(i,1),:)
  e = nodes(faces(i,2),:)
  f = nodes(faces(i,3),:)

  dV = v1(c, d, e, f)

  do j = 1, 3
    do k = 1, 3
      if (k.ge.j) then
        tmp = 2.d0*d(j)*d(k) + 2.d0*e(j)*e(k) + 2.d0*f(j)*f(k)
        tmp = tmp + d(j)*e(k) + d(k)*e(j) + d(j)*f(k) + d(k)*f(j) + e(j)*f(k) + e(k)*f(j)
        P(j,k) = P(j,k) + dV/20.d0 * tmp
      endif
    enddo
  enddo
  
enddo

Ixx = P(2,2) + P(3,3)
Iyy = P(1,1) + P(3,3)
Izz = P(1,1) + P(2,2)
Iyz = P(2,3)
Ixz = P(1,3)
Ixy = P(1,2)

dobrovolskis = [Ixx, Iyy, Izz, Iyz, Ixz, Ixy]

end function dobrovolskis

end module dobrovolskis_module


