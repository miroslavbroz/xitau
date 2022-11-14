! uvw.f90
! Compute (u, v, w) coordinates
! Miroslav Broz (miroslav.broz@email.cz), Nov 5th 2022

! Note: see polytype.f90

module uvw_module

double precision, dimension(3) :: hatu, hatv, hatw

contains

subroutine uvw(s, polys1, polys2, equatorial)

use polytype_module
use const_module
use vector_product_module
use rotate_module

implicit none
double precision, dimension(3), intent(in) :: s
type(polystype), dimension(:), pointer, intent(in) :: polys1
type(polystype), dimension(:), pointer, intent(out) :: polys2
logical, optional, intent(in) :: equatorial

integer :: i, j, k
double precision :: l, b, eps, zeta
logical :: e_
double precision, external :: eps_earth

if (present(equatorial)) then
  e_ = equatorial
else
  e_ = .false.
endif

! new basis (as in uvw1.f)
! towards observer
hatw = s
l = atan2(s(2),s(1))
b = asin(s(3))

! in (x,y) plane
!hatu = (/sin(l), -cos(l), 0.d0/)
hatu = (/-sin(l), cos(l), 0.d0/)

! perpendicular, left-handed?!
hatv = vector_product(hatu, hatw)

! ecliptic J2000 -> equatorial J2000
if (e_) then
  eps = eps_earth(j2000)
  zeta = atan2(sin(eps)*cos(l), (cos(b)*cos(eps) - sin(b)*sin(eps)*sin(l)))
  hatu = vaxis_rotate(hatu, hatw, -zeta)
  hatv = vaxis_rotate(hatv, hatw, -zeta)
endif

!write(*,*) 'hatu = ', hatu
!write(*,*) 'hatv = ', hatv
!write(*,*) 'hatw = ', hatw

!$omp parallel do private(i,j,k) shared(polys1,polys2,hatu,hatv,hatw)
do i = 1, size(polys1,1)

  polys2(i)%c = polys1(i)%c

  do j = 1, polys1(i)%c

    polys2(i)%s(j)%c = polys1(i)%s(j)%c

    do k = 1, polys1(i)%s(j)%c

      polys2(i)%s(j)%p(k,1) = dot_product(hatu, polys1(i)%s(j)%p(k,:))
      polys2(i)%s(j)%p(k,2) = dot_product(hatv, polys1(i)%s(j)%p(k,:))
      polys2(i)%s(j)%p(k,3) = dot_product(hatw, polys1(i)%s(j)%p(k,:))

    enddo
  enddo
enddo
!$omp end parallel do

return
end subroutine uvw

subroutine uvw_(normals)

implicit none
double precision, dimension(:,:), pointer, intent(inout) :: normals

integer :: i
double precision :: u, v, w

!$omp parallel do private(i,u,v,w) shared(normals,hatu,hatv,hatw)
do i = 1, size(normals,1)
  u = dot_product(hatu, normals(i,:))
  v = dot_product(hatv, normals(i,:))
  w = dot_product(hatw, normals(i,:))
  normals(i,:) = (/u, v, w/)
enddo
!$omp end parallel do

return
end subroutine uvw_

end module uvw_module


