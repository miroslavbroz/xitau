! shadowing.f90
! Compute shadowing.
! Miroslav Broz (miroslav.broz@email.cz), Sep 4th 2020

module shadowing_module

contains

! Compute an shadowing (of faces); convex version.

! n_to ... target-observer
! n_ts ... target-sun

subroutine shadowing(normals, n_to, n_ts, masks)

implicit none

double precision, dimension(:,:), intent(in) :: normals
double precision, dimension(3), intent(in) :: n_to, n_ts
logical, dimension(:), pointer, intent(out) :: masks

integer :: i
double precision :: cosa

masks = .false.

do i = 1, size(normals,1)
  cosa = dot_product(normals(i,:), n_to)
  if (cosa.gt.0.d0) then
    cosa = dot_product(normals(i,:), n_ts)
    if (cosa.gt.0.d0) then
      masks(i) = .true.
    endif
  endif
enddo

end subroutine shadowing

end module shadowing_module


