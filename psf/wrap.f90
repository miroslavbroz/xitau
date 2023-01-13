! wrap.f90
! Wrap-around order.
! Miroslav Broz (miroslav.broz@email.cz), Jan 11th 2023

module wrap_module

contains

subroutine wrap(psf, psf_)

implicit none
double precision, dimension(:,:), pointer, intent(in) :: psf
double precision, dimension(:,:), pointer, intent(out) :: psf_

integer :: i, j, n, m
double precision :: tmp

n = size(psf,1)
m = size(psf,2)

do i = 1, n
  do j = 1, m
    psf_(mod(n/2+i,n)+1,mod(m/2+j,m)+1) = psf(i,j)
  enddo
enddo

end subroutine wrap

end module wrap_module


