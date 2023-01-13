! psf.f90
! Point Spread Function.
! Miroslav Broz (miroslav.broz@email.cz), Jan 9th 2023

! Note: In order to have a 1-pixel precision, PSF's aren't centered!

! i .. row
! j .. column

!     1   2   3   4
!   -----------------
! 1 |   |   |   |   |
!   -----------------
! 2 |   | C |   |   |
!   -----------------
! 3 |   |   |   |   |
!   -----------------
! 4 |   |   |   |   |
!   -----------------

! Note: Alternatively, FFT routines use a wrap-around ordering.

module psf_module

contains

! 1-pixel PSF.

subroutine psf_1(psf)

implicit none
double precision, dimension(:,:), pointer, intent(inout) :: psf

psf = 0.d0
psf(size(psf,1)/2, size(psf,2)/2) = 1.d0

return
end subroutine psf_1

! Gaussian PSF.

subroutine psf_gauss(sigma, psf)

use const_module

implicit none
double precision, intent(in) :: sigma
double precision, dimension(:,:), pointer, intent(inout) :: psf

integer :: i, j, w, h
double precision :: u, v, rsq

h = size(psf,1)
w = size(psf,2)

do i = 1, h
  do j = 1, w
    u = -0.5d0*w + j
    v =  0.5d0*h - i
    rsq = u*u + v*v

    psf(i,j) = 1.d0/(sqrt(2.d0*pi)*sigma) * exp(-rsq/(2.d0*sigma**2))

  enddo
enddo

psf = psf/sum(psf)  ! re-normalisation due to discretisation

return
end subroutine psf_gauss

! Moffat PSF.

! Reference: https://en.wikipedia.org/wiki/Moffat_distribution

subroutine psf_moffat(alpha, beta, psf)

use const_module

implicit none
double precision, intent(in) :: alpha, beta
double precision, dimension(:,:), pointer, intent(inout) :: psf

integer :: i, j, w, h
double precision :: u, v, rsq

h = size(psf,1)
w = size(psf,2)

do i = 1, h
  do j = 1, w
    u = -0.5d0*w + j
    v =  0.5d0*h - i
    rsq = u*u + v*v

    psf(i,j) = (beta-1)/(pi*alpha**2) * (1.d0 + rsq/alpha**2)**(-beta)

  enddo
enddo

psf = psf/sum(psf)

end subroutine psf_moffat

end module psf_module


