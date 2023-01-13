! convolve.f90
! Convolve a PNM image with a PSF (direct space).
! Miroslav Broz (miroslav.broz@email.cz), Jan 9th 2023

module convolve_module

contains

subroutine convolve(a, psf, b)

implicit none
double precision, dimension(:,:), pointer, intent(in) :: a, psf
double precision, dimension(:,:), pointer, intent(out) :: b

integer :: i, j, k, l
integer :: m, n
double precision :: s

! Note: Conditions for the indices in the inner (PSF) cycle:
!
! i+k-K/2 >= 1
! i+k-K/2 <= I
! j+l-L/2 >= 1
! j+l-L/2 <= J

do i = 1, size(a,1)
  do j = 1, size(a,2)

    s = 0.d0
    do k = max(1, 1-i+size(psf,1)/2), min(size(psf,1), size(a,1)+0-i+size(psf,1)/2)
      do l = max(1, 1-j+size(psf,2)/2), min(size(psf,2), size(a,2)+0-j+size(psf,2)/2)

        s = s + a(i+k-size(psf,1)/2, j+l-size(psf,2)/2) * psf(k,l)

      enddo
    enddo
    b(i,j) = s

  enddo
enddo

return
end subroutine convolve

end module convolve_module


