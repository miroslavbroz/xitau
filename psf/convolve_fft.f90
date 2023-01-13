! convolve_fft.f90
! Convolve a PNM image with a PSF (fast Fourier transform).
! Miroslav Broz (miroslav.broz@email.cz), Jan 9th 2023

module convolve_fft_module

contains

subroutine convolve_fft(a, psf, b)

use realft2_module

implicit none
double precision, dimension(:,:), pointer, intent(in) :: a, psf
double precision, dimension(:,:), pointer, intent(out) :: b

double complex, dimension(:,:), allocatable :: spec, spec_psf
double complex, dimension(:), allocatable :: speq, speq_psf
double precision :: tmp

if ((size(a,1).ne.size(psf,1)).or.(size(a,2).ne.size(psf,2))) then
  write(*,*) 'convolve_fft: Error sizes of a, psf do not match!'
  stop
endif

allocate(spec(size(a,1)/2, size(a,2)))
allocate(speq(size(a,2)))
allocate(spec_psf(size(psf,1)/2, size(psf,2)))
allocate(speq_psf(size(psf,2)))

call realft2(a, spec, speq, 1)
call realft2(psf, spec_psf, speq_psf, 1)

tmp = 2.d0/(size(a,1)*size(a,2))
spec = tmp * spec * spec_psf
speq = tmp * speq * speq_psf

call realft2(b, spec, speq, -1)

deallocate(spec)
deallocate(speq)
deallocate(spec_psf)
deallocate(speq_psf)

return
end subroutine convolve_fft

end module convolve_fft_module


