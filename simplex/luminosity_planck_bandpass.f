c luminosity_planck_bandpass.f
c Compute luninosities from temperatures and radii, assuming simple Planck law
c   and effective wavelength/bandpass.
c Miroslav Broz (miroslav.broz@email.cz), Apr 1st 2016

      subroutine luminosity_planck_bandpass(T_eff, R_star, nbod,
     :  lambda, band, Lum, Lumtot)

      implicit none
      include '../misc/const.inc'
c input
      integer nbod
      real*8 T_eff(nbod), R_star(nbod), lambda, band
c output
      real*8 Lum(nbod), Lumtot
c internal
      integer k
      real*8 eps
c functions
      external lum_func
c a common block due to integration by NR routines
      real*8 T, R
      common /cb_Lum/ T, R

      data eps /1.d-6/

      Lumtot = 0.d0

      do k = 1, nbod
        T = T_eff(k)
        R = R_star(k)*R_S

        if (T.gt.0.d0) then
          call qtrap(lum_func, lambda-band/2.d0, lambda+band/2.d0,
     :      eps, Lum(k))
        else
          Lum(k) = 0.d0
        endif

        Lumtot = Lumtot + Lum(k)
      enddo

      return
      end


