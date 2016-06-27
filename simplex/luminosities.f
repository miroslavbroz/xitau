c luminosities.f
c Compute luninosities from temperatures and radii.
c Miroslav Broz (miroslav.broz@email.cz), Apr 1st 2016

      subroutine luminosities(T_eff, R_star, nbod, lambda, band,
     :  Lum, Lumtot, use_planck)

      implicit none
      include '../misc/const.inc'
c input
      integer nbod
      real*8 T_eff(nbod), R_star(nbod), lambda, band
      logical use_planck
c output
      real*8 Lum(nbod), Lumtot

      if (use_planck) then
        call luminosity_planck_bandpass(T_eff, R_star, nbod,
     :    lambda, band, Lum, Lumtot)
      else
        call luminosity_synthetic_bandpass(T_eff, R_star, nbod,
     :    lambda, band, Lum, Lumtot)
      endif

      return
      end


