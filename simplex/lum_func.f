c lum_func.f
c Luminosity as a function of lambda, T and R.
c Miroslav Broz (miroslav.broz@email.cz), Jun 24th 2016

      real*8 function lum_func(lambda)
      implicit none
      include '../misc/const.inc'
      real*8 lambda
      real*8 planck
      real*8 T, R
      common /cb_Lum/ T, R

      lum_func = pi_*planck(T,lambda) * 4.d0*pi_*R**2

      return
      end

