c cb_absol.inc
c Absolute synthetic spectra shared by luminosity_synthetic_*.f subroutines.
c Miroslav Broz (miroslav.broz@email.cz), Jul 27th 2017

      integer n_absol(NBODMAX)
      real*8 lambda_absol(SYNMAX,NBODMAX), flux_absol(SYNMAX,NBODMAX)

      common /cb_absol/ lambda_absol, flux_absol, n_absol

