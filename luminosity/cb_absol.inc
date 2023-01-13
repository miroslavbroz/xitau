c cb_absol.inc
c Absolute synthetic spectra shared by luminosity_synthetic_*.f subroutines.
c Miroslav Broz (miroslav.broz@email.cz), Jul 27th 2017

      integer n_absol(NBODMAX)
      real*8 lambda_absol(OBSMAX,NBODMAX), flux_absol(OBSMAX,NBODMAX)

      common /cb_absol/ lambda_absol, flux_absol, n_absol

