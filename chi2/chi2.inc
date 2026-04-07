! chi2.inc
! Include file for chi2 subroutines.
! Miroslav Broz (miroslav.broz@email.cz), Aug 19th 2020

      integer NDIMMAX
      parameter(NDIMMAX = 160)		! number of chi^2 dimensions (free parameters)

      integer NBODMAX, OUTMAX1, OUTMAX2, OUTMAX, OBSMAX, TIMEMAX
      parameter (NBODMAX = 5)			! max number of bodies
      parameter (OUTMAX1 = 400000)		! max number of forward integration points
      parameter (OUTMAX2 = 600000)		! max number of backward integration points
      parameter (OUTMAX = OUTMAX1+OUTMAX2)	! max number of integration points
      parameter (OBSMAX = 100000)		! max number of observations
      parameter (TIMEMAX = OBSMAX*(2*NBODMAX+4))	! max number of times of interest

      integer MINMAX, SYNMAX, SEDMAX, AOMAX, OCCMAX, RADMAX
      parameter (MINMAX = OUTMAX)		! max number of minima
      parameter (SYNMAX = 2000000)		! max number of spectra points
      parameter (SEDMAX = 100)			! max number of SED points
      parameter (AOMAX = 3000)			! max number of AO images
      parameter (OCCMAX = 1000)			! max number of occultation events
      parameter (RADMAX = 1000)			! max number of radar images

      integer BANDMAX, FLTMAX
      parameter (BANDMAX = 35)			! max number of photometric bands
      parameter (FLTMAX = 2000)			! max number of filter points

