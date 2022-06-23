! simplex.inc
! Include file for simplex subroutines.
! Miroslav Broz (miroslav.broz@email.cz), Aug 19th 2020

      integer NDIMMAX
      parameter(NDIMMAX = 50)		! number of chi^2 dimensions (free parameters)

      integer NBODMAX, OUTMAX1, OUTMAX2, OUTMAX, OBSMAX, MINMAX
      integer TIMEMAX, BANDMAX, SEDMAX, FLTMAX, AOMAX, OCCMAX
      parameter (NBODMAX=5)			! max number of bodies
      parameter (OUTMAX1=400000)		! max number of forward integration points
      parameter (OUTMAX2=600000)		! max number of backward integration points
      parameter (OUTMAX=OUTMAX1+OUTMAX2)        ! max number of integration points
      parameter (OBSMAX=200000)			! max number of observations
      parameter (MINMAX=OUTMAX)			! max number of minima
      parameter (TIMEMAX=OBSMAX*(2*NBODMAX+4))	! max number of times of interest
      parameter (BANDMAX=5)			! max number of photometric bands
      parameter (SEDMAX=100)			! max number of SED points
      parameter (FLTMAX=2000)			! max number of filter points
      parameter (AOMAX=3000)			! max number of AO images
      parameter (OCCMAX=1000)			! max number of occultations

