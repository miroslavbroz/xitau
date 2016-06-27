c simplex.inc
c Include file for simplex subroutines.
c Miroslav Broz (miroslav.broz@email.cz), Jul 24th 2015

      integer NDIMMAX
      parameter(NDIMMAX = 40)		! number of chi^2 dimensions (free parameters)

      integer NBODMAX, OUTMAX1, OUTMAX2, OUTMAX, OBSMAX, MINMAX,
     :  TIMEMAX, BANDMAX, SEDMAX
      parameter (NBODMAX=4)			! max number of bodies
      parameter (OUTMAX1=100000)		! max number of forward integration points
      parameter (OUTMAX2=200000)		! max number of backward integration points
      parameter (OUTMAX=OUTMAX1+OUTMAX2)        ! max number of integration points
      parameter (OBSMAX=100000)			! max number of observations
      parameter (MINMAX=OUTMAX)			! max number of minima
      parameter (TIMEMAX=OBSMAX*(2*NBODMAX+4))	! max number of times of interest
      parameter (BANDMAX=5)			! max number of photometric bands
      parameter (SEDMAX=16)			! max number of SED points

