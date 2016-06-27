c limcof.inc
c Include file for limb-darkening tables.
c Miroslav Broz (miroslav.broz@email.cz), Jun 2nd 2016

      integer LIMBMAX, AVAILMAX, LAMBDAMAX
      parameter (LIMBMAX = 200000)
      parameter (AVAILMAX = 10000)
      parameter (LAMBDAMAX = 16)

      integer n_limb, n_avail

      real*8 lambda_limb(LIMBMAX)
      real*8 Teff_limb(LIMBMAX)
      real*8 logg_limb(LIMBMAX)
      real*8 Z_limb(LIMBMAX)
      real*8 u_limb(LIMBMAX)

      real*8 Teff_avail(AVAILMAX)
      real*8 logg_avail(AVAILMAX)
      real*8 Z_avail(AVAILMAX)

      common /limcof/ lambda_limb, Teff_limb, logg_limb, Z_limb, u_limb,
     :  Teff_avail, logg_avail, Z_avail,
     :  n_limb, n_avail

