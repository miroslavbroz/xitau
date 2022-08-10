c tides.inc
c Common variables for tides computations.
c Miroslav Broz (miroslav.broz@email.cz), Nov 7th 2013

      real*8 capR(NPLMAX)
      real*8 k_2(NPLMAX)
      real*8 Delta_t(NPLMAX)
      real*8 MoI(NPLMAX)
      real*8 T_tides(3,NPLMAX)

      logical use_tides
      logical debug_spin

      common /tides/ capR, k_2, Delta_t, MoI, T_tides, use_tides,
     :  debug_spin


