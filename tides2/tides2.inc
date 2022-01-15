c tides2.inc
c Common block for tides and oblatness computations.
c Miroslav Broz (miroslav.broz@email.cz), Sep 2nd 2020

      real*8 k_L(NPLMAX)
      real*8 omega_rot(NPLMAX)
      real*8 R_body(NPLMAX)

      real*8 koef1(NPLMAX)
      real*8 koef2(NPLMAX)
      real*8 R_body5(NPLMAX)

      real*8 external_mass

      logical use_tides2, use_ppn

      common /tides2/ k_L, omega_rot, R_body,
     :  koef1, koef2, R_body5,
     :  external_mass,
     :  use_tides2, use_ppn

