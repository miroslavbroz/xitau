c tides.inc
c Common block for tides and oblatness computations.
c Miroslav Broz (miroslav.broz@email.cz), May 25th 2016

      real*8 k_L(NPLMAX)
      real*8 omega_rot(NPLMAX)
      real*8 R_body(NPLMAX)

      real*8 koef1(NPLMAX)
      real*8 koef2(NPLMAX)
      real*8 R_body5(NPLMAX)

      logical use_tides, use_oblatness, use_ppn

      common /tides/ k_L, omega_rot, R_body,
     :  koef1, koef2, R_body5,
     :  use_tides, use_oblatness, use_ppn

