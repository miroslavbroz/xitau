c const.inc
c Physical constants.
c Miroslav Broz (miroslav.broz@email.cz), Jul 24th 2015

c References:
c
c   IAU 2012 Resolutions
c   IAU 2015 Resolutions
c   Harmanec & Prsa (2011)
c   http://physics.nist.gov/cgi-bin/cuu/Value?h
c   http://physics.nist.gov/cgi-bin/cuu/Value?k

      real*8 pi_, deg, rad
      real*8 c, AU, GM_S, R_S, day, pc, arcsec, mas
      real*8 h, k_B, G
      real*8 eps_J2000

      parameter(pi_ = 4.d0*atan(1.d0))
      parameter(deg = pi_/180.d0, rad = 180.d0/pi_)

      parameter(c = 2.99792458d8)  ! m/s
      parameter(AU = 1.49597870700d11)  ! m, from IAU 2012
      parameter(GM_S = 0.2959122082855911d-03)  ! AU^3/day^2, from JPLEPH DE405
      parameter(R_S = 6.957d8)  ! m, from IAU 2015
      parameter(day = 86400.d0)  ! s
      parameter(pc = 648000.d0/pi_*AU)  ! m, from IAU 2015
      parameter(arcsec = (deg/3600.d0))  ! rad
      parameter(mas = arcsec/1.d3)  ! rad

      parameter(h = 6.626070040d-34)  ! J s, from 2014 CODATA
      parameter(k_B = 1.38064852d-23)  ! J K^-1, from 2014 CODATA
      parameter(G = 6.67430d-11)  ! m^3 kg^-1 s^-2, from 2018 CODATA

      parameter(eps_J2000 = (23.d0+26.d0/60.d0+21.448d0/3600.d0)*deg)

