c eps_earth.f
c Ecliptic inclination wrt. equator.
c Miroslav Broz (miroslav.broz@email.cz), Jan 18th 2022

c Reference: Astronomical almanac, 2010, p. B52.

      real*8 function eps_earth(jd)
      implicit none
      real*8 jd
      include 'const.inc'
      real*8 T,T2,T3,T4,T5

      T = (jd-J2000)/36525.d0
      T2 = T*T
      T3 = T2*T
      T4 = T3*T
      T5 = T4*T

      eps_earth = (23.d0 + 26.d0/60.d0 + (21.406d0 - 46.836769d0*T
     :  - 0.0001831d0*T2 + 0.00200340d0*T3 - 0.576d-6*T4
     :  - 4.34e-8*T5)/3600.d0)*deg

      return
      end

