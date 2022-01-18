c hec88.f
c Use Harmanec (1988) relations to constrain the radius, mass,
c   luminosity, or log g of normal Main-Sequence  stars.
c Miroslav Broz (miroslav.broz@email.cz), May 24 2017

c Reference:
c  Harmanec P.: Stellar masses and radii based on modern binary data.
c  Bull. Astron. Inst. Czechosl. 39, 329-345, 1988.

      subroutine hec88(T_eff, R, M, L, logg)
c input
      real*8 T_eff
c output
      real*8 R, M, L, logg

c Teff ... effective temperature [K]
c R    ... radius [R_S]
c M    ... mass [M_S]
c L    ... luminosity [L_S]
c logg ... radius [cgs]

      real*8 Mbol_Sun
      parameter(Mbol_Sun = 4.69d0)  ! mag; Popper (1980)
c      parameter(Mbol_Sun = 4.75d0)  ! mag

      real*8 X, logM, logR, Mbol

      X = log10(T_eff)

      logM = (((-1.744951d0*X + 30.31681d0)*X - 196.2387d0)*X
     :  + 562.6774d0)*X - 604.0760d0

      logR = (((-0.8656627d0*X + 16.22018d0)*X - 112.2303d0)*X
     :  + 341.6602d0)*X - 387.0969d0

      Mbol = (((4.328314d0*X - 81.10091d0)*X + 561.1516d0)*X
     :  - 1718.301d0)*X + 1977.795d0

c      Mbol = 42.31d0 - 5.d0*logR - 10.d0*X

      logg = 4.438d0 + logM - 2.d0*logR

      R = 10.d0**logR
      M = 10.d0**logM
      L = 10.d0**(0.4d0*(Mbol_Sun-Mbol))

      return
      end


