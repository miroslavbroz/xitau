
      REAL*8 FUNCTION gammq(a,x)
      REAL*8  a,x
C USES gcf,gser
c Returns the incomplete gamma function Q(a, x) ≡ 1 − P(a, x).
      REAL*8 gammcf,gamser,gln
c      if (x.lt.0.d0.or.a.le.0.d0) pause 'bad arguments in gammq'
      if (x.lt.0.d0.or.a.le.0.d0) then
        gammq = 1.0d0
        return
      endif
      if (x.lt.a+1.d0) then ! Use the series representation
        call gser(gamser,a,x,gln)
        gammq=1.d0-gamser ! and take its complement.
      else ! Use the continued fraction representation.
        call gcf(gammcf,a,x,gln)
        gammq=gammcf
      endif
      return
      END

