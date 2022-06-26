
      REAL*8 FUNCTION gammp(a,x)
      REAL*8 a,x
C USES gcf,gser
c      Returns the incomplete gamma function P(a, x).
      REAL*8 gammcf,gamser,gln
c      if (x.lt.0.d0.or.a.le.0.d0) pause 'bad arguments in gammp'
      if (x.lt.0.d0.or.a.le.0.d0) then
        gammp=0.d0
        return
      endif
      if (x.lt.a+1.d0) then ! Use the series representation.
        call gser(gamser,a,x,gln)
        gammp=gamser
      else ! Use the continued fraction representation
        call gcf(gammcf,a,x,gln)
        gammp=1.d0-gammcf ! and take its complement.
      endif
      return
      END

