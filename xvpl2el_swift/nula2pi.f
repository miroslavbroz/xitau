
c**********************************************************************
      double precision function nula2pi(x)
c**********************************************************************
c
c  <0,2pi) truncation
c
      double precision x,frac,y,pi2
      parameter(pi2=6.283185307d0)
      y=frac(x/pi2)*pi2
      if (y.lt.0d0) y=y+pi2
      nula2pi=y
      return
      end 

c**********************************************************************
      double precision function frac(x)
c**********************************************************************
c
c  fractional part of x
c
      double precision x
      frac=x-int(x)
      return
      end
 

