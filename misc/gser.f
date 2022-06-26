
      SUBROUTINE gser(gamser,a,x,gln)
      INTEGER ITMAX
      REAL*8 a,gamser,gln,x,EPS
      PARAMETER (ITMAX=100,EPS=3.d-7)
C USES gammln
c Returns the incomplete gamma function P(a, x) evaluated by its series representation as
c gamser. Also returns lnΓ(a) as gln.
      INTEGER n
      REAL*8 ap,del,sum,gammln
      gln=gammln(a)
      if (x.le.0.d0) then
        if (x.lt.0.d0) pause 'x < 0 in gser'
        gamser=0.d0
        return
      endif
      ap=a
      sum=1.d0/a
      del=sum
      do 11 n=1,ITMAX
        ap=ap+1.d0
        del=del*x/ap
        sum=sum+del
        if (abs(del).lt.abs(sum)*EPS) goto 1
11    enddo
      write(*,*) '# gser: a too large, ITMAX too small in gser'
      gamser=0.d0
      return
1     continue
      gamser=sum*exp(-x+a*log(x)-gln)
      return
      END

 
