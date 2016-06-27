
      SUBROUTINE gcf(gammcf,a,x,gln)
      INTEGER ITMAX
      REAL*8 a,gammcf,gln,x,EPS,FPMIN
      PARAMETER (ITMAX=100,EPS=3.d-7,FPMIN=1.d-30)
C USES gammln
c Returns the incomplete gamma function Q(a, x) evaluated by its continued fraction representation
c as gammcf. Also returns ln Γ(a) as gln.
c Parameters: ITMAX is the maximum allowed number of iterations; EPS is the relative accuracy;
c FPMIN is a number near the smallest representable floating-point number.
      INTEGER i
      REAL*8 an,b,c,d,del,h,gammln
      gln=gammln(a)
      b=x+1.d0-a ! Set up for evaluating continued fraction by modified
      c=1.d0/FPMIN ! Lentz’s method (§5.2) with b0 = 0.
      d=1.d0/b
      h=d
      do 11 i=1,ITMAX ! Iterate to convergence.
        an=-i*(i-a)
        b=b+2.d0
        d=an*d+b
        if (abs(d).lt.FPMIN) d=FPMIN
        c=b+an/c
        if (abs(c).lt.FPMIN) c=FPMIN
        d=1.d0/d
        del=d*c
        h=h*del
        if (abs(del-1.d0).lt.EPS) goto 1
11    enddo
      write(*,*) '# gcf: a too large, ITMAX too small in gcf'
      gammcf=0.d0
      return
1     continue
      gammcf=exp(-x+a*log(x)-gln)*h ! Put factors in front.
      return
      END

