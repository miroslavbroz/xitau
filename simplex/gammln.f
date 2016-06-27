
      REAL*8 FUNCTION gammln(xx)
      REAL*8 xx
C Returns the value ln[ (xx)] for xx > 0.
      INTEGER j
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
c Internal arithmetic will be done in double precision, a nicety that you can omit if five-figure accuracy is good enough.
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     :  24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     :  -.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    enddo
      gammln=tmp+log(stp*ser/x)
      return
      END

