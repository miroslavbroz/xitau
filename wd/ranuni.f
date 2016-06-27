
      subroutine ranuni(sn,smod,sm1p1)
c  Version of January 17, 2003
c
c   On each call, subroutine ranuni generates a pseudo-random number,
c     sm1p1, distributed with uniform probability over the range
c     -1. to +1.
c
c   The input number sn, from which both output numbers are generated,
c     should be larger than the modulus 1.00000001d8 and smaller
c     than twice the modulus. The returned number smod will be in
c     that range and can be used as the input sn on the next call
c
      implicit real*8(a-h,o-z)
      st=23.d0
      xmod=1.00000001d8
      smod=st*sn
      goto 2
    1 smod=smod-xmod
    2 if(smod.gt.xmod) goto 1
      sm1p1=(2.d0*smod/xmod-1.d0)
      return
      end
      subroutine rangau(smod,nn,sd,gau)
      implicit real*8(a-h,o-z)
c  Version of February 6, 1997
      ffac=0.961d0
      sfac=ffac*3.d0*sd/(dsqrt(3.d0*dfloat(nn)))
      g1=0.d0
      do 22 i=1,nn
      sn=smod
      call ranuni(sn,smod,sm1p1)
      g1=g1+sm1p1
   22 continue
      gau=sfac*g1
      return
      end
