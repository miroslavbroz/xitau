
      subroutine legendre(x,pleg,n)
c  Version of January 7, 2002
      implicit real*8 (a-h,o-z)
      dimension pleg(n)
      pleg(1)=1.d0
      pleg(2)=x
      if(n.le.2) return
      denom=1.d0
      do 1 i=3,n
      fac1=x*(2.d0*denom+1.d0)
      fac2=denom
      denom=denom+1.d0
      pleg(i)=(fac1*pleg(i-1)-fac2*pleg(i-2))/denom
   1  continue
      return
      end
      subroutine binnum(x,n,y,j)
c  Version of January 7, 2002
      implicit real*8(a-h,o-z)
      dimension x(n)
      mon=1
      if(x(1).gt.x(2)) mon=-1
      do 1 i=1,n
      if(mon.eq.-1) goto 3
      if(y.le.x(i)) goto 2
      goto 1
   3  if(y.gt.x(i)) goto 2
   1  continue
   2  continue
      j=i-1
      return
      end
