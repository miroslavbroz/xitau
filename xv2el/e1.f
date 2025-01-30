c**********************************************************************
      subroutine e1(x,v,elmts,m1,m2)
c**********************************************************************
c
c  determination of elemnts from the position and velocity
c  x(3)      rectagular coordinates of the body [au]
c  v(3)      velocity [au/day] 
c  elmts(6)  elements of orbit
c  m1       mass of the Sun (or zero)
c  m2       mass of the planet (or zero)
c
      implicit none
      include '../misc/const.inc'

      double precision x(3),v(3),elmts(6),m1,m2
      double precision v2,r,a,xx,yy,zz,p,e,i0,e0,m0,v0,
     &  omega,omega0,sinv,cosv,nula2pi,kg,gms2

      gms2 = (m1+m2)*GM_S
      v2 = v(1)**2+v(2)**2+v(3)**2
      r = sqrt(x(1)**2+x(2)**2+x(3)**2)
      a = 1.d0/(2.d0/r-v2/gms2)
      xx = x(2)*v(3)-x(3)*v(2)
      yy = x(1)*v(3)-x(3)*v(1)
      zz = x(1)*v(2)-x(2)*v(1)
      p = (xx**2+yy**2+zz**2)/gms2
      e = sqrt(1.d0-p/a)
      i0 = acos(zz/sqrt(xx**2+yy**2+zz**2))
      omega = nula2pi(atan2(xx,yy))
      sinv = (x(1)*v(1)+x(2)*v(2)+x(3)*v(3))/r*sqrt(p/gms2)
      cosv = (p/r-1.d0)
      v0 = nula2pi(atan2(sinv,cosv))
      e0 = 2.d0*atan(sqrt((1.d0-e)/(1.d0+e))*tan(v0/2.d0))
      m0 = nula2pi(e0-e*sin(e0))
      omega0 = nula2pi(atan2(x(3)/sin(i0),x(1)*cos(omega)+
     &  x(2)*sin(omega))-v0)

c      write(*,*) 'xx = ', xx
c      write(*,*) 'yy = ', yy
c      write(*,*) 'zz = ', zz
c      write(*,*) 'i0 = ', i0*rad
c      write(*,*) 'omega0 = ', omega0*rad

      elmts(1)=a
      elmts(2)=e
      elmts(3)=i0
      elmts(4)=omega0	! peri
      elmts(5)=omega	! node
      elmts(6)=m0

      return
      end

