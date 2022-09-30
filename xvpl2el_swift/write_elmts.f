c write_elmts.f
c Write orbital elements to stdout.
c Miroslav Broz (miroslav.broz@email.cz), Jul 30th 2015

      subroutine write_elmts(t,id,elmts,m1,m2,d)

      include 'common.inc'
      include '../misc/const.inc'
      integer id
      real*8 t, elmts(6), m1, m2, d

      integer j
      real*8 a, e, i0, omega0, omega, m0
      real*8 varpi, P, n, t_Bessel, tau, tau_Bessel, a_angular,
     :  omega0_PA
      real*8 nula2pi, jdate2bessel

c compute additional variables
      a      = elmts(1)
      e      = elmts(2)
      i0     = elmts(3)
      omega0 = elmts(4)  ! node
      omega  = elmts(5)  ! peri
      m0     = elmts(6)

c a**3/P**2 = m1+m2
c M = (t-tau)*n 

      a_angular = atan(a/d)
      omega0_PA = nula2pi(90.d0*deg - omega0)         ! the reference direction is UP in x, y plane! (as for the P.A.)
      varpi = nula2pi(90.d0*deg - (omega0+omega))
      P = sqrt(a**3.d0/(m1+m2)*4.d0*pi_**2)
c      write(*,*) 'GM_S = ', GM_S
c      write(*,*) 'm1 = ', m1, ' = ', m1/GM_S, ' M_S'
c      write(*,*) 'm2 = ', m2, ' = ', m2/GM_S, ' M_S'
c      write(*,*) 'a = ', a, ' au = ', a*au/1.e3, ' km'
c      write(*,*) 'P = ', P, ' day'
c      stop
      n = 2.d0*pi_/P
      t_Bessel = jdate2bessel(t)
      tau = t - 365.25*m0/n
      tau_Bessel = jdate2bessel(tau)

c write output
      write(*,20) t, id, elmts(1), elmts(2),
     :  (nula2pi(elmts(j))*rad, j=3,6),
     :  t_Bessel, a_angular/arcsec, P/365.25d0, P, n*rad,
     :  varpi*rad, omega0_PA*rad,
     :  tau, tau_Bessel
20    format(f22.8,1x,i4,6(1x,f22.16),
     :  8x, f12.4, 6(1x,f20.12), 1x, f22.8, 1x, f12.4)

      return
      end



