c getacc_tides.f
c Tidal accelerations (to be added).
c Miroslav Broz (miroslav.broz@email.cz), May 26th 2016

c Reference: Fabrycky (2010), Eq. (3)
c f_T = -3 k_L,p G m_star^2 / m_p R_p^5 / r^7 \hat r

      subroutine getacc_tides(nbod,mass,xb,yb,zb,irij8,axb,ayb,azb)

      include "../swift.inc"
      include "tides.inc"

c input
      integer nbod
      real*8 mass(nbod)
      real*8 xb(nbod),yb(nbod),zb(nbod)
      real*8 irij8(NPLMAX,NPLMAX)

c output
      real*8 axb(nbod),ayb(nbod),azb(nbod)

c internal
      integer i, j
      real*8 xij, yij, zij
      real*8 acc, ax, ay, az, fac

      if (.not.use_tides) return

      do i = 1, nbod  ! "planet"
        do j = 1, nbod  ! "star"
          if (j.ne.i) then
            xij = xb(i) - xb(j)
            yij = yb(i) - yb(j)
            zij = zb(i) - zb(j)

            acc = koef1(i)*mass(j)**2/mass(i) * R_body5(i) * irij8(i,j)
            ax = acc*xij
            ay = acc*yij
            az = acc*zij

            axb(i) = axb(i) - ax
            ayb(i) = ayb(i) - ay
            azb(i) = azb(i) - az

c back-reaction
            fac = mass(i)/mass(j)
            axb(j) = axb(j) + fac*ax
            ayb(j) = ayb(j) + fac*ay
            azb(j) = azb(j) + fac*az

          endif
        enddo
      enddo

      return
      end


