c getacc_oblat.f
c Oblatness accelerations (to be added).
c Miroslav Broz (miroslav.broz@email.cz), May 26th 2016

c Reference: Fabrycky (2010), Eq. (4)
c
c f_R = -1/2 k_L,star Omega_star^2 R_star^5 / r^4 \hat r
c
c Warning: This only holds for a spin PERPENDICULAR to the orbit!

      subroutine getacc_oblat(nbod,mass,xb,yb,zb,irij5,axb,ayb,azb)

      include "../swift.inc"
      include "tides2.inc"

c input
      integer nbod
      real*8 mass(nbod)
      real*8 xb(nbod),yb(nbod),zb(nbod)
      real*8 irij5(NPLMAX,NPLMAX)

c output
      real*8 axb(nbod),ayb(nbod),azb(nbod)

c internal
      integer i, j
      real*8 xij, yij, zij
      real*8 acc, ax, ay, az, fac

      if (.not.use_oblat) return

      do i = 1, nbod  ! "planet"
        do j = 1, nbod  ! "star"
          if (j.ne.i) then

            xij = xb(i) - xb(j)
            yij = yb(i) - yb(j)
            zij = zb(i) - zb(j)

            acc = koef2(j) * R_body5(j) * irij5(i,j)
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


