c getacc_oblat.f
c Oblatness accelerations (to be added).
c Miroslav Broz (miroslav.broz@email.cz), Oct 15th 2024

c Reference: Fabrycky (2010), Eq. (4), w. a correction of 1/2 -> 3.
c
c f_R = -3 k_L,star Omega_star^2 R_star^5 / r^5 \vec r
c
c Warning: This only holds for a spin PERPENDICULAR to the orbit!
c Warning: This only includes the radial component of acceleration!

      subroutine getacc_oblat(nbod_,mass,xb,yb,zb,irij5,axb,ayb,azb)

      include "../swift.inc"
      include "tides2.inc"
      include '../chi2/chi2.inc'
      include '../chi2/dependent.inc'
      include '../misc/const.inc'

c input
      integer nbod_
      real*8 mass(nbod)
      real*8 xb(nbod),yb(nbod),zb(nbod)
      real*8 irij5(NPLMAX,NPLMAX)

c output
      real*8 axb(nbod),ayb(nbod),azb(nbod)

c internal
      integer i, j
      real*8 xij, yij, zij
      real*8 acc, ax, ay, az, fac
      real*8 n0

      if (.not.use_oblat) return

c n0 = sqrt(mass/R_body^3)
c k_L = -C20 (n0/Omega_rot)^2
c koef2 = 0.5 k_L Omega_rot^2
c acc = koef2 R_body^5 / rij^5

c from dependent.inc
      do i = 1, nbod
        R_body(i) = R_star(i)*R_S/AU
        koef2(i) = -3.0d0*C20(i)*mass(i)*R_body(i)**2
      enddo

      do i = 1, nbod  ! "planet"
        do j = 1, nbod  ! "star"
          if (j.ne.i) then

            xij = xb(i) - xb(j)
            yij = yb(i) - yb(j)
            zij = zb(i) - zb(j)

            acc = koef2(j) * irij5(i,j)
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


