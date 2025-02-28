c geometry_hierarch2.f
c Convert elements to barycentric coordinates for geometry like ((1+2)+3)+4;
c use log(e), varpi, and true longitude (instead of e, omega, M).
c Miroslav Broz (miroslav.broz@email.cz), Sep 16th 2020

c      _          \                |
c     / \          |               |
c     1 2          3               4
c     \_/          |               |
c                 /                |

      subroutine geometry_hierarch2(nbod_, m, elmts, r, v)

      implicit none
      include '../chi2/chi2.inc'
      include '../chi2/dependent.inc'
      include '../misc/const.inc'
c input
      integer nbod_
      real*8 m(NBODMAX)
      real*8 elmts(NBODMAX,6)
c output
      real*8 r(NBODMAX,3), v(NBODMAX,3)
c internal
      integer i, k, ialpha
      real*8 rj(NBODMAX,3), vj(NBODMAX,3)
      real*8 msum, P, a, e, n, capom, varpi, lambda, omega, capm, tmp

c convert to radians
      do i = 2, nbod
        do k = 3, 6
          elmts(i,k) = elmts(i,k)*deg
        enddo
      enddo

c compute Jacobian coordinates
      msum = m(1)
      ialpha = -1
      do i = 2, nbod
        msum = msum + m(i)
        P = elmts(i,1)
        n = 2.d0*pi_/P
        a = (msum / n**2)**(1.d0/3.d0)
        e = 10.d0**elmts(i,2)
        lambda = elmts(i,6)
        varpi = elmts(i,5)
        capom = elmts(i,4)
        omega = varpi - capom
        capm = lambda - capom - omega
        call orbel_el2xv(msum,ialpha,
     :    a,e,elmts(i,3),
     :    capom,omega,capm,
     :    rj(i,1),rj(i,2),rj(i,3),
     :    vj(i,1),vj(i,2),vj(i,3))

c adjust coordinates (the elements were standard stellar-astronomy)
        tmp = rj(i,1)
        rj(i,1) = -rj(i,2)
        rj(i,2) = tmp
        rj(i,3) = -rj(i,3)
        tmp = vj(i,1)
        vj(i,1) = -vj(i,2)
        vj(i,2) = tmp
        vj(i,3) = -vj(i,3)

        if (debug) then
          write(*,*) '# a = ', a, ' au = ', a*au/R_S, ' R_S'
        endif
      enddo

c convert to barycentric frame
      call coord_j2b(nbod,m,
     :  rj(1,1),rj(1,2),rj(1,3),vj(1,1),vj(1,2),vj(1,3),
     :  r(1,1),r(1,2),r(1,3),v(1,1),v(1,2),v(1,3))

      return
      end


