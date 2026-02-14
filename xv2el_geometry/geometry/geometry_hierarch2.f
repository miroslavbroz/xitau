c geometry_hierarch2.f
c Convert barycentric coordinates to elements for geometry like ((1+2)+3)+4;
c use log(e), varpi, and true longitude (instead of e, omega, M).
c Miroslav Broz (miroslav.broz@email.cz), Sep 16th 2020

c      _          \                |
c     / \          |               |
c     1 2          3               4
c     \_/          |               |
c                 /                |

      subroutine geometry_hierarch2(nbod_, m, r, v, elmts)

      implicit none
      include '../chi2/chi2.inc'
      include '../chi2/dependent.inc'
      include '../misc/const.inc'
c input
      integer nbod_
      real*8 m(NBODMAX)
      real*8 r(NBODMAX,3), v(NBODMAX,3)
c output
      real*8 elmts(NBODMAX,6)
c internal
      integer i, k, ialpha
      real*8 rj(NBODMAX,3), vj(NBODMAX,3)
      real*8 msum, P, a, e, n, capom, varpi, lambda, omega, capm, tmp
c functions
      real*8 nula2pi

c convert to Jacobian coordinates
      call coord_b2j(nbod,m,
     :  r(1,1),r(1,2),r(1,3),
     :  v(1,1),v(1,2),v(1,3),
     :  rj(1,1),rj(1,2),rj(1,3),
     :  vj(1,1),vj(1,2),vj(1,3))

c compute orbital elements
      msum = m(1)
      ialpha = -1
      do i = 2, nbod
        msum = msum + m(i)

c adjust coordinates (the elements were standard stellar-astronomy)
        tmp = rj(i,1)
        rj(i,1) = rj(i,2)
        rj(i,2) = -tmp
        rj(i,3) = -rj(i,3)
        tmp = vj(i,1)
        vj(i,1) = vj(i,2)
        vj(i,2) = -tmp
        vj(i,3) = -vj(i,3)

        call orbel_xv2el(
     :    rj(i,1),rj(i,2),rj(i,3),
     :    vj(i,1),vj(i,2),vj(i,3),
     :    msum,ialpha,a,e,elmts(i,3),capom,omega,capm)

        n = sqrt(msum / a**3)
        P = 2.d0*pi_/n
        elmts(i,1) = P
        elmts(i,2) = log10(e)
        varpi = capom + omega
        lambda = capom + omega + capm
        elmts(i,4) = capom
        elmts(i,5) = nula2pi(varpi)
        elmts(i,6) = nula2pi(lambda)

c        if (debug_swift) then
c          write(*,*) '# a = ', a, ' au = ', a*au/R_S, ' R_S'
c        endif
      enddo

c convert to degrees
      do i = 2, nbod
        do k = 3, 6
          elmts(i,k) = elmts(i,k)/deg
        enddo
      enddo

      return
      end


