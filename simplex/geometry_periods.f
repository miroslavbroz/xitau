c geometry_periods.f
c Convert elements to barycentric coordinates for geometry like ((1+2)+3)+4;
c use P (instead of a).
c Miroslav Broz (miroslav.broz@email.cz), Oct 21st 2022

c      _          \                |
c     / \          |               |
c     1 2          3               4
c     \_/          |               |
c                 /                |

      subroutine geometry_periods(nbod_, m, elmts, r, v)

      implicit none
      include 'simplex.inc'
      include 'dependent.inc'
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
      real*8 msum, P, n, a, tmp

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
        call orbel_el2xv(msum,ialpha,
     :    a,elmts(i,2),elmts(i,3),
     :    elmts(i,4),elmts(i,5),elmts(i,6),
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

        if (debug_swift) then
          write(*,*) '# a = ', a, ' au = ', a*au/R_S, ' R_S'
        endif
      enddo

c convert to barycentric frame
      call coord_j2b(nbod,m,
     :  rj(1,1),rj(1,2),rj(1,3),vj(1,1),vj(1,2),vj(1,3),
     :  r(1,1),r(1,2),r(1,3),v(1,1),v(1,2),v(1,3))

      return
      end

