c geometry_equatorial.f
c Convert elements to barycentric coordinates for geometry like ((1+2)+3)+4.
c Miroslav Broz (miroslav.broz@email.cz), Apr 11th 2022

c      _          \                |
c     / \          |               |
c     1 2          3               4
c     \_/          |               |
c                 /                |

      subroutine geometry_equatorial(nbod_, m, elmts, r, v)

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
      real*8 msum, P, a, e, n, tmp, eps, capm
c functions
      real*8 eps_earth

c convert to radians
      do i = 2, nbod
        do k = 3, 6
          elmts(i,k) = elmts(i,k)*deg
        enddo
      enddo

      eps = eps_earth(T0)  ! or J2000?

c compute Jacobian coordinates
      msum = m(1)
      ialpha = -1
      do i = 2, nbod
        msum = msum + m(i)
        P = elmts(i,1)
        n = 2.d0*pi_/P
        a = (msum / n**2)**(1.d0/3.d0)
        write(*,*) '# a = ', a, ' au = ', a*au/1.d3, ' km'
        e = 10.d0**elmts(i,2)
        capm = elmts(i,6)
        call orbel_el2xv(msum,ialpha,
     :    a,e,elmts(i,3),
     :    elmts(i,4),elmts(i,5),capm,
     :    rj(i,1),rj(i,2),rj(i,3),
     :    vj(i,1),vj(i,2),vj(i,3))

c the elements were equatorial (J2000)
        call rotx(rj(i,1),rj(i,2),rj(i,3),-eps)
        call rotx(vj(i,1),vj(i,2),vj(i,3),-eps)

      enddo

c convert to barycentric frame
      call coord_j2b(nbod,m,
     :  rj(1,1),rj(1,2),rj(1,3),vj(1,1),vj(1,2),vj(1,3),
     :  r(1,1),r(1,2),r(1,3),v(1,1),v(1,2),v(1,3))

      return
      end

      subroutine rotx(x, y, z, phi)
      implicit none
      real*8 x, y, z, phi
      real*8 y_, z_, c, s
      c = cos(phi)
      s = sin(phi)
      y_ = y*c - z*s
      z_ = y*s + z*c
      y = y_
      z = z_
      return
      end


