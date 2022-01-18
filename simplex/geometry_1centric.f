c geometry_1centric.f
c Convert 1-centric cartesian coordinates to barycentric.
c Miroslav Broz (miroslav.broz@email.cz), Jun 28th 2016

      subroutine geometry_1centric(nbod, m, elmts, r, v)

      implicit none
      include 'simplex.inc'
c input
      integer nbod
      real*8 m(NBODMAX)
      real*8 elmts(NBODMAX,6)
c output
      real*8 r(NBODMAX,3), v(NBODMAX,3)
c internal
      integer i
      real*8 rh(NBODMAX,3), vh(NBODMAX,3)

      rh(1,1) = 0.d0
      rh(1,2) = 0.d0
      rh(1,3) = 0.d0
      vh(1,1) = 0.d0
      vh(1,2) = 0.d0
      vh(1,3) = 0.d0

      do i = 2, nbod
        rh(i,1) = elmts(i,1)
        rh(i,2) = elmts(i,2)
        rh(i,3) = elmts(i,3)
        vh(i,1) = elmts(i,4)
        vh(i,2) = elmts(i,5)
        vh(i,3) = elmts(i,6)
      enddo

c convert to barycentric frame
      call coord_h2b(nbod,m,
     :  rh(1,1),rh(1,2),rh(1,3),vh(1,1),vh(1,2),vh(1,3),
     :  r(1,1),r(1,2),r(1,3),v(1,1),v(1,2),v(1,3))

      return
      end


