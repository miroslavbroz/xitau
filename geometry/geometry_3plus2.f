c geometry_twopairs2.f
c Convert elements to barycentric coordinates for a geometry like ((1+2)+3)+(4+5).
c Miroslav Broz (miroslav.broz@email.cz), Mar 14th 2025

c      _      \        _ 
c     / \      |      / \
c     1 2      3      4 5
c     \_/      |      \_/
c             /          

      subroutine geometry_3plus2(nbod, m, elmts, r, v)

      implicit none
      include '../chi2/chi2.inc'
      include '../misc/const.inc'
c input
      integer nbod
      real*8 m(NBODMAX)
      real*8 elmts(NBODMAX,6)
c output
      real*8 r(NBODMAX,3), v(NBODMAX,3)
c internal
      integer i, k, ialpha
      real*8 rj(NBODMAX,3), vj(NBODMAX,3)
      real*8 rh(NBODMAX,3), vh(NBODMAX,3)
      real*8 r2_1(3), v2_1(3)
      real*8 r12_1(3), v12_1(3)
      real*8 r3_12(3), v3_12(3)
      real*8 r123_12(3), v123_12(3)
      real*8 r45_123(3), v45_123(3)
      real*8 r5_4(3), v5_4(3)
      real*8 r45_4(3), v45_4(3)
      real*8 msum, tmp

      if (nbod.lt.5) then
        write(*,*) "geometry_3plus2.f: Error number of bodies ",
     :    "nbod = ", nbod, " .lt. 5."
        stop
      endif

c convert to radians
      do i = 2, nbod
        do k = 3, 6
          elmts(i,k) = elmts(i,k)*deg
        enddo
      enddo

c 1-centric
      msum = m(1)+m(2)
      ialpha = -1
      i = 2
      call orbel__(msum,ialpha,
     :  elmts(i,1),elmts(i,2),elmts(i,3),
     :  elmts(i,4),elmts(i,5),elmts(i,6),
     :  r2_1(1),r2_1(2),r2_1(3),
     :  v2_1(1),v2_1(2),v2_1(3))

      do k = 1, 3
        r12_1(k) = m(2)*r2_1(k)/msum
        v12_1(k) = m(2)*v2_1(k)/msum
      enddo

c (1+2)-centric
      msum = m(1)+m(2)+m(3)
      i = 3
      call orbel__(msum,ialpha,
     :  elmts(i,1),elmts(i,2),elmts(i,3),
     :  elmts(i,4),elmts(i,5),elmts(i,6),
     :  r3_12(1),r3_12(2),r3_12(3),
     :  v3_12(1),v3_12(2),v3_12(3))

      do k = 1, 3
        r123_12(k) = m(3)*r3_12(k)/msum
        v123_12(k) = m(3)*v3_12(k)/msum
      enddo

c (1+2+3)-centric
      msum = m(1)+m(2)+m(3)+m(4)+m(5)
      i = 4
      call orbel__(msum,ialpha,
     :  elmts(i,1),elmts(i,2),elmts(i,3),
     :  elmts(i,4),elmts(i,5),elmts(i,6),
     :  r45_123(1),r45_123(2),r45_123(3),
     :  v45_123(1),v45_123(2),v45_123(3))

c 4-centric
      msum = m(4)+m(5)
      i = 5
      call orbel__(msum,ialpha,
     :  elmts(i,1),elmts(i,2),elmts(i,3),
     :  elmts(i,4),elmts(i,5),elmts(i,6),
     :  r5_4(1),r5_4(2),r5_4(3),
     :  v5_4(1),v5_4(2),v5_4(3))

      do k = 1, 3
        r45_4(k) = m(5)*r5_4(k)/msum
        v45_4(k) = m(5)*v5_4(k)/msum
      enddo

c everything to 1-centric
      do k = 1, 3
        rh(1,k) = 0.d0
        vh(1,k) = 0.d0

        rh(2,k) = r2_1(k)
        vh(2,k) = v2_1(k)

        rh(3,k) = r12_1(k) + r3_12(k)
        vh(3,k) = v12_1(k) + v3_12(k)

        rh(4,k) = r12_1(k) + r123_12(k) + r45_123(k) - r45_4(k)
        vh(4,k) = v12_1(k) + v123_12(k) + v45_123(k) - v45_4(k)

        rh(5,k) = rh(4,k) + r5_4(k)
        vh(5,k) = vh(4,k) + v5_4(k)
      enddo

c everything to Jacobian
      call coord_h2j(nbod,m,
     :  rh(1,1),rh(1,2),rh(1,3),vh(1,1),vh(1,2),vh(1,3),
     :  rj(1,1),rj(1,2),rj(1,3),vj(1,1),vj(1,2),vj(1,3))

c adjust coordinates (the elements were standard stellar-astronomy)
      do i = 1, nbod
        tmp = rj(i,1)
        rj(i,1) = -rj(i,2)
        rj(i,2) = tmp
        rj(i,3) = -rj(i,3)
        tmp = vj(i,1)
        vj(i,1) = -vj(i,2)
        vj(i,2) = tmp
        vj(i,3) = -vj(i,3)
      enddo

c finally, convert to barycentric frame
      call coord_j2b(nbod,m,
     :  rj(1,1),rj(1,2),rj(1,3),vj(1,1),vj(1,2),vj(1,3),
     :  r(1,1),r(1,2),r(1,3),v(1,1),v(1,2),v(1,3))

      return
      end


