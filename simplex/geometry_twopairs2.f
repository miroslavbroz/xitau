c geometry_twopairs2.f
c Convert elements to barycentric coordinates for a geometry like ((1+2)+(3+4))+5;
c use log(e), varpi, and true longitude (instead of e, omega, M).
c Miroslav Broz (miroslav.broz@email.cz), Dec 10th 2021

c      _          _               \ 
c     / \        / \               |
c     1 2        3 4               5
c     \_/        \_/               |
c                                 / 

      subroutine geometry_twopairs2(nbod, m, elmts, r, v)

      implicit none
      include 'simplex.inc'
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
      real*8 r2_1(3), v2_1(3), r12_1(3), v12_1(3)
      real*8 r4_3(3), v4_3(3), r34_3(3), v34_3(3)
      real*8 r34_12(3), v34_12(3)
      real*8 msum, tmp

      if (nbod.lt.4) then
        write(*,*) "geometry_twopairs2.f: Error number of bodies ",
     :    "nbod = ", nbod, " .lt. 4."
        stop
      endif

c convert to radians
      do i = 2, nbod
        do k = 3, 6
          elmts(i,k) = elmts(i,k)*deg
        enddo
      enddo

c (1+2) pair, 1-centric coordinates
      msum = m(1)+m(2)
      ialpha = -1
      i = 2
      call orbel__(msum,ialpha,
     :  elmts(i,1),elmts(i,2),elmts(i,3),
     :  elmts(i,4),elmts(i,5),elmts(i,6),
     :  r2_1(1),r2_1(2),r2_1(3),
     :  v2_1(1),v2_1(2),v2_1(3))

c barycenter
      do k = 1, 3
        r12_1(k) = m(2)*r2_1(k)/msum
        v12_1(k) = m(2)*v2_1(k)/msum
      enddo

c (3+4) pair, 3-centric
      msum = m(3)+m(4)
      i = 3
      call orbel__(msum,ialpha,
     :  elmts(i,1),elmts(i,2),elmts(i,3),
     :  elmts(i,4),elmts(i,5),elmts(i,6),
     :  r4_3(1),r4_3(2),r4_3(3),
     :  v4_3(1),v4_3(2),v4_3(3))

c barycenter
      do k = 1, 3
        r34_3(k) = m(4)*r4_3(k)/msum
        v34_3(k) = m(4)*v4_3(k)/msum
      enddo

c (1+2)+(3+4) mutual orbit, (1+2)-centric
      msum = m(1)+m(2)+m(3)+m(4)
      i = 4
      call orbel__(msum,ialpha,
     :  elmts(i,1),elmts(i,2),elmts(i,3),
     :  elmts(i,4),elmts(i,5),elmts(i,6),
     :  r34_12(1),r34_12(2),r34_12(3),
     :  v34_12(1),v34_12(2),v34_12(3))

c everything to 1-centric
      do k = 1, 3
        rh(1,k) = 0.d0
        vh(1,k) = 0.d0

        rh(2,k) = r2_1(k)
        vh(2,k) = v2_1(k)

        rh(3,k) = r12_1(k) + r34_12(k) - r34_3(k)
        vh(3,k) = v12_1(k) + v34_12(k) - v34_3(k)

        rh(4,k) = rh(3,k) + r4_3(k)
        vh(4,k) = vh(3,k) + v4_3(k)
      enddo

c everything to Jacobian
      call coord_h2j(4,m,
     :  rh(1,1),rh(1,2),rh(1,3),vh(1,1),vh(1,2),vh(1,3),
     :  rj(1,1),rj(1,2),rj(1,3),vj(1,1),vj(1,2),vj(1,3))

c other bodies (also Jacobian)
      do i = 5, nbod
        msum = msum + m(i)
        call orbel__(msum,ialpha,
     :    elmts(i,1),elmts(i,2),elmts(i,3),
     :    elmts(i,4),elmts(i,5),elmts(i,6),
     :    rj(i,1),rj(i,2),rj(i,3),
     :    vj(i,1),vj(i,2),vj(i,3))
      enddo

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


      subroutine orbel__(msum,ialpha,P,loge,inc,capom,varpi,lambda,
     :  x,y,z,vx,vy,vz)

      include '../misc/const.inc'
      integer ialpha
      real*8 msum,P,loge,inc,capom,varpi,lambda
      real*8 x,y,z,vx,vy,vz
      real*8 a,e,n,omega,capm

      n = 2.d0*pi_/P
      a = (msum / n**2)**(1.d0/3.d0)
      e = 10.d0**loge
      omega = varpi - capom
      capm = lambda - capom - omega

      call orbel_el2xv(msum,ialpha,
     :  a,e,inc,
     :  capom,omega,capm,
     :  x,y,z,
     :  vx,vy,vz)

      return
      end


