c geometry_twopairs2.f
c Convert elements to barycentric coordinates for a geometry like ((1+2)+3)+(4+5).
c Miroslav Broz (miroslav.broz@email.cz), Mar 14th 2025

c      _      \        _ 
c     / \      |      / \
c     1 2      3      4 5
c     \_/      |      \_/
c             /          

      subroutine geometry_3plus2(nbod, m, r, v, elmts)

      implicit none
      include '../chi2/chi2.inc'
      include '../misc/const.inc'
c input
      integer nbod
      real*8 m(NBODMAX)
      real*8 r(NBODMAX,3), v(NBODMAX,3)
c output
      real*8 elmts(NBODMAX,6)
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

c convert to Jacobian coordinates
      call coord_b2j(nbod,m,
     :  r(1,1),r(1,2),r(1,3),v(1,1),v(1,2),v(1,3),
     :  rj(1,1),rj(1,2),rj(1,3),vj(1,1),vj(1,2),vj(1,3))

c adjust coordinates (the elements were standard stellar-astronomy)
      do i = 1, nbod
        tmp = rj(i,1)
        rj(i,1) = rj(i,2)
        rj(i,2) = -tmp
        rj(i,3) = -rj(i,3)
        tmp = vj(i,1)
        vj(i,1) = vj(i,2)
        vj(i,2) = -tmp
        vj(i,3) = -vj(i,3)
      enddo

c everything to heliocentric
      call coord_j2h(nbod,m,
     :  rj(1,1),rj(1,2),rj(1,3),vj(1,1),vj(1,2),vj(1,3),
     :  rh(1,1),rh(1,2),rh(1,3),vh(1,1),vh(1,2),vh(1,3))

c everything to 1-centric
      do k = 1, 3
        r2_1(k) = rh(2,k) 
        v2_1(k) = vh(2,k) 

        msum = m(1)+m(2)
        r12_1(k) = m(2)*r2_1(k)/msum
        v12_1(k) = m(2)*v2_1(k)/msum

        r3_12(k) = rh(3,k) - r12_1(k)
        v3_12(k) = vh(3,k) - v12_1(k)

        msum = m(1)+m(2)+m(3)
        r123_12(k) = m(3)*r3_12(k)/msum
        v123_12(k) = m(3)*v3_12(k)/msum

        r5_4(k) = rh(5,k) - rh(4,k) 
        v5_4(k) = vh(5,k) - vh(4,k) 

        msum = m(4)+m(5)
        r45_4(k) = m(5)*r5_4(k)/msum
        v45_4(k) = m(5)*v5_4(k)/msum

        r45_123(k) = rh(4,k) + r45_4(k) - r12_1(k) - r123_12(k)
        v45_123(k) = vh(4,k) + v45_4(k) - v12_1(k) - v123_12(k)
      enddo

c 1-centric
      msum = m(1)+m(2)
      ialpha = -1
      i = 2
      call orbel___(msum,ialpha,
     :  r2_1(1),r2_1(2),r2_1(3),
     :  v2_1(1),v2_1(2),v2_1(3),
     :  elmts(i,1),elmts(i,2),elmts(i,3),
     :  elmts(i,4),elmts(i,5),elmts(i,6))

c (1+2)-centric
      msum = m(1)+m(2)+m(3)
      i = 3
      call orbel___(msum,ialpha,
     :  r3_12(1),r3_12(2),r3_12(3),
     :  v3_12(1),v3_12(2),v3_12(3),
     :  elmts(i,1),elmts(i,2),elmts(i,3),
     :  elmts(i,4),elmts(i,5),elmts(i,6))

c (1+2+3)-centric
      msum = m(1)+m(2)+m(3)+m(4)+m(5)
      i = 4
      call orbel___(msum,ialpha,
     :  r45_123(1),r45_123(2),r45_123(3),
     :  v45_123(1),v45_123(2),v45_123(3),
     :  elmts(i,1),elmts(i,2),elmts(i,3),
     :  elmts(i,4),elmts(i,5),elmts(i,6))

c 4-centric
      msum = m(4)+m(5)
      i = 5
      call orbel___(msum,ialpha,
     :  r5_4(1),r5_4(2),r5_4(3),
     :  v5_4(1),v5_4(2),v5_4(3),
     :  elmts(i,1),elmts(i,2),elmts(i,3),
     :  elmts(i,4),elmts(i,5),elmts(i,6))

c convert to degrees
      do i = 2, nbod
        do k = 3, 6
          elmts(i,k) = elmts(i,k)/deg
        enddo
      enddo

      return
      end

      subroutine orbel___(msum,ialpha,x,y,z,vx,vy,vz,
     :  P,loge,inc,capom,varpi,lambda)

      include '../misc/const.inc'
      include '../chi2/chi2.inc'
      include '../chi2/dependent.inc'
      integer ialpha
      real*8 x,y,z,vx,vy,vz
      real*8 msum,P,loge,inc,capom,varpi,lambda
      real*8 a,e,n,omega,capm
      real*8 nula2pi
      
      call orbel_xv2el(x,y,z,vx,vy,vz,
     :  msum,ialpha,a,e,inc,capom,omega,capm)
      
      n = (msum / a**3)**(1.d0/2.d0)
      P = 2.d0*pi_/n
      loge = log10(e)
      varpi = nula2pi(capom + omega)
      lambda = nula2pi(capom + omega + capm)
      
c      if (debug) then
c        write(*,*) '# a = ', a, ' au = ', a*au/R_S, ' R_S'
c      endif
     
      return
      end

