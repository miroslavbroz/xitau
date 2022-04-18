c***************************************************************************
c			TU4_GETACCB.F
c*************************************************************************
c GETACCB returns the bary. acc. on each of n mutually
c interacting objects by direct pairwise summation
c	
c
c             Input:
c                 nbod          ==>  number of massive bodies (int scalar)
c                 mass          ==>  mass of planets (real array)
c                 j2rp2,j4rp4   ==>  J2*radii_pl^2 and  J4*radii_pl^4
c                                     (real scalars)
c                 xb,yb,zb      ==>  position of planets in beri coord 
c                                    (real arrays)
c             Output:
c               axb,ayb,azb   ==>  accel in beri coord (real arrays) 
c
c Remarks:  Based on Martin's NB4M routines
c Authors:  Martin Duncan 
c Date:    3/8/93
c Last revision: 4/5/95

      subroutine tu4_getaccb(time,nbod,mass,j2rp2,j4rp4,xb,yb,zb,
     :  vxb,vyb,vzb,axb,ayb,azb)

      use getacc_bf2_module
      use getacc_mp2_module

      include '../swift.inc'

c...  Inputs Only: 
      integer nbod
      real*8 time
      real*8 mass(nbod),j2rp2,j4rp4
      real*8 xb(nbod),yb(nbod),zb(nbod)
      real*8 vxb(nbod),vyb(nbod),vzb(nbod)

c...  Output
      real*8 axb(nbod),ayb(nbod),azb(nbod)

c...  Internals
      real*8 xx,yy,zz,rr2,fac3,mi,mj
      real*8 axx,ayy,azz,fac1,fac2
      real*8 xh(NPLMAX),yh(NPLMAX),zh(NPLMAX),irh(NPLMAX)
      real*8 aoblx(NPLMAX),aobly(NPLMAX),aoblz(NPLMAX)
      integer i,j
      real*8 time_

c inverse distances for getacc_tides, getacc_oblat and getacc_ppn
      real*8 fac5,fac8
      real*8 irij(NPLMAX,NPLMAX)
      real*8 irij2(NPLMAX,NPLMAX)
      real*8 irij5(NPLMAX,NPLMAX)
      real*8 irij8(NPLMAX,NPLMAX)

c----
c...  executable code

      xh(1) = 0.0d0
      yh(1) = 0.0d0
      zh(1) = 0.0d0

c...  do sun part first
      axb(1) = 0.0d0
      ayb(1) = 0.0d0
      azb(1) = 0.0d0
      i = 1
      do j = i+1,nbod
         mi = mass(i)
         mj = mass(j)
         xx = xb(i) - xb(j)
         yy = yb(i) - yb(j)
         zz = zb(i) - zb(j)
         rr2 = xx**2 + yy**2 + zz**2
         fac2 = 1.d0/rr2
         fac1 = sqrt(fac2)
         fac3 = fac1*fac2

c..      save for the J2 and J4 calculations
         xh(j) = -xx
         yh(j) = -yy
         zh(j) = -zz
         irh(j) = fac1

         irij(i,j) = fac1
         irij(j,i) = fac1
         irij2(i,j) = fac2
         irij2(j,i) = fac2
         fac5 = fac3*fac2
         fac8 = fac5*fac3
         irij5(i,j) = fac5
         irij5(j,i) = fac5
         irij8(i,j) = fac8
         irij8(j,i) = fac8

         axx = xx*fac3
         ayy = yy*fac3
         azz = zz*fac3
         axb(i) = axb(i) - axx*mj
         ayb(i) = ayb(i) - ayy*mj
         azb(i) = azb(i) - azz*mj
         axb(j) =   axx*mi
         ayb(j) =   ayy*mi
         azb(j) =   azz*mi
      enddo

      do i = 2,nbod-1
         do j = i+1,nbod
            mi = mass(i)
            mj = mass(j)
            xx = xb(i) - xb(j)
            yy = yb(i) - yb(j)
            zz = zb(i) - zb(j)
            rr2 = xx**2 + yy**2 + zz**2
            fac2 = 1.d0/rr2
            fac1 = sqrt(fac2)
            fac3 = fac1*fac2

            irij(i,j) = fac1
            irij(j,i) = fac1
            irij2(i,j) = fac2
            irij2(j,i) = fac2
            fac5 = fac3*fac2
            fac8 = fac5*fac3
            irij5(i,j) = fac5
            irij5(j,i) = fac5
            irij8(i,j) = fac8
            irij8(j,i) = fac8
            
            axx = xx*fac3
            ayy = yy*fac3
            azz = zz*fac3
            axb(i) = axb(i) - axx*mj
            ayb(i) = ayb(i) - ayy*mj
            azb(i) = azb(i) - azz*mj
            axb(j) = axb(j) + axx*mi
            ayb(j) = ayb(j) + ayy*mi
            azb(j) = azb(j) + azz*mi
         enddo
      enddo

      if(j2rp2.ne.0.0d0) then
         call obl_acc(nbod,mass,j2rp2,j4rp4,xh,yh,zh,irh,
     &        aoblx,aobly,aoblz)
         do i = 1,nbod
            axb(i) = axb(i) + aoblx(i)
            ayb(i) = ayb(i) + aobly(i)
            azb(i) = azb(i) + aoblz(i)
         enddo
      endif

      call getaccb_tides(nbod,mass,xb,yb,zb,vxb,vyb,vzb,axb,ayb,azb)
      call getacc_tides2(time,nbod,mass,xb,yb,zb,axb,ayb,azb)
      call getacc_oblat(nbod,mass,xb,yb,zb,irij5,axb,ayb,azb)
      call getacc_ppn(nbod,mass,xb,yb,zb,vxb,vyb,vzb,irij,irij2,
     :  axb,ayb,azb)
      call getacc_bf2(time,nbod,mass,xb,yb,zb,axb,ayb,azb)
      call getacc_mp2(time,nbod,mass,xb,yb,zb,axb,ayb,azb)

      return
      end                       !  tu4_getaccb
c____________________________________________________________________________
