c*************************************************************************
c                            BS_STEP.F
c*************************************************************************
c This subroutine has same i/o as STEP_KDK but here we use a BS
c Steps both massive and test particles in the same subroutine.
c
c             Input:
c                 i1st          ==>  = 0 if first step; = 1 not (int scalar)
c                 time          ==>  current time (real scalar)
c                 nbod          ==>  number of massive bodies (int scalar)
c                 ntp           ==>  number of massive bodies (int scalar)
c                 mass          ==>  mass of bodies (real array)
c                 j2rp2,j4rp4   ==>  J2*radii_pl^2 and  J4*radii_pl^4
c                                     (real scalars)
c                 xh,yh,zh      ==>  initial position in helio coord 
c                                    (real arrays)
c                 vxh,vyh,vzh   ==>  initial velocity in helio coord 
c                                    (real arrays)
c                 xht,yht,zht    ==>  initial part position in helio coord 
c                                      (real arrays)
c                 vxht,vyht,vzht ==>  initial velocity in helio coord 
c                                        (real arrays)
c                 istat           ==>  status of the test paricles
c                                      (2d integer array)
c                                      istat(i,1) = 0 ==> active:  = 1 not
c                                      istat(i,2) = -1 ==> Danby did not work
c                 rstat           ==>  status of the test paricles
c                                      (2d real array)
c                 dt            ==>  time step
c             Output:
c                 xh,yh,zh      ==>  final position in helio coord 
c                                       (real arrays)
c                 vxh,vyh,vzh   ==>  final velocity in helio coord 
c                                       (real arrays)
c                 xht,yht,zht    ==>  final position in helio coord 
c                                       (real arrays)
c                 vxht,vyht,vzht ==>  final position in helio coord 
c                                       (real arrays)
c
c
c Remarks:  
c Authors:  Hal Levison
c Date:    5/17/93
c Last revision: 2/24/94

c      subroutine bs_step(i1st,time,nbod,ntp,mass,j2rp2,j4rp4,
c     &     xh,yh,zh,vxh,vyh,vzh,xht,yht,zht,vxht,vyht,vzht,
c     &     istat,rstat,dt)	

      subroutine bs_step(i1st,time,nbod,ntp,mass,j2rp2,j4rp4,
     &     xb,yb,zb,vxb,vyb,vzb,xbt,ybt,zbt,vxbt,vybt,vzbt,
     &     istat,rstat,dt,eps)

      include '../swift.inc'
      include 'bs.inc'

c...  Inputs Only: 
      integer nbod,ntp,i1st
      real*8 mass(nbod),dt,time,j2rp2,j4rp4

c...  Inputs and Outputs:
      integer istat(NTPMAX,NSTAT)
      real*8 rstat(NTPMAX,NSTATR)
c      real*8 xh(nbod),yh(nbod),zh(nbod)
c      real*8 vxh(nbod),vyh(nbod),vzh(nbod)
c      real*8 xht(ntp),yht(ntp),zht(ntp)
c      real*8 vxht(ntp),vyht(ntp),vzht(ntp)
      real*8 xb(nbod),yb(nbod),zb(nbod)
      real*8 vxb(nbod),vyb(nbod),vzb(nbod)
      real*8 xbt(ntp),ybt(ntp),zbt(ntp)
      real*8 vxbt(ntp),vybt(ntp),vzbt(ntp)

c...  Internals
      integer j,i,ntpi,link(NTPMAX)
      integer istattmp(NTPMAX,NSTAT),jj,i1stin
      real*8 eps
c      real*8 xb(NPLMAX),yb(NPLMAX),zb(NPLMAX)
c      real*8 vxb(NPLMAX),vyb(NPLMAX),vzb(NPLMAX)
c      real*8 xbt(NTPMAX),ybt(NTPMAX),zbt(NTPMAX)
c      real*8 vxbt(NTPMAX),vybt(NTPMAX),vzbt(NTPMAX)
      real*8 ybs(6,(NTPMAX+NPLMAX)),tfake,dttmp,msys

      data i1stin/0/

c      save i1stin,eps

c----
c...  Executable code 

c...  set things up if this is the initial call

c      if(i1stin.eq.0) then
c	write(*,*) 'Enter the value for eps : '
c	read(*,*) eps
c        write(*,*) ' eps = ',eps
c        write(*,*) ' CONTINUE: '
c        i1stin = 1
c      endif

c...  Convert to barycentric coords
c      call coord_h2b(nbod,mass,xh,yh,zh,vxh,vyh,vzh,
c     &     xb,yb,zb,vxb,vyb,vzb,msys)
c      call coord_h2b_tp(ntp,xht,yht,zht,vxht,vyht,vzht,
c     &     xb(1),yb(1),zb(1),vxb(1),vyb(1),vzb(1),
c     &     xbt,ybt,zbt,vxbt,vybt,vzbt)

c...  copy to the big array
      do i=1,nbod
         ybs(1,i) = xb(i)
         ybs(2,i) = yb(i)
         ybs(3,i) = zb(i)
         ybs(4,i) = vxb(i)
         ybs(5,i) = vyb(i)
         ybs(6,i) = vzb(i)
      enddo

      ntpi = 0
      do i=1,ntp
         if(istat(i,1).eq.0) then
            ntpi = ntpi + 1
            j = ntpi + nbod
            link(ntpi) = i
            ybs(1,j) = xbt(i)
            ybs(2,j) = ybt(i)
            ybs(3,j) = zbt(i)
            ybs(4,j) = vxbt(i)
            ybs(5,j) = vybt(i)
            ybs(6,j) = vzbt(i)
            do jj = 1,NSTAT
               istattmp(ntpi,jj) = istat(i,jj)
            enddo
         endif
      enddo

      tfake = 0.0d0
      dttmp = dt

c      do while(tfake.lt.dt)
      do while( (abs(tfake-dt)/dt) .gt. 1.0e-7 )    ! just to be real safe
         call bs_int(nbod,ntpi,mass,j2rp2,j4rp4,istattmp,
     &        tfake,dttmp,ybs,eps)
         dttmp = dt - tfake
      enddo

c...  put things back
      do i=1,nbod
         xb(i) = ybs(1,i)
         yb(i) = ybs(2,i)
         zb(i) = ybs(3,i)
         vxb(i) = ybs(4,i)
         vyb(i) = ybs(5,i)
         vzb(i) = ybs(6,i)
      enddo

      do i=1,ntpi
         j = i + nbod
         xbt(link(i)) = ybs(1,j)
         ybt(link(i)) = ybs(2,j)
         zbt(link(i)) = ybs(3,j)
         vxbt(link(i)) = ybs(4,j)
         vybt(link(i)) = ybs(5,j)
         vzbt(link(i)) = ybs(6,j)
         do jj = 1,NSTAT
            istat(link(i),jj) = istattmp(i,jj)
         enddo
      enddo

c...  Convert back to helio. coords at the end of the step
c	call coord_b2h(nbod,mass,xb,yb,zb,vxb,vyb,vzb,
c     &         xh,yh,zh,vxh,vyh,vzh)
c	call coord_b2h_tp(ntp,xbt,ybt,zbt,vxbt,vybt,vzbt,
c     &         xb(1),yb(1),zb(1),vxb(1),vyb(1),vzb(1),
c     &         xht,yht,zht,vxht,vyht,vzht)

      return

      end   ! bs_step
c------------------------------------------------------------------------



