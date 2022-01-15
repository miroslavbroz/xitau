c swift_bs_xyzb.f
c Call swift_bs_xyzb integrator.
c Miroslav Broz (miroslav.broz@email.cz), Apr 3rd 2021

      subroutine swift_bs_xyzb(NBOD,m,r,v,omega0,s0,
     :  NOUT,NMAX,tout,rout,vout,inparfile,eps_BS,debug,
     :  n_of_interest,t_of_interest)

      include '../swift.inc'
      include '../tides/spin.inc'
      include '../tides/tides.inc'
      include 'simplex.inc'

      integer NBOD
      real*8 m(NBODMAX),r(NBODMAX,3),v(NBODMAX,3)
      real*8 omega0(NBODMAX),s0(NBODMAX,3)
      integer NOUT,NMAX
      real*8 tout(NMAX)
      real*8 rout(NMAX,NBODMAX,3),vout(NMAX,NBODMAX,3)
      character*80 inparfile
      real*8 eps_BS
      logical debug
      integer n_of_interest
      real*8 t_of_interest(TIMEMAX)

      real*8 xb(NPLMAX),yb(NPLMAX),zb(NPLMAX)
      real*8 vxb(NPLMAX),vyb(NPLMAX),vzb(NPLMAX)
      real*8 xbt(NTPMAX),ybt(NTPMAX),zbt(NTPMAX)
      real*8 vxbt(NTPMAX),vybt(NTPMAX),vzbt(NTPMAX)

      integer istat(NTPMAX,NSTAT)
      integer i,j,ntp,iflgchk,i1st,iu
      real*8 rstat(NTPMAX,NSTATR)
      real*8 j2rp2,j4rp4
      real*8 t0,tstop,dt,dtout,dtdump,eps
      real*8 t,tout_,dttmp,dtnext
      real*8 tspin,tspinout
      real*8 rmin,rmax,rmaxu,qmin,rplsq(NPLMAX)
      logical lclose 
      character*80 fopenstat,outfile
      logical interesting

c read integration parameters from file

      call io_init_param(inparfile,t0,tstop,dt,dtout,dtdump,
     &  iflgchk,rmin,rmax,rmaxu,qmin,lclose,outfile,fopenstat)

      if (debug) then
        write(*,*) "# dt = ", dt, " day"
        write(*,*) "# dtout = ", dtout, " day"
      endif

c initial conditions for planets
      do i = 1,NBOD
        xb(i)  = r(i,1)
        yb(i)  = r(i,2)
        zb(i)  = r(i,3)
        vxb(i) = v(i,1)
        vyb(i) = v(i,2)
        vzb(i) = v(i,3)
      enddo

c initial dump
      NOUT = 1
      do i = 1,NBOD
        tout(NOUT) = t0
        rout(NOUT,i,1) = xb(i)
        rout(NOUT,i,2) = yb(i)
        rout(NOUT,i,3) = zb(i)
        vout(NOUT,i,1) = vxb(i)
        vout(NOUT,i,2) = vyb(i)
        vout(NOUT,i,3) = vzb(i)
      enddo

      t = t0
      tout_ = t0 + dtout
      dtnext = dt
      eps = 1.e-8
      ntp = 0
      i1st = 0
      j2rp2 = 0.d0
      j4rp4 = 0.d0

c initial conditions for spins
      iu = 20
      tspin = t0 + dtspin
      tspinout = t0 + dtspinout
      do i = 1, NBODMAX
        omega(i) = omega0(i)
        do j = 1, 3
          s(j,i) = s0(i,j)  ! i<->j convention :-(
          L_spin(j,i) = s(j,i) * MoI(i) * omega(i)
        enddo
      enddo
      if (debug_spin) then
        call io_write_spin(t,nbod,outspinfile,iu,fopenstat)
      endif

c integration loop
      j = 1
      do while (t.le.tstop-eps)

        do while ((t_of_interest(j).le.t).and.(j.lt.n_of_interest))
          j = j+1
        enddo

        dttmp = t_of_interest(j)-t
        if ((dttmp.lt.dtnext).and.(dttmp.gt.0.d0)) then
          interesting = .true.
          dtnext = dtnext-dttmp
        else
          interesting = .false.
          dttmp = dtnext
          dtnext = dt
        endif

        call bs_step(i1st,t,NBOD,ntp,m,j2rp2,j4rp4,
     &    xb,yb,zb,vxb,vyb,vzb,xbt,ybt,zbt,vxbt,vybt,
     &    vzbt,istat,rstat,dttmp,eps_BS)

        t = t + dttmp

c if it is time, output orbital elements
        if ((t.ge.tout_-eps).or.(interesting)) then 
          NOUT = NOUT+1
          if (NOUT.gt.NMAX) then
            write(*,*) "swift_bs_xyzb: Error number of output data ",
     :        "> OUTMAX = ", NMAX, ", t = ", t, " days"
            stop
          endif
          do i = 1,NBOD
            tout(NOUT) = t
            rout(NOUT,i,1) = xb(i)
            rout(NOUT,i,2) = yb(i)
            rout(NOUT,i,3) = zb(i)
            vout(NOUT,i,1) = vxb(i)
            vout(NOUT,i,2) = vyb(i)
            vout(NOUT,i,3) = vzb(i)
          enddo
        endif

        if (t.ge.tout_-eps) then
          tout_ = tout_ + dtout
        endif

c evolve spin axes due to tides
        if (t.ge.tspin-eps) then
          call spin_evolve(t,nbod,dtspin)
          tspin = tspin + dtspin
        endif

        if (debug_spin) then
          if (t.ge.tspinout-eps) then
            call io_write_spin(t,nbod,outspinfile,iu,"append")
            tspinout = tspinout + dtspinout
          endif
        endif

      enddo

      return
      end    ! swift_bs_xyzb


