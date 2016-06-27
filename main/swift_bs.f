c swift_bs.f
c Run swift_bs integrator separately, which us suitable
c   for studies of long-term evolution and stability.
c Miroslav Broz (miroslav.broz@email.cz), Mar 16th 2016

      program swift_bs

      include "../swift.inc"

      character*80 inparfile,inplfile,intpfile,intidesfile
      real*8 eps_BS

      integer NBOD
      real*8 m(NPLMAX)
      real*8 xb(NPLMAX),yb(NPLMAX),zb(NPLMAX)
      real*8 vxb(NPLMAX),vyb(NPLMAX),vzb(NPLMAX)
      real*8 xbt(NTPMAX),ybt(NTPMAX),zbt(NTPMAX)
      real*8 vxbt(NTPMAX),vybt(NTPMAX),vzbt(NTPMAX)

      integer istat(NTPMAX,NSTAT)
      integer i,ntp,iflgchk,i1st,iue
      real*8 rstat(NTPMAX,NSTATR)
      real*8 j2rp2,j4rp4
      real*8 t0,tstop,dt,dtout,dtdump,eps
      real*8 t,tout_,tdump_,eoff
      real*8 rmin,rmax,rmaxu,qmin,rplsq(NPLMAX)
      logical lclose 
      character*80 fopenstat,outfile

c print version
      call util_version

c read input parameters (and write input to terminal)
      write(*,*) "# inparfile : "
      read(*,*,err=990,end=990) inparfile
      write(*,*) "# inparfile = ", trim(inparfile)

      write(*,*) "# inplfile : "
      read(*,*,err=990,end=990) inplfile
      write(*,*) "# inplfile = ", trim(inplfile)

      write(*,*) "# intpfile : "
      read(*,*,err=990,end=990) intpfile
      write(*,*) "# intpfile = ", trim(intpfile)

      write(*,*) "# intidesfile : "
      read(*,*,err=990,end=990) intidesfile
      write(*,*) "# intidesfile = ", trim(intidesfile)

      write(*,*) "# eps_BS : "
      read(*,*,err=990,end=990) eps_BS
      write(*,*) "# eps_BS = ", eps_BS

c read integration parameters from files
      call io_init_param(inparfile,t0,tstop,dt,dtout,dtdump,
     :  iflgchk,rmin,rmax,rmaxu,qmin,lclose,outfile,fopenstat)

      call io_init_pl(inplfile,lclose,iflgchk,NBOD,m,xb,yb,zb,
     :  vxb,vyb,vzb,rplsq,j2rp2,j4rp4)

      call io_init_tides(intidesfile,NBOD)

      t = t0
      tout_ = t0 + dtout
      tdump_ = t0 + dtdump
      eps = 1.d-8
      ntp = 0
      i1st = 0
      iue = 30

c initial dump
      call io_dump_param("dump_param.dat",t,tstop,dt,dtout,dtdump,
     :  iflgchk,rmin,rmax,rmaxu,qmin,lclose,outfile)

      call io_dump_pl("dump_pl.dat",NBOD,m,xb,yb,zb,
     :  vxb,vyb,vzb,lclose,iflgchk,rplsq,j2rp2,j4rp4)

      call io_write_pl(outfile,t,NBOD,m,xb,yb,zb,vxb,vyb,vzb)

      if (btest(iflgchk,2)) then    ! bit 2 is set
         eoff = 0.0d0
         call anal_energy_write(t0,nbod,m,j2rp2,j4rp4,xb,yb,zb,
     :     vxb,vyb,vzb,iue,fopenstat,eoff)
      endif

c integration loop
      do while (t.le.tstop-eps)

        call bs_step(i1st,t,NBOD,ntp,m,j2rp2,j4rp4,
     &    xb,yb,zb,vxb,vyb,vzb,xbt,ybt,zbt,vxbt,vybt,
     &    vzbt,istat,rstat,dt,eps_BS)

        t = t + dt

c if it is time, output and dump coordinates and velocities
        if (t .ge. tout_-eps) then 
          call io_write_pl(outfile,t,NBOD,m,xb,yb,zb,vxb,vyb,vzb)
          tout_ = tout_ + dtout
        endif

        if (t .ge. tdump_-eps) then 
          call io_dump_param("dump_param.dat",t,tstop,dt,dtout,dtdump,
     :      iflgchk,rmin,rmax,rmaxu,qmin,lclose,outfile)

          call io_dump_pl("dump_pl.dat",NBOD,m,xb,yb,zb,
     :      vxb,vyb,vzb,lclose,iflgchk,rplsq,j2rp2,j4rp4)

          if (btest(iflgchk,2)) then    ! bit 2 is set
             eoff = 0.0d0
             call anal_energy_write(t,nbod,m,j2rp2,j4rp4,xb,yb,zb,
     :         vxb,vyb,vzb,iue,fopenstat,eoff)
          endif

          write(*,*) "t = ", t, "  fraction done = ", t/tstop

          tdump_ = tdump_ + dtdump
        endif
      enddo

      stop

990   continue
      write(*,*) "swift_bs: Error reading standard input!"

      end    ! swift_bs

