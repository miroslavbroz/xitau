c swift_bs.f
c Run swift_bs integrator separately, which us suitable
c   for studies of long-term evolution and stability.
c Miroslav Broz (miroslav.broz@email.cz), Mar 16th 2016

      program swift_bs

      include "../swift.inc"
      include '../tides/spin.inc'
      include "../chi2/chi2.inc"
      include "../chi2/dependent.inc"

      character*80 inparfile,inplfile,intpfile,intidesfile

      real*8 m(NPLMAX)
      real*8 xb(NPLMAX),yb(NPLMAX),zb(NPLMAX)
      real*8 vxb(NPLMAX),vyb(NPLMAX),vzb(NPLMAX)
      real*8 xbt(NTPMAX),ybt(NTPMAX),zbt(NTPMAX)
      real*8 vxbt(NTPMAX),vybt(NTPMAX),vzbt(NTPMAX)

      integer istat(NTPMAX,NSTAT)
      integer i,ntp,iflgchk,i1st,iue,iuf
      real*8 rstat(NTPMAX,NSTATR)
      real*8 j2rp2,j4rp4
      real*8 tstop,dt,dtout,dtdump,eps
      real*8 t,tout_,tdump_,eoff
      real*8 tspin,tspinout
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

      write(*,*) "# eps_BS : "
      read(*,*,err=990,end=990) eps_BS
      write(*,*) "# eps_BS = ", eps_BS

      write(*,*) "# use_multipole : "
      read(*,*,err=990,end=990) use_multipole
      write(*,*) "# use_multipole = ", use_multipole

      write(*,*) "# use_bruteforce : "
      read(*,*,err=990,end=990) use_bruteforce
      write(*,*) "# use_bruteforce = ", use_bruteforce

      read(*,*,err=990,end=990) use_oblat
      write(*,*) "# use_oblat = ", use_oblat

      read(*,*,err=990,end=990) use_tides
      write(*,*) "# use_tides = ", use_tides

      read(*,*,err=990,end=990) use_tides2
      write(*,*) "# use_tides2 = ", use_tides2

      read(*,*,err=990,end=990) use_ppn
      write(*,*) "# use_ppn = ", use_ppn

      write(*,*) "# is_forward : "
      read(*,*,err=990,end=990) is_forward
      write(*,*) "# is_forward = ", is_forward

c read integration parameters from files
      call io_init_param(inparfile,t0,tstop,dt,dtout,dtdump,
     :  iflgchk,rmin,rmax,rmaxu,qmin,lclose,outfile,fopenstat)

      call io_init_pl(inplfile,lclose,iflgchk,NBOD,m,xb,yb,zb,
     :  vxb,vyb,vzb,rplsq,j2rp2,j4rp4)

      call io_init_spin("spin.in", nbod)
      call io_init_tides("tides.in", nbod)
      call io_init_tides2("tides2.in", nbod)

      t = t0
      tout_ = t0 + dtout
      tdump_ = t0 + dtdump
      tspin = t0 + dtspin
      tspinout = t0 + dtspinout
      eps = 1.d-8
      ntp = 0
      i1st = 0
      iue = 30
      iuf = 40

c initial dump
      call io_dump_param("dump_param.dat",t,tstop,dt,dtout,dtdump,
     :  iflgchk,rmin,rmax,rmaxu,qmin,lclose,outfile)

      call io_dump_pl("dump_pl.dat",NBOD,m,xb,yb,zb,
     :  vxb,vyb,vzb,lclose,iflgchk,rplsq,j2rp2,j4rp4)

      call io_dump_spin("dump_spin.dat",NBOD)

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

          call io_dump_spin("dump_spin.dat",NBOD)

          if (btest(iflgchk,2)) then    ! bit 2 is set
             eoff = 0.0d0
             call anal_energy_write(t,nbod,m,j2rp2,j4rp4,xb,yb,zb,
     :         vxb,vyb,vzb,iue,fopenstat,eoff)
          endif

          write(*,*) "t = ", t, "  fraction done = ", t/tstop

          tdump_ = tdump_ + dtdump
        endif

c evolve spin axes due to tides
        if (t.ge.tspin-eps) then
          call spin_evolve(t,nbod,dtspin)
          tspin = tspin + dtspin
        endif

        if (t.ge.tspinout-eps) then
          call io_write_spin(t,nbod,outspinfile,iuf,"append")
          tspinout = tspinout + dtspinout
        endif

      enddo

      stop

990   continue
      write(*,*) "swift_bs: Error reading standard input!"

      end    ! swift_bs

