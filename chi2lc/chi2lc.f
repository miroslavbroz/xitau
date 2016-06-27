c chi2lc.f
c Convert chi2.in parameters to that of lc.in (Wilson-Devinney code).
c Miroslav Broz (miroslav.broz@email.cz), Jun 24th 2016

c Note: This code is inferior wrt. chi2 or simplex, which have
c more precise computation of Omega_Kopal and (non-Planck) luminosity!

      program chi2lc

      implicit none

      include '../misc/const.inc'

c constants
      integer NDIMMAX
      parameter(NDIMMAX=32)
      integer NBODMAX
      parameter(NBODMAX=4)
      integer BANDMAX
      parameter(BANDMAX=5)

c input
      integer nparam
      real*8 x_param(NDIMMAX)
      real*8 T0
      integer nbod
      integer nband

c variables for WD code (lc.in file)
      integer mpage,nref,mref,ifsmv1,ifsmv2,icor1,icor2,ld
      integer jdphs
      real*8 hjd0,period,dpdt,pshift,stdev,seed
      integer noise
      real*8 hjdst,hjdsp,hjdin,phstrt,phstop,phin,phn
      integer mode,ipb,ifat1,ifat2,n1,n2
      real*8 perr0,dperdt,the,vunit
      real*8 e,a,f1,f2,vga,xincl,gr1,gr2,abunin
      real*8 tavh,tavc,alb1,alb2,poth,potc,rm,xbol1,xbol2,ybol1,ybol2
      integer iband
      real*8 hlum,clum,xh,xc,yh,yc,el3,opsf,zero,factor,wl

c temporary
      integer i, j, k
      real*8 m(NBODMAX), elmts(NBODMAX,6)
      real*8 T_eff(NBODMAX), R_star(NBODMAX), v_rot(NBODMAX)
      real*8 zero_(BANDMAX)
      real*8 gamma,d_pc
      real*8 lambda_eff, band_eff
      real*8 Lum(NBODMAX), Lumtot
      real*8 r1, r2, r3, q
      character*255 str
      character*80 file_SKY(NBODMAX), file_RV(NBODMAX), file_TTV,
     :  file_ECL, file_VIS, file_CLO

c functions
      real*8 Omega

c
c read input parameters
c
5     continue
        read(*,10,err=990,end=990) str
10      format(a)
      if (str(1:1).eq.'#') goto 5

      read(str,*,err=990,end=990) nparam
      if (nparam.gt.NDIMMAX) then
        write(*,*) 'Error: nparam > NDIMMAX = ', NDIMMAX
        stop
      endif

      read(*,*,err=990,end=990) (x_param(i), i=1,nparam)

      read(*,*,err=990,end=990) T0
      read(*,*,err=990,end=990) nbod
      if (nbod.gt.NBODMAX) then
        write(*,*) "# Error nbod = ", nbod, ".gt. NBODMAX = ",NBODMAX
        stop
      endif

      do i = 1, nbod
        read(*,10,err=990,end=990) file_SKY(i)
      enddo
      do i = 1, nbod
        read(*,10,err=990,end=990) file_RV(i)
      enddo
      read(*,10,err=990,end=990) file_TTV
      read(*,10,err=990,end=990) file_ECL
      read(*,10,err=990,end=990) file_VIS
      read(*,10,err=990,end=990) file_CLO

      read(*,*,err=990,end=990) nband
      if (nband.gt.BANDMAX) then
        write(*,*) "# Error nband = ", nband, ".gt. BANDMAX = ", BANDMAX
        stop
      endif
c
c create m(), elmts() arrays for easy manipulation
c
      j = 0
      do i = 1, nbod
        j = j+1
c        m(i) = x_param(j)*GM_S
        m(i) = x_param(j)
      enddo

      do i = 2, nbod
        do k = 1, 6
          j = j+1
          elmts(i,k) = x_param(j)
        enddo
        do k = 3, 6
          elmts(i,k) = elmts(i,k)*deg
        enddo
      enddo

      do i = 1, nbod
        j = j+1
        T_eff(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        R_star(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        v_rot(i) = x_param(j)
      enddo

      do i = 1, nband
        j = j+1
        zero_(i) = x_param(j)
      enddo

      j = j+1
      gamma = x_param(j)
      j = j+1
      d_pc = x_param(j)
c
c compute luminosities (for photocentre computations)
c
      lambda_eff = 550.d-9
      band_eff = 88.d-9
      call luminosity_planck_bandpass(T_eff, R_star, nbod, lambda_eff,
     :  band_eff, Lum, Lumtot)
c
c compute WD parameters
c
      mpage = 1
      nref = 2
      mref = 1
      ifsmv1 = 0
      ifsmv2 = 0
      icor1 = 1
      icor2 = 1
      ld = 2

      jdphs = 1
      hjd0 = T0-2400000.0d0
      period = sqrt(elmts(2,1)**3/(m(1)+m(2))) * 365.25d0
      dpdt = -0.0d-0
      pshift = +0.0d0
      stdev = 0.0d-0
      noise = 0
      seed = 138472375.d0

      hjdst = 56222.000000d0
      hjdsp = 56238.034100d0
      hjdin = 0.01d0
      phstrt = -0.2d0
      phstop = +1.2d0
      phin = 0.001d0
      phn = 0.25d0

c beware of mode number (has to be 0 or 2 for detached systems)
      mode = 0
      ipb = 0
      ifat1 = 1
      ifat2 = 1
      n1 = 30
      n2 = 30
      perr0 = +1.57d0
      dperdt = +0.0d-0
      the = 0.0d0
      VUNIT = 1.0d0

      e = elmts(2,2)
      if (e.lt.0.d0) then
        e = 0.d0
      endif
      a = elmts(2,1)*AU/R_S
      f1 = 1.0d0
      f2 = 1.0d0
      vga = -0.0d0
      xincl = elmts(2,3)*rad
      gr1 = 1.0d0
      gr2 = 1.0d0
      abunin = +0.0d0

      tavh = T_eff(1)/1.d4
      tavc = T_eff(2)/1.d4
      alb1 = +1.0d0
      alb2 = +1.0d0

c approximate values of Kopal potential, assuming R_star is equatoreal radius
      q = m(2)/m(1)
      r1 = R_star(1)*R_S/(elmts(2,1)*AU)
      r2 = 1.d0-r1
      r3 = r1
      poth = Omega(r1,r2,r3,q)
      r2 = R_star(2)*R_S/(elmts(2,1)*AU)
      r1 = 1.d0-r2
      r3 = r1
      potc = Omega(r1,r2,r3,q)

      rm = m(2)/m(1)
      xbol1 = 0.5d0
      xbol2 = 0.5d0
      ybol1 = 0.5d0
      ybol2 = 0.5d0

      iband = 7
      hlum = Lum(1)/Lumtot
      clum = Lum(2)/Lumtot
      xh = 0.5d0
      xc = 0.5d0
      yh = 0.5d0
      yc = 0.5d0
      el3 = ((Lum(3)+Lum(4))/Lumtot)/(4.d0*pi_)
      opsf = 0.0d-0
      zero = +1.0d0
      factor = 1.0d0
      wl = 0.55d0
c
c write lc.in file
c
      write(*,*) mpage,nref,mref,ifsmv1,ifsmv2,icor1,icor2,ld
      write(*,*) jdphs,hjd0,period,dpdt,pshift,stdev,noise,seed
      write(*,*) hjdst,hjdsp,hjdin,phstrt,phstop,phin,phn
      write(*,*) mode,ipb,ifat1,ifat2,n1,n2,perr0,dperdt,the,vunit
      write(*,*) e,a,f1,f2,vga,xincl,gr1,gr2,abunin
      write(*,*) tavh,tavc,alb1,alb2,poth,potc,rm,xbol1,xbol2,ybol1,
     :  ybol2
      write(*,*) iband,hlum,clum,xh,xc,yh,yc,el3,opsf,zero,factor,wl
c
c some debugging info
c
c      write(*,*)
c      write(*,*) '# a = ', a, " R_S"
c      write(*,*) '# f1 = ', f1
c      write(*,*) '# f2 = ', f2
c      write(*,*) '# poth = ', poth
c      write(*,*) '# potc = ', potc
c      write(*,*) '# hlum = ', hlum, ' // hot component luminosity'
c      write(*,*) '# clum = ', clum, ' // cold component luminosity'
c      write(*,*) '# el3 = ', el3, ' // third-light flux'
c      write(*,*) '# el3*4*pi = ', el3*4.d0*pi

      stop

990   continue
      write(*,*) 'chi2: Error reading standard input.'

      end

c
c Kopal potential, Wilson & Devinney (1971), Eq. (1)
c
      real*8 function Omega(r1, r2, r3, q)
      implicit none
      real*8 r1, r2, r3, q
      Omega = 1.d0/r1 + q/r2 + 0.5d0*(1.d0+q)*r3**2
      return
      end


