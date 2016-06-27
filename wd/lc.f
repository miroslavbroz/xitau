c lc.f
c Wilson-Devinney code.
c modifications by MB, Apr 18th 2016

      program lc

      implicit none

      include 'lc.inc'

      logical debug
      real*8 phas, smagg, jd
c
c read input parameters
c
      read(*,*) mpage,nref,mref,ifsmv1,ifsmv2,icor1,icor2,ld
      read(*,*) jdphs,hjd0,period,dpdt,pshift,stdev,noise,seed
      read(*,*) hjdst,hjdsp,hjdin,phstrt,phstop,phin,phn
      read(*,*) mode,ipb,ifat1,ifat2,n1,n2,perr0,dperdt,the,vunit
      read(*,*) e,a,f1,f2,vga,xincl,gr1,gr2,abunin
      read(*,*) tavh,tavc,alb1,alb2,poth,potc,rm,xbol1,xbol2,ybol1,
     $  ybol2
      read(*,*) iband,hlum,clum,xh,xc,yh,yc,el3,opsf,zero,factor,wl

      debug = .true.
c
c write header
c
      write(*,*) '# JD & phase & magnitude'
c
c call WD code
c
      phas = phstrt
      do while (phas.le.phstop)

        call lc_call(phas, debug, smagg)

        jd = hjd0 + phas*period
        write(*,*) jd, phas, smagg
        phas = phas + phin
      enddo

      stop
      end


