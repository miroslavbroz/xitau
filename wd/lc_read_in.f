c lc_read_in.f
c Read lc.in file with parameters for WD code.
c modified by Miroslav Broz (miroslav.broz@email.cz), Apr 21st 2016

      subroutine lc_read_in(filename)

      implicit none

c input
      character*(*) filename

c output (everything is passed in the common block)
      include 'lc.inc'

c temporary
      integer iu, ierr
      data iu /10/

      open(iu, file=filename, status="old", iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "lc_read_in.f: Error opening file '",
     :    trim(filename), "'."
        stop
      endif
      
      read(iu,*) mpage,nref,mref,ifsmv1,ifsmv2,icor1,icor2,ld
      read(iu,*) jdphs,hjd0,period,dpdt,pshift,stdev,noise,seed
      read(iu,*) hjdst,hjdsp,hjdin,phstrt,phstop,phin,phn
      read(iu,*) mode,ipb,ifat1,ifat2,n1,n2,perr0,dperdt,the,vunit
      read(iu,*) e,a,f1,f2,vga,xincl,gr1,gr2,abunin
      read(iu,*) tavh,tavc,alb1,alb2,poth,potc,rm,xbol1,xbol2,ybol1,
     $  ybol2
      read(iu,*) iband,hlum,clum,xh,xc,yh,yc,el3,opsf,zero,factor,wl

      close(iu)

      return
      end


