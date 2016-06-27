c lc.inc
c Common block for all lightcurve parameters.
c modified by Miroslav Broz (miroslav.broz@email.cz), Apr 20th 2016

c see explanations in lc.in example

      integer mpage, nref, mref, ifsmv1, ifsmv2, icor1, icor2, ld
      integer noise
      integer mode, ipb, ifat1, ifat2, n1, n2
      integer iband

      real*8 jdphs, hjd0, period, dpdt, pshift, stdev, seed
      real*8 hjdst, hjdsp, hjdin, phstrt, phstop, phin, phn
      real*8 perr0, dperdt, the, vunit
      real*8 e, a, f1, f2, vga, xincl, gr1, gr2, abunin
      real*8 tavh, tavc, alb1, alb2, poth, potc, rm
      real*8 xbol1, xbol2, ybol1, ybol2
      real*8 hlum, clum, xh,xc, yh, yc, el3, opsf, zero, factor, wl

      common /cb_lc/
     :  jdphs, hjd0, period, dpdt, pshift, stdev, seed,
     :  hjdst, hjdsp, hjdin, phstrt, phstop, phin, phn,
     :  perr0, dperdt, the, vunit,
     :  e, a, f1, f2, vga, xincl, gr1, gr2, abunin,
     :  tavh, tavc, alb1, alb2, poth, potc, rm,
     :  xbol1, xbol2, ybol1, ybol2,
     :  hlum, clum, xh,xc, yh, yc, el3, opsf, zero, factor, wl,
     :  mpage, nref, mref, ifsmv1, ifsmv2, icor1, icor2, ld,
     :  noise,
     :  mode, ipb, ifat1, ifat2, n1, n2,
     :  iband



