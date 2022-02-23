c lc_call_nbody.f
c A call to the Wilson-Devinney code, 1 lightcurve point, N-body variant.
c modifications by MB, Apr 18th 2016

      subroutine lc_call_nbody(gpha_, sema, xincl_, tavh_, tavc_,
     :  poth_, potc_, rm_, iband_, hlum_, clum_, el3_, debug, i2nd,
     :  smagg, vkm1_, vkm2_)

      implicit real*8(a-h,o-z)

      save  ! everything

c input
      real*8 gpha_        ! true phase of EB (i.e. NOT the usual mean phase)
      real*8 sema         ! semimajor axis of EB
      real*8 xincl_       ! inclination of EB
      real*8 tavh_        ! temperature for hot (or primary) component [10^4 K]
      real*8 tavc_        ! dtto for cold (secondary) [10^4 K]
      real*8 poth_        ! Kopal potential value for hot
      real*8 potc_        ! dtto for cold
      real*8 rm_          ! mass ratio q = m2/m1
      integer iband_      ! photometric band (e.g. 7 = V)
      real*8 hlum_        ! luminosity of hot component [arbitrary units]
      real*8 clum_        ! dtto for cold
      real*8 el3_         ! 3rd-light flux, L3 = 4*pi*el3
      integer i2nd        ! 2nd call with enforced initialisation
      logical debug       ! debugging

c output
      real*8 smagg       ! magnitude
      real*8 vkm1_, vkm2_  ! Rossiter effect

      include 'lc.inc'
c
c explanations of (some) variables
c

c RV ... surface radial velocity for primary
c RVQ ... dtto for secondary
c GRX ... x-coordinate of gravity on primary
c GRY ... y
c GRZ ... z
c GRXQ ... dtto on secondary
c GRYQ ... y
c GRZQ ... z
c MMSAVE ... 
c FR1 ... 
c FR2 ... 
c HLD ... 
c RM ... mass ratio
c POTH ... Kopal potential, hot component
c POTC ... cold component
c GR1 ... exponent of gravity brightening for primary
c GR2 ... dtto for secondary
c ALB1 ... albedo of primary
c ALB2 ... dtto of secondary
c N1 ... discretisation in theta
c N2 ... discretisation in phi
c F1 ... synchronicity ratio of primary 
c F2 ... dtto of secondary
c MOD ... derived from MODE
c XINCL ... inclination [deg]
c THE ... X-ray eclipse duration
c MODE ... mode of the program
c SNTH ... sin(theta) values
c CSTH ... cos(theta)
c SNFI ... sin(phi)
c CSFI ... cos(phi)
c GRV1 ... surface gravity^gravity darkening coefficient for primary, Wilson & Devinney (1971), Eq. (5)
c GRV2 ... dtto from secondary
c RFTEMP ... temporary reflection
c RF1 ... reflection coefficient on primary
c RF2 ... dtto for secondary
c XX1 ... x coordinate of surface -> primary vector
c YY1 ... y
c ZZ1 ... z
c XX2 ... x for secondary
c YY2 ... y
c ZZ2 ... z
c GLUMP1 ... R^2 sin(theta)/cos(beta) for primary
c GLUMP2 ... dtto for secondary
c SLUMP1 ... GLUMP1 * intensity/pole_intensity * sbr for primary
c SLUMP2 ... dtto for secondary
c sbr ... HLUM or CLUM luminozity divided by computed luminosity normalized by polar one and affected by darkening
c CSBT1 ... cos(beta) for primary
c CSBT2 ... dtto for secondary
c GMAG1 ... gravity magnitude for primary
c GMAG2 ... dtto for secondary
c glog1 ... surface log(g) arising from primary
c glog2 ... dtto from secondary
c tld ... temperature (new)
c CORFAC ... contrast factor, spot/base intensity ratio

c
c declarations of ALL variables
c
      dimension rad(4),drdo(4),xtha(4),xfia(4),po(2)
      dimension rv(3011),grx(3011),gry(3011),grz(3011),rvq(3011),
     $  grxq(3011),gryq(3011),grzq(3011),slump1(3011),slump2(3011),
     $  fr1(3011),fr2(3011),glump1(3011),glump2(3011),xx1(3011),
     $  xx2(3011),yy1(3011),yy2(3011),zz1(3011),zz2(3011),grv1(3011),
     $  grv2(3011),rftemp(3011),rf1(3011),rf2(3011),csbt1(3011),
     $  csbt2(3011),gmag1(3011),gmag2(3011),glog1(3011),glog2(3011),
     $  hld(3200),snfi(6400),csfi(6400),tld(6400),snth(260),csth(260),
     $  theta(520),rho(520),aa(20),bb(20),mmsave(124)
      dimension fbin1(100000),fbin2(100000),delv1(100000),delv2(100000),
     $  count1(100000),count2(100000),delwl1(100000),delwl2(100000),
     $  resf1(100000),resf2(100000),wl1(100000),wl2(100000),dvks1(100),
     $  dvks2(100),wll1(100),wll2(100),tau1(100),tau2(100),emm1(100),
     $  emm2(100),ewid1(100),ewid2(100),depth1(100),depth2(100),
     $  hbarw1(100),hbarw2(100),taug(100000),emmg(100000)
      DIMENSION XLAT(2,100),xlong(2,100)
      dimension xcl(100),ycl(100),zcl(100),rcl(100),op1(100),fcl(100),
     $  dens(100),encl(100),edens(100),xmue(100),yskp(14000),zskp(14000)
      dimension message(2,4)
      dimension abun(19),glog(11),grand(250800),plcof(1300)

      integer i1st
c
c common blocks
c
      common /abung/ abun,glog
      common /arrayleg/ grand,istart
      common /planckleg/ plcof
      common /atmmessages/ message,komp
      common /ramprange/ tlowtol,thightol,glowtol,ghightol
      COMMON /FLVAR/ PSHIFT_,DP,EF,EFC,ECOS,perr0_,PHPER,pconsc,pconic,
     $  PHPERI,VSUM1,VSUM2,VRA1,VRA2,VKM1,VKM2,VUNIT_,vfvu,trc,qfacd
      COMMON /DPDX/ DPDX1,DPDX2,PHSV,PCSV
      COMMON /ECCEN/ E_,A_,PERIOD_,VGA_,SINI_,VF,VFAC,VGAM,VOL1,VOL2,
     $  IFC
      COMMON /KFAC/ KFF1,KFF2,kfo1,kfo2
      COMMON /INVAR/ KH,IPB_,IRTE,NREF_,IRVOL1,IRVOL2,mref_,
     $  ifsmv1_,ifsmv2_,icor1_,icor2_,ld_,ncl,jdphs_,ipc
      COMMON /SPOTS/SINLAT(2,100),COSLAT(2,100),SINLNG(2,100),
     $  COSLNG(2,100),RADSP(2,100),temsp(2,100),xlng(2,100),kks(2,100),
     $  Lspot(2,100)
      common /cld/ acm,opsf_
      common /ardot/ dperdt_,hjd,hjd0_,perr
      common /prof2/ du1,du2,du3,du4,binw1,binw2,sc1,sc2,sl1,sl2,
     $  clight
      common /inprof/ in1min,in1max,in2min,in2max,mpage_,nl1,nl2
      common /ipro/ nbins,nl,inmax,inmin,nf1,nf2
      COMMON /NSPT/ NSP1,NSP2

c added by MB
      include 'output.inc'

      character*255 str
c
c data
c
      data xtha(1),xtha(2),xtha(3),xtha(4),xfia(1),xfia(2),xfia(3),
     $  xfia(4)/0.d0,1.570796d0,1.570796d0,1.570796d0,
     $  0.d0,0.d0,1.5707963d0,3.14159365d0/

      data i1st /0/
c
c pass the subroutine parameters which are already in some common block
c
      xincl = xincl_
      a = sema
      tavh = tavh_
      tavc = tavc_
      poth = poth_
      potc = potc_
      rm = rm_
      iband = iband_
      hlum = hlum_
      clum = clum_
      el3 = el3_ 
c
c this (long) part is called only once!
c
      if (i1st.eq.0) then
c
c some varibles are in multiple common blocks, so we must assign them manually
c
        PSHIFT_ = PSHIFT
        perr0_ = perr0
        VUNIT_ = VUNIT
        E_ = E
        A_ = A
        PERIOD_ = PERIOD
        VGA_ = VGA
        SINI_ = SINI
        IPB_ = IPB
        NREF_ = NREF
        mref_ = mref
        ifsmv1_ = ifsmv1
        ifsmv2_ = ifsmv2
        icor1_ = icor1
        icor2_ = icor2
        ld_ = ld
        jdphs_ = jdphs
        opsf_ = opsf
        dperdt_ = dperdt
        hjd0_ = hjd0
        mpage_ = mpage
c
c check parameters (see p. 30 in ebdoc2003.2feb2004.pdf)
c
        if (N1.gt.60) then
          write(*,*) "Error: grid is too fine, N1 = ", N1, " .gt. 60"
          stop
        endif

        if (N2.gt.60) then
          write(*,*) "Error: grid is too fine, N2 = ", N2, " .gt. 60"
          stop
        endif
c
c write input parameters
c
        if (debug) then
          write(*,*) '# parameters read from lc.in file:'
          write(*,*)
          write(*,*) '# mpage  = ', mpage
          write(*,*) '# nref   = ', nref
          write(*,*) '# mref   = ', mref
          write(*,*) '# ifsmv1 = ', ifsmv1
          write(*,*) '# ifsmv2 = ', ifsmv2
          write(*,*) '# icor1  = ', icor1
          write(*,*) '# icor2  = ', icor2
          write(*,*) '# ld     = ', ld
          write(*,*)
          write(*,*) '# jdphs  = ', jdphs
          write(*,*) '# hjd0   = ', hjd0, ' day'
          write(*,*) '# period = ', period, ' day'
          write(*,*) '# dpdt   = ', dpdt
          write(*,*) '# pshift = ', pshift
          write(*,*) '# stdev  = ', stdev, ' mag'
          write(*,*) '# noise  = ', noise
          write(*,*) '# seed   = ', seed
          write(*,*)
          write(*,*) '# hjdst  = ', hjdst, ' day'
          write(*,*) '# hjdsp  = ', hjdsp, ' day'
          write(*,*) '# hjdin  = ', hjdin, ' day'
          write(*,*) '# phstrt = ', phstrt
          write(*,*) '# phstop = ', phstop
          write(*,*) '# phin   = ', phin
          write(*,*) '# phn    = ', phn
          write(*,*)
          write(*,*) '# MODE   = ', MODE
          write(*,*) '# IPB    = ', IPB
          write(*,*) '# IFAT1  = ', IFAT1
          write(*,*) '# IFAT2  = ', IFAT2
          write(*,*) '# N1     = ', N1
          write(*,*) '# N2     = ', N2
          write(*,*) '# perr0  = ', perr0, ' rad'
          write(*,*) '# dperdt = ', dperdt, ' rad/day'
          write(*,*) '# the    = ', the
          write(*,*) '# VUNIT  = ', VUNIT
          write(*,*)
          write(*,*) '# E      = ', E
          write(*,*) '# A      = ', A, ' R_S'
          write(*,*) '# F1     = ', F1
          write(*,*) '# F2     = ', F2
          write(*,*) '# VGA    = ', VGA, ' km/s'
          write(*,*) '# XINCL  = ', XINCL, ' deg'
          write(*,*) '# GR1    = ', GR1
          write(*,*) '# GR2    = ', GR2
          write(*,*) '# abunin = ', abunin, ' [M/H]'
          write(*,*)
          write(*,*) '# tavh = ', tavh, ' 10^4 K'
          write(*,*) '# tavc = ', tavc, ' 10^4 K'
          write(*,*) '# alb1 = ', alb1
          write(*,*) '# alb2 = ', alb2
          write(*,*) '# poth = ', poth
          write(*,*) '# potc = ', potc
          write(*,*) '# rm    = ', rm
          write(*,*) '# xbol1 = ', xbol1
          write(*,*) '# xbol2 = ', xbol2
          write(*,*) '# ybol1 = ', ybol1
          write(*,*) '# ybol2 = ', ybol2
          write(*,*) '# iband  = ', iband
          write(*,*) '# HLUM   = ', HLUM
          write(*,*) '# CLUM   = ', CLUM
          write(*,*) '# XH     = ', XH
          write(*,*) '# xc     = ', xc
          write(*,*) '# yh     = ', yh
          write(*,*) '# yc     = ', yc
          write(*,*) '# EL3    = ', EL3, ' (flux)'
          write(*,*) '# Lum3   = ', EL3*4.d0*pi, ' (luminosity)'
          write(*,*) '# opsf   = ', opsf
          write(*,*) '# ZERO   = ', ZERO, ' mag'
          write(*,*) '# FACTOR = ', FACTOR
          write(*,*) '# wl     = ', wl, ' mu'
          write(*,*)
        endif
c
c physical constants (cgs)
c
        ot=1.d0/3.d0
        KH=17
        pi=dacos(-1.d0)
        clight=2.99792458d5
        en0=6.0254d23
        rsuncm=6.960d10

c  Ramp ranges are set below. The following values seem to work. They may be changed.
        tlowtol=1500.d0
        thightol=50000.d0
        glowtol=4.0d0
        ghightol=4.0d0
       
        abun(1)=1.d0
        abun(2)=0.5d0
        abun(3)=0.3d0
        abun(4)=0.2d0
        abun(5)=0.1d0
        abun(6)=0.0d0
        abun(7)=-0.1d0
        abun(8)=-0.2d0
        abun(9)=-0.3d0
        abun(10)=-0.5d0
        abun(11)=-1.0d0
        abun(12)=-1.5d0
        abun(13)=-2.0d0
        abun(14)=-2.5d0
        abun(15)=-3.0d0
        abun(16)=-3.5d0
        abun(17)=-4.0d0
        abun(18)=-4.5d0
        abun(19)=-5.0d0
       
        glog(1)=0.0d0
        glog(2)=0.5d0
        glog(3)=1.0d0
        glog(4)=1.5d0
        glog(5)=2.0d0
        glog(6)=2.5d0
        glog(7)=3.0d0
        glog(8)=3.5d0
        glog(9)=4.0d0
        glog(10)=4.5d0
        glog(11)=5.0d0
        nn=100
        gau=0.d0
c
c read atmosphere files
c
        open(unit=22,file='atmcof.dat',status='old')
        read(22,*) grand
        close(22)
       
        open(unit=23,file='phoebe_atmcofplanck.dat',status='old')
        read(23,*) plcof
        close(23)
c
c convert units
c
        acm=rsuncm*a
       
        message(1,1)=0
        message(1,2)=0
        message(2,1)=0
        message(2,2)=0
        message(1,3)=0
        message(1,4)=0
        message(2,3)=0
        message(2,4)=0
c
c  The following lines take care of abundances that may not be among
c  the 19 Kurucz values (see abun array). abunin is reset at the allowed
c  value nearest the input value.
c
        call binnum(abun,19,abunin,iab)
       
        dif1=abunin-abun(iab)
        if(iab.ne.19) then
          dif2=abun(iab+1)-abun(iab)
          dif=dif1/dif2
          if ((dif.lt.0.d0).or.(dif.gt.0.5d0)) then
            iab=iab+1
          endif
        endif

        if(dif1.ne.0.d0) then
          write(6,287) abunin,abun(iab)
287       format('Input [M/H] = ',f6.3,' is not a value recognized by ',
     $      'the program. Replaced by ',f5.2)
        endif
        abunin=abun(iab)
        istart=1+(iab-1)*13200
c
c no spots
c
        nf1=1
        nf2=1
        NSP1=0
        NSP2=0
c
c no clouds
c
        ncl=0
       
        dint1=pi*(1.d0-xbol1/3.d0)
        dint2=pi*(1.d0-xbol2/3.d0)
        if(ld.eq.2) then
          dint1=dint1+PI*2.d0*ybol1/9.d0
          dint2=dint2+PI*2.d0*ybol2/9.d0
        else if (ld.eq.3) then
          dint1=dint1-.2d0*pi*ybol1
          dint2=dint2-.2d0*pi*ybol2
        endif
        NSTOT=NSP1+NSP2
        NP1=N1+1
        NP2=N1+N2+2
        IRTE=0
        IRVOL1=0
        IRVOL2=0

c Note: If mmsave array is re-dimensioned, change upper limit in DO 421 loop. imm of 124 is OK up to N=60.
        do imm=1,124
          mmsave(imm)=0
        enddo
        nn1=n1
c
c compute sines and cosines
c
        CALL SINCOS(1,nn1,N1,SNTH,CSTH,SNFI,CSFI,MMSAVE)
        CALL SINCOS(2,N2,N1,SNTH,CSTH,SNFI,CSFI,MMSAVE)

        hjd=hjd0

        i1st = 1
      endif  ! i1st

      if (i2nd.eq.0) then
c
c compute critial potentials and volumetric quantities
c
        CALL modlog(RV,GRX,GRY,GRZ,RVQ,GRXQ,GRYQ,GRZQ,MMSAVE,FR1,FR2,
     $    HLD,rm,poth,potc,gr1,gr2,alb1,alb2,n1,n2,f1,f2,mod,xincl,the,
     $    mode,snth,csth,snfi,csfi,grv1,grv2,xx1,yy1,zz1,xx2,yy2,zz2,
     $    glump1,glump2,csbt1,csbt2,gmag1,gmag2,glog1,glog2)

        CALL VOLUME(VOL1,RM,POTH,DP,F1,nn1,N1,1,RV,GRX,GRY,GRZ,RVQ,
     $    GRXQ,GRYQ,GRZQ,MMSAVE,FR1,FR2,HLD,SNTH,CSTH,SNFI,CSFI,SUMMD,
     $    SMD,GRV1,GRV2,XX1,YY1,ZZ1,XX2,YY2,ZZ2,CSBT1,CSBT2,GLUMP1,
     $    GLUMP2,GMAG1,GMAG2,glog1,glog2,GR1,1)

        CALL VOLUME(VOL2,RM,POTC,DP,F2,N2,N1,2,RV,GRX,GRY,GRZ,RVQ,
     $    GRXQ,GRYQ,GRZQ,MMSAVE,FR1,FR2,HLD,SNTH,CSTH,SNFI,CSFI,SUMMD,
     $    SMD,GRV1,GRV2,XX1,YY1,ZZ1,XX2,YY2,ZZ2,CSBT1,CSBT2,GLUMP1,
     $    GLUMP2,GMAG1,GMAG2,glog1,glog2,GR2,1)
c
c non-zero eccentricity case
c
        if (e.ne.0.d0) then
          DAP=1.d0+E
          P1AP=POTH-2.d0*E*RM/(1.d0-E*E)
          VL1=VOL1
         
          CALL VOLUME(VL1,RM,P1AP,DAP,F1,nn1,N1,1,RV,GRX,GRY,GRZ,RVQ,
     $      GRXQ,GRYQ,GRZQ,MMSAVE,FR1,FR2,HLD,SNTH,CSTH,SNFI,CSFI,SUMMD,
     $      SMD,GRV1,GRV2,XX1,YY1,ZZ1,XX2,YY2,ZZ2,CSBT1,CSBT2,GLUMP1,
     $      GLUMP2,GMAG1,GMAG2,glog1,glog2,GR1,2)
         
          DPDX1=(POTH-P1AP)*(1.d0-E*E)*.5d0/E
          P2AP=POTC-2.d0*E/(1.d0-E*E)
          VL2=VOL2
         
          CALL VOLUME(VL2,RM,P2AP,DAP,F2,N2,N1,2,RV,GRX,GRY,GRZ,RVQ,
     $      GRXQ,GRYQ,GRZQ,MMSAVE,FR1,FR2,HLD,SNTH,CSTH,SNFI,CSFI,SUMMD,
     $      SMD,GRV1,GRV2,XX1,YY1,ZZ1,XX2,YY2,ZZ2,CSBT1,CSBT2,GLUMP1,
     $      GLUMP2,GMAG1,GMAG2,glog1,glog2,GR2,2)
         
          DPDX2=(POTC-P2AP)*(1.d0-E*E)*.5d0/E
        endif

c save potentials
        PHSV=POTH
        PCSV=POTC
        if ((E.EQ.0.d0).and.(MOD.EQ.1)) then
          write(6,*) ' PROGRAM SHOULD NOT BE USED IN MODE 1 OR 3',
     $      ' WITH NON-ZERO ECCENTRICITY'
        endif

c
c compute lightcuve point at the normalisation phase phn
c
        CALL BBL_nbody(RV,GRX,GRY,GRZ,RVQ,GRXQ,GRYQ,GRZQ,MMSAVE,FR1,FR2,
     $    HLD,SLUMP1,SLUMP2,THETA,RHO,AA,BB,POTH,POTC,N1,N2,F1,F2,D,
     $    HLUM,clum,xh,xc,yh,yc,gr1,gr2,wl,sm1,sm2,tpolh,tpolc,sbrh,
     $    sbrc,tavh,tavc,alb1,alb2,xbol1,xbol2,ybol1,ybol2,phn,rm,xincl,
     $    hot,cool,snth,csth,snfi,csfi,tld,glump1,glump2,xx1,xx2,yy1,
     $    yy2,zz1,zz2,dint1,dint2,grv1,grv2,rftemp,rf1,rf2,csbt1,csbt2,
     $    gmag1,gmag2,glog1,glog2,fbin1,fbin2,delv1,delv2,count1,count2,
     $    delwl1,delwl2,resf1,resf2,wl1,wl2,dvks1,dvks2,tau1,tau2,emm1,
     $    emm2,hbarw1,hbarw2,xcl,ycl,zcl,rcl,op1,fcl,dens,encl,edens,
     $    taug,emmg,yskp,zskp,mode,iband,ifat1,ifat2,1)

c save the total flux at phn
        ALL=HOT+COOL+EL3
c
c check stellar surfaces
c
        KH=0
        if (kfo1.ne.0) then
          write(6,*) ' Primary star exceeds outer contact surface'
        endif
        if (KFF1.eq.1) then
          write(6,*) ' Primary component exceeds critical lobe'
        endif
        if (kfo2.ne.0) then
          write(6,*) ' Secondary star exceeds outer contact surface'
        endif
        if (KFF2.eq.1) then
          write(6,*) ' Secondary component exceeds critical lobe'
        endif
c
c compute absolute dimensions (mass, luminosity, radius, log g)
c
        rr1=.6203505d0*vol1**ot
        rr2=.6203505d0*vol2**ot
        tav1=10000.d0*tavh
        tav2=10000.d0*tavc

        call mlrg(a,period,rm,rr1,rr2,tav1,tav2,sms1,sms2,sr1,sr2,
     $    bolm1,bolm2,xlg1,xlg2)

        if (debug) then
          write(*,*) '# tav1 = ', tav1, " K"
          write(*,*) '# tav2 = ', tav2, " K"
          write(*,*) '# sms1 = ', sms1, " M_S"
          write(*,*) '# sms2 = ', sms2, " M_S"
          write(*,*) '# sr1 = ', sr1, " R_S"
          write(*,*) '# sr2 = ', sr2, " R_S"
          write(*,*) '# bolm1 = ', bolm1, " mag"
          write(*,*) '# bolm2 = ', bolm2, " mag"
          write(*,*) '# xlg1 = ', xlg1, " (cgs)"
          write(*,*) '# xlg2 = ', xlg2, " (cgs)"
          write(*,*)
        endif
       
        LL1=MMSAVE(N1)+1
        NPP2=NP2-1
        LL2=MMSAVE(NPP2)+1
        LLL1=MMSAVE(NP1)
        LLL2=MMSAVE(NP2)
        LLLL1=(LL1+LLL1)/2
        LLLL2=(LL2+LLL2)/2
c restore potentials
        POTH=PHSV
        POTC=PCSV
        PO(1)=POTH
        PO(2)=POTC

c output stellar surfaces and quantities
        if (debug) then
          NP1 = N1+1
          NP2 = N1+N2+2
          LLL1 = MMSAVE(NP1)
          LLL2 = MMSAVE(NP2)

          iu=10
          open(unit=iu, file="star1.dat", status="unknown")
          str = "# x & y & z [a = 1 units] & " //
     :      "RV [km s^-1] & " //
     :      "gravity g^gravity darkening coefficient [] & " //
     :      "log g [cgs] gravity & " //
     :      "R reflection coefficient [] & " //
     :      "T temperature [K] & " //
     :      "Delta phi [rad] & " //
     :      "Delta theta [rad] & " //
     :      "Delta F_lambda [erg s^-1 cm^-2 cm^-1]"
          write(iu,"(a)") str
          write(iu,*) "# primary, 1st quadrant only"
          do i = 1, LLL1
            IS = i
            write(iu,*) XX1(i), YY1(i), ZZ1(i), RV(i),
     :        GRV1(i), glog1(i), RF1(i), tld(i),
     :        Delta_phi(is), Delta_theta(is), Delta_F_lambda(is)
          enddo
          close(iu)

          open(unit=iu, file="star2.dat", status="unknown")
          write(iu,"(a)") str
          write(iu,*) "# secondary, 1st quadrant only"
          do i = 1, LLL2
            IS = i+LLL1  ! index for common arrays for both components
            write(iu,*) XX2(i), YY2(i), ZZ2(i), RVQ(i),
     :        GRV2(i), glog2(i), RF2(i), tld(is),
     :        Delta_phi(is), Delta_theta(is), Delta_F_lambda(is)
          enddo
          close(iu)
        endif

        i2nd = 1
      endif  ! i2nd

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c compute lightcurve (using repeated calls)
c
      CALL modlog(RV,GRX,GRY,GRZ,RVQ,GRXQ,GRYQ,GRZQ,MMSAVE,FR1,FR2,
     $  HLD,rm,poth,potc,gr1,gr2,alb1,alb2,n1,n2,f1,f2,mod,xincl,the,
     $  mode,snth,csth,snfi,csfi,grv1,grv2,xx1,yy1,zz1,xx2,yy2,zz2,
     $  glump1,glump2,csbt1,csbt2,gmag1,gmag2,glog1,glog2)

      phas = gpha_

      CALL BBL_nbody(RV,GRX,GRY,GRZ,RVQ,GRXQ,GRYQ,GRZQ,MMSAVE,FR1,FR2,
     $  HLD,SLUMP1,SLUMP2,THETA,RHO,AA,BB,POTH,POTC,N1,N2,F1,F2,D,hlum,
     $  clum,xh,xc,yh,yc,gr1,gr2,wl,sm1,sm2,tpolh,tpolc,sbrh,sbrc,
     $  tavh,tavc,alb1,alb2,xbol1,xbol2,ybol1,ybol2,phas,rm,xincl,
     $  hot,cool,snth,csth,snfi,csfi,tld,glump1,glump2,xx1,xx2,yy1,
     $  yy2,zz1,zz2,dint1,dint2,grv1,grv2,rftemp,rf1,rf2,csbt1,csbt2,
     $  gmag1,gmag2,glog1,glog2,fbin1,fbin2,delv1,delv2,count1,count2,
     $  delwl1,delwl2,resf1,resf2,wl1,wl2,dvks1,dvks2,tau1,tau2,emm1,
     $  emm2,hbarw1,hbarw2,xcl,ycl,zcl,rcl,op1,fcl,dens,encl,edens,
     $  taug,emmg,yskp,zskp,mode,iband,ifat1,ifat2,0)

      HTT=HOT
      TOTAL=HTT+COOL+EL3
      TOTALL=TOTAL/ALL
      TOT=TOTALL*FACTOR
c
c apply noise
c
      if (stdev.gt.0.d0) then
        call rangau(seed,nn,stdev,gau)
        ranf=1.d0+gau*dsqrt(totall**noise)
        total=total*ranf
        tot=tot*ranf
        totall=totall*ranf
      endif
c
c compute magnitude
c
      SMAGG=-1.085736d0*dlog(TOTALL)+ZERO
c
c Rossiter effect
c
      vkm1_ = vkm1
      vkm2_ = vkm2

      END


