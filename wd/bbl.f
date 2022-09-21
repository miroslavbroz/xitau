c bbl.f
c modified Apr 16th 2016 by MB

      SUBROUTINE BBL(RV,GRX,GRY,GRZ,RVQ,GRXQ,GRYQ,GRZQ,MMSAVE,FR1,FR2,
     $  HLD,SLUMP1,SLUMP2,THETA,RHO,AA,BB,PHSV,PCSV,N1,N2,F1,F2,d,hlum,
     $  clum,xh,xc,yh,yc,gr1,gr2,wl,sm1,sm2,tpolh,tpolc,sbrh,sbrc,
     $  tavh,tavc,alb1,alb2,xbol1,xbol2,ybol1,ybol2,phas,rm,
     $  xincl,hot,cool,snth,csth,snfi,csfi,tld,glump1,glump2,xx1,xx2,
     $  yy1,yy2,zz1,zz2,dint1,dint2,grv1,grv2,rftemp,rf1,rf2,csbt1,
     $  csbt2,gmag1,gmag2,glog1,glog2,fbin1,fbin2,delv1,delv2,count1,
     $  count2,delwl1,delwl2,resf1,resf2,wl1,wl2,dvks1,dvks2,tau1,tau2,
     $  emm1,emm2,hbarw1,hbarw2,xcl,ycl,zcl,rcl,op1,fcl,dens,encl,edens,
     $  taug,emmg,yskp,zskp,mode,iband,ifat1,ifat2,ifphn)

      implicit real*8 (a-h,o-z)

c other input parameters are passed in common blocks!

c  Version of December 18, 2003

      DIMENSION RV(*),GRX(*),GRY(*),GRZ(*),RVQ(*),GRXQ(*),GRYQ(*),
     $  GRZQ(*),MMSAVE(*),FR1(*),FR2(*),HLD(*),SLUMP1(*),SLUMP2(*),
     $  THETA(*),RHO(*),AA(*),BB(*),SNTH(*),CSTH(*),SNFI(*),CSFI(*),
     $  TLD(*),GLUMP1(*),GLUMP2(*),XX1(*),XX2(*),YY1(*),YY2(*),ZZ1(*),
     $  ZZ2(*),GRV1(*),GRV2(*),RFTEMP(*),RF1(*),RF2(*),CSBT1(*),
     $  CSBT2(*),GMAG1(*),GMAG2(*),glog1(*),glog2(*)
      dimension fbin1(*),fbin2(*),delv1(*),delv2(*),count1(*),count2(*),
     $  delwl1(*),delwl2(*),resf1(*),resf2(*),wl1(*),wl2(*),dvks1(*),
     $  dvks2(*),tau1(*),tau2(*),hbarw1(*),hbarw2(*),taug(*),emm1(*),
     $  emm2(*),emmg(*)
      dimension xcl(*),ycl(*),zcl(*),rcl(*),op1(*),fcl(*),dens(*),
     $  edens(*),encl(*),yskp(*),zskp(*)

      COMMON /INVAR/ KH,IPBDUM,IRTE,NREF,IRVOL1,irvol2,mref,ifsmv1,
     $  ifsmv2,icor1,icor2,ld,ncl,jdphs,ipc
      COMMON /FLVAR/ PSHIFT,DP,EF,EFC,ECOS,perr0,PHPER,pconsc,pconic,
     $  PHPERI,VSUM1,VSUM2,VRA1,VRA2,VKM1,VKM2,VUNIT,vfvu,trc,qfacd
      common /nspt/ nsp1,nsp2
      common /ardot/ dperdt,hjd,hjd0,perr
      common /spots/ snlat(2,100),cslat(2,100),snlng(2,100),
     $  cslng(2,100),rdsp(2,100),tmsp(2,100),xlng(2,100),kks(2,100),
     $  Lspot(2,100)
      COMMON /ECCEN/ E,A,PERIOD,VGA,SINI,VF,VFAC,VGAM,VOL1,VOL2,IFC
      common /prof2/ vo1,vo2,ff1,ff2,du1,du2,du3,du4,du5,du6,du7

c rm    ... M2/M1 mass ratio (input parameter)
c qfac1 ... 1/(1+M2/M1) = M1/(M1+M2)
c qfac  ... M2/(M1+M2)

      pi=3.141592653589793d0
      twopi=pi+pi
      ff1=f1
      ff2=f2
      qfac1=1.d0/(1.d0+rm)
      qfac=rm*qfac1

      IF(MODE.EQ.1) XC=XH
      if(mode.eq.1) yc=yh
      PSFT=PHAS-PHPERI
      do while (PSFT.GT.1.d0)
        PSFT=PSFT-1.d0
      enddo
      do while (PSFT.LT.0.d0)
        PSFT=PSFT+1.d0
      enddo
      XMEAN=PSFT*twopi
      TR=XMEAN

c PHAS   ... phase (input parameter)
c PHPERI ... phase of superior conjunction
c XMEAN  ... M mean anomaly
c TR     ... f true anomaly
c E      ... e eccentricity

c kp     ... c(k)omponent
c ifsmv1 ... spot motion switch
c nsp    ... number of spots

      do kp=1,2
        nsp=nsp1*(2-kp)+nsp2*(kp-1)
        ff=f1*dfloat(2-kp)+f2*dfloat(kp-1)
        ifsmv=ifsmv1*(2-kp)+ifsmv2*(kp-1)
        if(ifsmv.ne.0) then
          do i=1,nsp
            xlg=xlng(kp,i)+twopi*ff*(phas-pconsc)-(tr-trc)
            snlng(kp,i)=dsin(xlg)
            cslng(kp,i)=dcos(xlg)
          enddo
        endif
      enddo

c DUM    ... E eccentric anomaly (not needed here)
c U      ... phi true longitude (f+omega) [rad]
c GPHA   ... phi/(2pi)-0.25 true phase wrt. this orientation: O--o
c EF     ... a(1-e^2), a = 1
c D      ... r distance, a = 1 units
c qfacd  ... M2/(M1+M2) r

      if(e.ne.0.d0) then
        call KEPLER(XMEAN,E,DUM,TR)
      endif
      U=TR+PERR
      COSU=dcos(U)
      GPHA=U*.1591549d0-.25d0
      do while (GPHA.lt.0.d0)
        GPHA=GPHA+1.d0
      enddo
      do while (GPHA.GE.1.d0)
        GPHA=GPHA-1.d0
      enddo
      D=EF/(1.d0+E*dcos(TR))
      qfacd=qfac*d

c e = 0 => IRTE = 1
c non-zero eccentricity case

      IF(IRTE.NE.1) then

        CALL LCR(RV,GRX,GRY,GRZ,RVQ,GRXQ,GRYQ,GRZQ,MMSAVE,FR1,FR2,HLD,
     $    slump1,SLUMP2,RM,PHSV,PCSV,N1,N2,F1,F2,D,HLUM,CLUM,xh,xc,yh,
     $    yc,gr1,gr2,SM1,SM2,TPOLH,TPOLC,SBRH,SBRC,IFAT1,IFAT2,TAVH,
     $    TAVC,alb1,alb2,xbol1,xbol2,ybol1,ybol2,vol1,vol2,snth,csth,
     $    snfi,csfi,tld,glump1,glump2,xx1,xx2,yy1,yy2,zz1,zz2,dint1,
     $    dint2,grv1,grv2,csbt1,csbt2,rftemp,rf1,rf2,gmag1,gmag2,glog1,
     $    glog2,mode,iband)

      endif

c qfac  ... M2/(M1+M2)
c qfac1 ... 1/(1+M2/M1) = M1/(M1+M2)
c SINI  ... sin(i) wrt. observer
c ECOS  ... e cos(omega)
c COSU  ... cos(phi)
c EFC   ... sqrt(1-e^2)
c VGAM  ... gamma velocity

      VO1=qfac*SINI*(ECOS+COSU)/EFC+VGAM
      VO2=-qfac1*SINI*(ECOS+COSU)/EFC+VGAM

c only GPHA and XINCL are needed in light subroutine

      call light(gpha,xincl,xh,xc,yh,yc,n1,n2,hot,cool,rv,grx,gry,grz,
     $  rvq,grxq,gryq,grzq,mmsave,theta,rho,aa,bb,slump1,slump2,somhot,
     $  somkul,d,wl,snth,csth,snfi,csfi,tld,gmag1,gmag2,glog1,glog2,
     $  fbin1,fbin2,delv1,delv2,count1,count2,delwl1,delwl2,resf1,resf2,
     $  wl1,wl2,dvks1,dvks2,tau1,tau2,emm1,emm2,hbarw1,hbarw2,xcl,ycl,
     $  zcl,rcl,op1,fcl,edens,encl,dens,taug,emmg,yskp,zskp,iband,ifat1,
     $  ifat2,ifphn)

c F1 ... synchronicity ratio for primary
c F2 ... dtto for secondary

      VRA1=0.d0
      VRA2=0.d0
      IF(HOT.GT.0.d0) VRA1=F1*SOMHOT/HOT
      IF(COOL.GT.0.d0) VRA2=F2*SOMKUL/COOL
      vsum1=vo1
      vsum2=vo2
      if(icor1.eq.1) vsum1=vo1+vra1
      if(icor2.eq.1) vsum2=vo2+vra2
      vfcc=vfac/vunit
      VKM1=VSUM1*vfcc
      VKM2=VSUM2*vfcc

      RETURN
      END

