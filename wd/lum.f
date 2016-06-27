c lum.f
c modified Jun 10th 2016 by MB

c Compute the luminosity of one component of the EB as a sum over its surface.
c Gravity darkening is also accounted for;
c the limb darkening is applied at the end (for the whole disk).

      Subroutine lum (xlum,x,y,tpoll,n,n1,komp,sbr,rv,rvq,glump1,
     $  glump2,glog1,glog2,grv1,grv2,mmsave,summ,fr,sm,ifat,vol,rm,om,
     $  f,d,snth,iband)

c   Version of January 8, 2003

c xlum ... HLUM or CLUM luminosity from lc.in
c tpoll ... temperature [10^4 K] @ the pole
c komp ... component number [1|2]
c sbr ... xlum divided by computed luminosity normalized by polar one and affected by darkening
c glump1 ... R^2 sin(theta)/cos(beta) for primary
c glump2 ... dtto for secondary
c glog1 ... surface log(g) for primary
c glog2 ... dtto for secondary
c grv1 ... gravity |g| for primary
c grv2 ... dtto for secondary
c summ ... surface times gravity?
c sm ... surface area
c vol ... volume

      implicit real*8 (a-h,o-z)

      dimension rv(*),rvq(*),mmsave(*),fr(*),snth(*),glump1(*),
     $  glump2(*),glog1(*),glog2(*),grv1(*),grv2(*)
      dimension message(2,4)
      common /atmmessages/ message,kompcom
      common /radi/ R1H,RLH,R1C,RLC
      common /invar/ khdum,ipbdum,irtedm,nrefdm,irv1dm,irv2dm,mrefdm,
     $  is1dm,is2dm,ic1dm,ic2dm,ld,ncl,jdphs,ipc
      common /gpoles/ gplog1,gplog2

      kompcom=komp
      TPOLE=10000.d0*TPOLL  ! temperature [K] @ the pole
      KR=0
      cmp=dfloat(komp-1)
      cmpp=dfloat(2-komp)
      gplog=cmpp*gplog1+cmp*gplog2
      if (ifat.eq.0) then
        call planckint(tpole,iband,pollog,polin)
      else
        call atmx(tpole,gplog,iband,pollog,polin)
      endif

      EN=dfloat(N)
      DELTH=1.570796326794897d0/EN
      SUM=0.d0
      SUMM=0.d0
      SM=0.d0
      VOL=0.d0

      DO 36 I=1,N
        IPN1=I+N1*(komp-1)
        SINTH=SNTH(IPN1)
        EM=SINTH*EN*1.3d0
        MM=EM+1.d0
        XM=dfloat(MM)
        DELFI=3.141592653589793d0/XM
        DFST=DELFI*SINTH
        SUMJ=0.d0
        SUMMJ=0.d0
        SMJ=0.d0
        VOLJ=0.d0

        DO 26 J=1,MM
          IP=(komp-1)*(N1+1)+I
          IX=MMSAVE(IP)+J
          IF(komp.EQ.1) GOTO 39
          IF(RVQ(IX).EQ.-1.d0) GOTO 25
          R=RVQ(IX)
          GOTO 49
39        continue
          IF(RV(IX).EQ.-1.d0) GOTO 25
          R=RV(IX)
49        continue

          grav=cmpp*grv1(ix)+cmp*grv2(ix)
          TLOCAL=TPOLE*dsqrt(dsqrt(GRAV))  ! Stefan-Boltzmann law, Wilson & Devinney (1971), Eq. (6)
          glogg=cmpp*glog1(ix)+cmp*glog2(ix)
          if (ifat.eq.0) then
            call planckint(tlocal,iband,xinlog,xint)
          else
            call atmx(tlocal,glogg,iband,xinlog,xint)
          endif

          GRAVM=xint/polin
          di=cmpp*glump1(ix)+cmp*glump2(ix)
          DIF=DI*GRAVM
          DIFF=DI*GRAV
          SMJ=SMJ+DI
          SUMJ=SUMJ+DIF
          SUMMJ=SUMMJ+DIFF
          VOLJ=VOLJ+R*R*R*FR(IX)
          GOTO 26
25        continue
          KR=1
26      enddo

        SMJ=SMJ*DELFI
        SUMJ=SUMJ*DELFI
        SUMMJ=SUMMJ*DELFI
        SM=SM+SMJ
        SUMM=SUMM+SUMMJ
        VOL=VOL+VOLJ*DFST
        SUM=SUM+SUMJ
36    enddo

      darkin=3.141592653589793d0*(1.d0-x/3.d0)
      if(ld.eq.2) darkin=darkin+.6981317d0*y
      if(ld.eq.3) darkin=darkin-.6283185d0*y
      SBR=.25d0*XLUM/(SUM*DELTH*DARKIN)
      SM=SM*DELTH*4.d0
      SUMM=SUMM*DELTH*4.d0  ! we've 4 quadrants
      VOL=VOL*1.3333333333333d0*DELTH

      IF(KR.EQ.0) RETURN

      CALL ELLONE(F,D,RM,XL1,OMD,XLD,omdum)
      CALL NEKMIN(RM,OM,XL1,ZD)

      IF(komp.EQ.2) XL1=D-XL1
      R1=cmpp*R1H+cmp*R1C
      RL=cmpp*RLH+cmp*RLC
      VOL=VOL+1.047198d0*XL1*R1*RL

      RETURN
      END


