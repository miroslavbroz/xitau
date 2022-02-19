!--------------------------------------------------------------------------
!Name:          RocheLobe.f90 (version 1.0)
!Authors:       Leahy, D. 
!Date:          December 15, 2014
!Email:         leahy@ucalgary.ca
!
! MAIN CODE FOR THE Roche Lobe MODEL 
!
! Free Parameters are:
! q (mass ratio M2/M1, for Roche Lobe of M1)
! F (fillout factor, F defined by Mochnacki 1984)
! P (rotation rate of M1 relative to synchronous rotation)
!    P=\Omega_{M1)/\Omega_{binary}, P<1 is subsynchronous, 
!    P=1 is synchronous, P>1 is supersynchronous
!
! *** DOUBLE PRECISION ***
!
program  RocheLobe
implicit none
real*8 :: q,F,P,RL1,OmegaL1,X2,Om2,X3,Om3,OmegaF,Rfr,Rbk,Ry,Rz
real*8 :: vol,Req,area,Ra,theta,phi,Rthephi,th0,ph0,pi,pi2
integer :: ifile,test,irad
pi = 3.14159265358979323846d0
!pi2=4.d0*datan(1.d0)
!write(6,*)"compare pi,4*datan(1.d0)", pi, pi2

! options for input: 
! 1. single value of q (F=P=1 assumed) with output R's,vol,area to terminal
! 2. file of set of q (F=P=1 assumed) with output R's,vol,area to file
! 3. single value of q,theta,phi (F=P=1 assumed) with output R(theta,phi) to terminal
! 4. file of set of (q,theta,phi) (F=P=1 assumed) with output R(theta,phi) to file
! 5. single value of q,F,P with output R's,vol,area to terminal
! 6. file of set of (q,F,P) with output R's,vol,area to file
! 7. single value of q,theta,phi,F,P with output R(theta,phi) to terminal
! 8. file of set of (q,theta,phi,F,P) with output R(theta,phi) to file
write(6,*)"**************Roche Lobe Radius Calculator***************"
write(6,*)"           (D.Leahy, University of Calgary, 2014)     "
write(6,*)"  Free Parameters are:"
write(6,*)"  q (mass ratio M2/M1, for Roche Lobe of M1)"
write(6,*)"  F (fillout factor, with F defined by Mochnacki 1984)"
write(6,*)"  P (rotation rate of M1 relative to synchronous rotation)"
write(6,*)"      P=\Omega_{M1)/\Omega_{binary}, P<1 is subsynchronous," 
write(6,*)"      P=1 is synchronous, P>1 is supersynchronous"
write(6,*)"  F can be in the range 0.1 to 1.0, P in the range 0.01 to 2."
write(6,*)"  In cases below where F and P are not specified, it is assumed F=P=1.0"
write(6,*)"  For file input, one set of input parameters per row is assumed, input is terminated by end of file."
write(6,*)" "

DO
write(6,*)"*************The following options are available*****************" 
Write(6,*)"1. Single input q and output radii,vol,area to terminal;"
Write(6,*)"2. Input set q from file Inputq.txt and output radii,vol,area to file;"
Write(6,*)"3. Single input q,theta,phi and output R(theta,phi) to terminal;"
Write(6,*)"4. Input set (q,theta,phi) from InputqThePhi.txt and output R(theta,phi) to file;"
Write(6,*)"5. Single input q,F,P and output radii,vol,area to terminal;"
Write(6,*)"6. Input set (q,P,F) from InputqPF.txt and output radii,vol,area to file;"
Write(6,*)"7. Single input q,F,P,theta,phi and output R(theta,phi) to terminal;"
Write(6,*)"8. Input set (q,theta,phi,F,P) from InputqThePhiFP.txt and output R(theta,phi) to file;"
write(6,*)"----> enter the option number(any other integer to exit):" 
Read(5,*) ifile

! for cases 1,2,5,6 need to calculate values of RL1,OmegaL1,OmegaF,Rbk,Ry,Rz,vol,Req,area,Ra
if (ifile.eq.1) then
Write(6,*) "Enter Mass Ratio(q):"
Read(5,*) q
F=1.0d0;P=1.0d0
!subroutine to calculate RL1,OmegaL1,OmegaF,Rfr=RL1,Rbk,Ry,Rz,vol,Req,area,Ra
Call GetRadii(q,F,P,RL1,OmegaL1,X2,Om2,X3,Om3,OmegaF,Rfr,Rbk,Ry,Rz,vol,Req,area,Ra)
write(6,'(A,F10.5,A,F15.10,A,F15.10)')          "  q=",  q,"     RL1=", RL1," OmegaL1=",OmegaL1
write(6,'(A,F15.10,A,F15.10,A,F15.10,A,F15.10)') " X2=", X2," OmegaX2=", Om2,"      X3=",  X3," OmegaX3=",Om3
write(6,'(A,F15.10,A,F15.10,A,F15.10,A,F15.10)') "Rbk=",Rbk,"      Ry=",  Ry,"      Rz=",Rz
write(6,'(A,F15.10,A,F15.10,A,F15.10,A,F15.10)') "vol=",vol,"   R_vol=", Req,"    area=",area,"  R_area=",Ra

elseif (ifile.eq.2) then
Write(6,*) "-- Output saved in file qRVolArea.txt -- "
Write(6,*) "values are: q,RL1,OmegaL1,X2,Om2,X3,Om3,Rbk,Ry,Rz,vol,Req,area,Ra"
Open(11,file="Inputq.txt");Open(12,file="qRVolArea.txt")
DO
   READ(11,*,IOSTAT=test) q
   IF (test > 0)  THEN
   write(6,*)"error in input file"
   exit
   ELSE IF (test < 0) THEN
   write(6,*)"end of file reached"
   exit
   ELSE
   F=1.d0;P=1.d0
   Call GetRadii(q,F,P,RL1,OmegaL1,X2,Om2,X3,Om3,OmegaF,Rfr,Rbk,Ry,Rz,vol,Req,area,Ra)
   !Save calculated radii to file:
   write(12,'(10F15.10)') q,RL1,OmegaL1,X2,Om2,X3,Om3,Rbk,Ry,Rz,vol,Req,area,Ra
   END IF
END DO

elseif (ifile.eq.5) then
Write(6,*) "Free Parameters: Mass Ratio(q), Fillout Factor(F), Rotation Parameter(P)"
Write(6,*) "Enter Free Parameters: q, F, P:"
Read(5,*) q, F, P
!subroutine to calculate RL1,OmegaL1,OmegaF,Rfr,Rbk,Ry,Rz,vol,Req,area,Ra
Call GetRadii(q,F,P,RL1,OmegaL1,X2,Om2,X3,Om3,OmegaF,Rfr,Rbk,Ry,Rz,vol,Req,area,Ra)
write(6,'(A,F10.5,A,F10.5,A,F10.5,A,F15.10)') " q=",q," F=",F," P=",P,"     RL1=", RL1," OmegaL1=",OmegaL1
write(6,'(A,F15.10,A,F15.10,A,E15.8,A,E15.8)') " X2=", X2," OmegaX2=", Om2,"     X3=",  X3," OmegaX3=",Om3
write(6,'(A,F15.10,A,F15.10,A,F15.10,A,F15.10)') "Rfr=",Rfr,"     Rbk=", Rbk,"     Ry=",  Ry,"      Rz=",Rz
write(6,'(A,F15.10,A,F15.10,A,F15.10,A,F15.10)') "vol=",vol,"   R_vol=", Req,"   area=",area,"  R_area=",Ra

elseif (ifile.eq.6) then
Write(6,*) "-- Output saved in file qFPRVolArea.txt -- "
Write(6,*) "values are: q,F,P,RL1,OmegaL1,X2,Om2,X3,Om3,OmegaF,Rfr,Rbk,Ry,Rz,vol,Req,area,Ra"
Open(11,file="InputqFP.txt");Open(12,file="qFPRVolArea.txt")
DO
   READ(11,*,IOSTAT=test) q, F, P
   IF (test > 0)  THEN
   write(6,*)"error in input file"
   exit
   ELSE IF (test < 0) THEN
   write(6,*)"end of file reached"
   exit
   ELSE
   Call GetRadii(q,F,P,RL1,OmegaL1,X2,Om2,X3,Om3,OmegaF,Rfr,Rbk,Ry,Rz,vol,Req,area,Ra)
   !Save calculated radii to file:
   write(12,'(20F15.10)') q,F,P,RL1,OmegaL1,X2,Om2,X3,Om3,OmegaF,Rfr,Rbk,Ry,Rz,vol,Req,area,Ra
   END IF
END DO

! for cases 3,4,7,8 need to calculate values of R(theta,phi)
elseif (ifile.eq.3) then
  write(6,*) "Output values of theta,phi will match input units"
  write(6,*) "If input theta,phi are in radians, enter 0, if in degrees, enter 1:"
  read(5,*) irad
  Write(6,*) "Enter Free Parameters: q, theta, phi:"
    Read(5,*) q, theta,phi
    F=1.d0;P=1.d0
    if (irad.eq.0) then
    th0=theta; ph0=phi
    elseif (irad.eq.1) then
    th0=theta*pi/180.d0; ph0=phi*pi/180.d0
    endif
  Call GetDerivRoot(q,P,RL1,1)
  OmegaL1 = potential(RL1,pi/2.d0,0.d0,q,P)
  Rthephi=Rtp(th0,ph0,q,P,F,RL1,OmegaL1)
  write(6,'(A,4F15.10)') "q,theta,phi,R(theta,phi)=",q,theta,phi,Rthephi

elseif (ifile.eq.4) then
  write(6,*) "Output values of theta,phi will match input units"
  write(6,*) "If input theta,phi are in radians, enter 0, if in degrees, enter 1:"
  read(5,*) irad
  Write(6,*) "---> Output saved in  -- qThePhiR.txt -- "
  Write(6,*) "values are: q,theta,phi,R(theta,phi)"
  Open(11,file="InputqThePhi.txt");Open(12,file="qThePhiR.txt")
  DO
   READ(11,*,IOSTAT=test) q,theta,phi
   F=1.d0;P=1.d0
   IF (test > 0)  THEN
   write(6,*)"error in input file"
   exit
   ELSE IF (test < 0) THEN
   write(6,*)"end of file reached"
   exit
   ELSE 
    if (irad.eq.0) then
    th0=theta; ph0=phi
    elseif (irad.eq.1) then
    th0=theta*pi/180.d0; ph0=phi*pi/180.d0
    endif
   Call GetDerivRoot(q,P,RL1,1)
   OmegaL1 = potential(RL1,pi/2.d0,0.d0,q,P)
   Rthephi=Rtp(th0,ph0,q,P,F,RL1,OmegaL1)
   write(12,'(4F15.10)') q,theta,phi,Rthephi
   END IF
  END DO

elseif (ifile.eq.7) then
  write(6,*) "Output values of theta,phi will match input units"
  write(6,*) "If input theta,phi are in radians, enter 0, if in degrees, enter 1:"
  read(5,*) irad
  Write(6,*) "Enter Free Parameters: q, theta, phi, F, P:"
    Read(5,*) q, theta,phi,F, P
    if (irad.eq.0) then
    th0=theta; ph0=phi
    elseif (irad.eq.1) then
    th0=theta*pi/180.d0; ph0=phi*pi/180.d0
    endif
  !calculate potential
  Call GetDerivRoot(q,P,RL1,1)
  OmegaL1 = potential(RL1,pi/2.d0,0.d0,q,P)
  !calculate the potential at the given filling factor F
  OmegaF = (OmegaL1+q*q/2.d0/(1.d0+q))/F - q*q/2.d0/(1.d0+q)
  !Call GetPotRoot(q,P,F,RL1,OmegaF,th0,ph0,Rthephi)
  Rthephi=Rtp(th0,ph0,q,P,F,RL1,OmegaF)
  write(6,'(A,6F15.10)') "q,theta,phi,F,P,R(theta,phi)=",q,theta,phi,F,P,Rthephi

elseif (ifile.eq.8) then
  write(6,*) "Output values of theta,phi will match input units"
  write(6,*) "If input theta,phi are in radians, enter 0, if in degrees, enter 1:"
  read(5,*) irad
  Write(6,*) "---> Output saved in  -- qThePhiFPR.txt -- "
  Write(6,*) "values are: q,theta,phi,F,P,R(theta,phi)"
  Open(11,file="InputqThePhiFP.txt");Open(12,file="qThePhiFPR.txt")
  DO
   READ(11,*,IOSTAT=test) q,theta,phi,F,P
   IF (test > 0)  THEN
   write(6,*)"error in input file"
   exit
   ELSE IF (test < 0) THEN
   write(6,*)"end of file reached"
   exit
   ELSE 
    if (irad.eq.0) then
    th0=theta; ph0=phi
    elseif (irad.eq.1) then
    th0=theta*pi/180.d0; ph0=phi*pi/180.d0
    endif
   Call GetDerivRoot(q,P,RL1,1)
   OmegaL1 = potential(RL1,pi/2.d0,0.d0,q,P)
   !calculate the potential at the given filling factor F
   OmegaF = (OmegaL1+q*q/2.d0/(1.d0+q))/F - q*q/2.d0/(1.d0+q)
   Rthephi=Rtp(th0,ph0,q,P,F,RL1,OmegaF)
   write(12,'(6F15.10)') q,theta,phi,F,P,Rthephi
   END IF
  END DO

elseif (ifile.ge.9 .or. ifile.le.0) then
  exit
endif
!close files after each calculation
Close(11);Close(12)

End DO

CONTAINS

! ********************************************************************

subroutine checkFP(F,P)
implicit none
real*8, intent(in) :: F, P

if (F.lt.0.1d0.or.F.gt.1.0d0) then
Write(6,*) "F not in allowed range"
stop
elseif (P.lt.0.01d0.or.P.gt.2.0d0) then
Write(6,*) "P not in allowed range"
stop
endif

end subroutine checkFP

! ********************************************************************

subroutine GetRadii(q,F,P,RL1,OmegaL1,X2,Om2,X3,Om3,OmegaF,Rfr,Rbk,Ry,Rz,vol,Req,area,Ra)
implicit none
real*8, intent(in) :: q, F, P
real*8, intent(out) :: RL1,OmegaL1,X2,Om2,X3,Om3,OmegaF,Rfr,Rbk,Ry,Rz,vol,Req,area,Ra
real*8 :: theta, phi, Rroot, pi
integer :: iroot
pi = 3.14159265358979323846d0

call checkFP(F,P)

! First find potential at L1 point along x-axis, OmegaL1, from setting 
!derivative of Roche Potential function to zero
theta=pi/2.d0;phi=0.d0;iroot=1
Call GetDerivRoot(q,P,RL1,iroot)
OmegaL1 = potential(RL1,theta,phi,q,P)

!Find the other two roots along the x=axis, for x<0 and for x>1:
theta=pi/2.d0;phi=0.d0;iroot=2
Call GetDerivRoot(q,P,X2,iroot)
Om2 = potential(X2,theta,phi,q,P)
theta=pi/2.d0;phi=0.d0;iroot=3
Call GetDerivRoot(q,P,X3,iroot)
Om3 = potential(X3,theta,phi,q,P)

!calculate the potential at the given filling factor F
OmegaF = (OmegaL1+q*q/2.d0/(1.d0+q))/F - q*q/2.d0/(1.d0+q)

theta = pi/2.d0;phi =0.d0
!subroutine to calculate R(theta,phi) for given potential 
!using root of potential function
Call GetPotRoot(q,P,F,RL1,OmegaF,theta,phi,Rroot)
Rfr = Rroot

theta = pi/2.d0;phi = pi
Call GetPotRoot(q,P,F,RL1,OmegaF,theta,phi,Rroot)
Rbk = Rroot

theta = pi/2.d0;phi = pi/2.d0
Call GetPotRoot(q,P,F,RL1,OmegaF,theta,phi,Rroot)
Ry = Rroot

theta = 0.d0;phi = 0.d0
Call GetPotRoot(q,P,F,RL1,OmegaF,theta,phi,Rroot)
Rz = Rroot

!call subroutine to calculate volume and area for volume radius R_eq and 
!area radius R_a 
Call GetVolArea(q, P,F,RL1, OmegaF, vol, area)
Req = (3.d0*vol/4.d0/pi)**(1.d0/3.d0)
Ra = (area/4.d0/pi)**(1.d0/2.d0)

end subroutine GetRadii

! ********************************************************************

subroutine GetDerivRoot(q,P,xroot,iroot)
real*8, intent(in) :: q, P
integer, intent(in) :: iroot
real*8, intent(out) :: xroot
real*8 :: a, b, c, fa, fb, fc, xcon, d, s, fs, dum
logical :: mflag, con1, con2, con3, con4, con5
!convergence criterion in x
xcon = 1.d-9
! initial guess for 3 cases 0<x<1, x<0, x>1
  if(iroot.eq.1) then
  a = 1.d-3;b = 0.999d0
  elseif (iroot.eq.2) then
  a=-1.d2; b=-0.0001d0
  elseif (iroot.eq.3) then
  a=1.001d0; b=1.d4
  endif
fa = deriv(a,q,P,iroot)
fb = deriv(b,q,P,iroot)
!write(6,*) "a,b,fa,fb",a,b,fa,fb

!test that root is bracketed by a,b
if (fa*fb .ge. 0.d0) then
write(6,*) "bad initial guess in deriv root, iroot=",iroot
stop 
endif

!swap a,b if needed
if (dabs(fa) .lt. dabs(fb)) then
dum = a; a = b; b = dum
dum = fa; fa = fb; fb = dum
!write(6,*) "swap: a,b,fa,fb",a,b,fa,fb
end if 

c = a
fc = deriv(c,q,P,iroot)
!mflag = 1
mflag = .true.

do while ((dabs(b-a) .gt. xcon) .and. (fb .ne. 0.d0))
  if ((fa .ne. fc) .and. (fb .ne. fc)) then
  s = a*fb*fc/((fa-fb)*(fa-fc))+b*fa*fc/((fb-fa)*(fb-fc))+c*fa*fb/((fc-fa)*(fc-fb))
  else
  s=b-fb*(b-a)/(fb-fa)
  endif
  con1 = (s.le.(3.d0*a+b)/4.d0) .or. (s.ge.b)
  con2 = mflag.and.(dabs(s-b).ge.dabs(b-c)/2.d0)
  con3 = (.not.mflag).and.(dabs(s-b).ge.dabs(c-d)/2.d0)
  con4 = mflag.and.(dabs(b-c).lt.dabs(xcon))
  con5 = (.not.mflag).and.(dabs(c-d).lt.dabs(xcon))
  if (con1.or.con2.or.con3.or.con4.or.con5) then
  s=(a+b)/2.d0
  mflag=.true.
  else
  mflag=.false.
  endif

  fs = deriv(s,q,P,iroot)
  d=c
  c=b; fc=deriv(c,q,P,iroot)

  if (fa*fs.lt.0.d0) then 
  b=s; fb=fs
  else 
  a=s; fa=fs
  endif 

!swap a,b if needed
  if (dabs(fa) .lt. dabs(fb)) then
  dum = a; a = b; b = dum
  dum = fa; fa = fb; fb = dum
  endif

enddo
xroot=s
end subroutine GetDerivRoot

! ********************************************************************

subroutine GetPotRoot(q,P,F,RL1,OmegaF,theta,phi,xroot)
real*8, intent(in) :: q,P,F,RL1,OmegaF,theta,phi
real*8, intent(out) :: xroot
real*8 :: a,b,c,fa,fb,fc,xcon,d,s,fs,dum
real*8 :: pi, Flim, alim, amax2, ang, ang2, thet2, ph2
logical :: mflag, con1, con2, con3, con4, con5,flag2

!near L1 point the Omega-OmegaF is tangent to zero so rootfinder doesn't work
!in this case just use R at angle alim off the x axis (set alim=1degree).
!if F.gt.Flim .and. theta and phi within angle alim of x axis, 
!then use R(theta,phi)=RL1*(1-ang/alim)+R(alim)*ang/alim with ang=angle from x-axis
!in this case need to find the root for thet2=pi/2.d0;ph2=alim to get R(alim)
pi = 3.14159265358979323846d0
Flim = 0.997d0;alim=0.1d0*pi/180.0d0
ang2 = phi*phi+(theta-pi/2.d0)*(theta-pi/2.d0)
ang=dsqrt(ang2)
!write(6,*)"amax2,ang2",amax2,ang2
if (F.gt.0.997d0 .and. ang.lt.alim) then
!xroot = RL1
!return
thet2=pi/2.d0;ph2=alim
flag2=.true.
else
thet2=theta;ph2=phi
flag2=.false.
endif

!convergence criterion in x
xcon = 1.d-9
! initial guess
a = 0.05d0*Reg(q)
b = 1.0001*RL1
fa = potential(a,thet2,ph2,q,P)-OmegaF
fb = potential(b,thet2,ph2,q,P)-OmegaF

!test that root is bracketed by a,b
if (fa*fb .ge. 0.d0) then
write(6,*) "a,b,fa,fb",a,b,fa,fb
write(6,*) "bad initial guess in potential root"
stop 
endif

!swap a,b if needed
if (dabs(fa) .lt. dabs(fb)) then
dum = a; a = b; b = dum
dum = fa; fa = fb; fb = dum
!write(6,*) "swap: a,b,fa,fb",a,b,fa,fb
end if 

c = a
fc = potential(c,thet2,ph2,q,P)-OmegaF
!mflag = 1
mflag = .true.

do while ((dabs(b-a) .gt. xcon) .and. (fb .ne. 0.d0))
  if ((fa .ne. fc) .and. (fb .ne. fc)) then
  s = a*fb*fc/((fa-fb)*(fa-fc))+b*fa*fc/((fb-fa)*(fb-fc))+c*fa*fb/((fc-fa)*(fc-fb))
  else
  s=b-fb*(b-a)/(fb-fa)
  endif
  con1 = (s.le.(3.d0*a+b)/4.d0) .or. (s.ge.b)
  con2 = mflag.and.(dabs(s-b).ge.dabs(b-c)/2.d0)
  con3 = (.not.mflag).and.(dabs(s-b).ge.dabs(c-d)/2.d0)
  con4 = mflag.and.(dabs(b-c).lt.dabs(xcon))
  con5 = (.not.mflag).and.(dabs(c-d).lt.dabs(xcon))
  if (con1.or.con2.or.con3.or.con4.or.con5) then
  s=(a+b)/2.d0
  mflag=.true.
  else
  mflag=.false.
  endif

  fs = potential(s,thet2,ph2,q,P)-OmegaF
  d=c
  c=b; fc=potential(c,thet2,ph2,q,P)-OmegaF

  if (fa*fs.lt.0.d0) then 
  b=s; fb=fs
  else 
  a=s; fa=fs
  endif 

!swap a,b if needed
  if (dabs(fa) .lt. dabs(fb)) then
  dum = a; a = b; b = dum
  dum = fa; fa = fb; fb = dum
  endif
enddo

!for case ang<alim then need to calculate R from RL1 and s
if (flag2) then
! linear interpolate between s and RL1
xroot=RL1*(1-ang/alim)+s*ang/alim
else
xroot=s
endif

end subroutine GetPotRoot

! ********************************************************************

!subroutine to calculate volume and area 
subroutine GetVolArea(q, P,F,RL1, OmegaF, vol, area)
implicit none
real*8, intent(in) :: q, P,F,RL1, OmegaF
real*8, intent(out) :: vol, area
real*8 :: pi
pi = 3.14159265358979323846d0
! for integration over cos(theta)=mu limits are 0 to 1
! call romberg to integrate volume, case 1
call romberg2(1, 0.0d0, 1.d0,q,P,F,RL1,OmegaF, vol)
! call romberg to integrate area, case 2, 
call romberg2(2, 0.0d0, 1.d0,q,P,F,RL1,OmegaF, area)
end subroutine GetVolArea

! ********************************************************************
!  THE FUNCTIONS

!Rtp give a value R(theta,phi) given a value of the potential OmegaF
real*8 function Rtp(theta,phi,q,P,F,RL1,OmegaF)
implicit none
real*8, intent(in) :: theta,phi,q,P,F,RL1,OmegaF 
real*8 :: Rroot
!write(6,*) "theta,phi",theta,phi
Call GetPotRoot(q,P,F,RL1,OmegaF,theta,phi,Rroot)
Rtp = Rroot
end function Rtp

! the dimensionless Roche Potential
real*8 function potential(r,theta,phi,q,P)
implicit none
real*8, intent(in) :: r,theta,phi,q,P
real*8 :: term1, term2, dcp, dst
dcp=dcos(phi);dst=dsin(theta)
term1 = 1.d0/r + q/dsqrt(1.d0-2.d0*r*dcp*dst+r*r)
term2 = -q*r*dcp*dst + (q+1.d0)/2.d0*P*P*r*r*dst*dst
potential = term1+term2
end function potential

! the derivative w.r.t. x of dimensionless Roche Potential for y=z=0
! valid for 0<x<1 (iroot=1); valid for x<0 (iroot=2); valid for x>1 (iroot=3);
real*8 function deriv(x,q,P,iroot)
implicit none
real*8, intent(in) :: x,q,P
integer, intent(in) :: iroot
real*8 :: term1, term2 
   if (iroot.eq.1) then
   term1 = -1.d0/(x*x) + q/((1.d0-x)*(1.d0-x))
   elseif (iroot.eq.2) then
   term1 = 1.d0/(x*x) + q/((1.d0-x)*(1.d0-x))
   elseif (iroot.eq.3) then
   term1 = -1.d0/(x*x) - q/((1.d0-x)*(1.d0-x))
endif
term2 = (q+1.d0)*P*P*x - q
deriv = term1+term2
end function deriv

!integration over phi for volume
real*8 function PV(theta,q,P,F,RL1,OmegaF) 
implicit none
real*8, intent(in) :: theta,q,P,F,RL1,OmegaF 
integer :: case
real*8 :: pvol, pi
pi = 3.14159265358979323846d0
case = 1
call Romberg1(case,0.d0,pi,theta,q,P,F,RL1,OmegaF,pvol)
PV = pvol
end function PV

!integration over phi for area
real*8 function PA(theta,q,P,F,RL1,OmegaF)
implicit none
real*8, intent(in) :: theta,q,P,F,RL1,OmegaF 
integer :: case
real*8 :: parea, pi
pi = 3.14159265358979323846d0
case = 2
call Romberg1(case,0.d0,pi,theta,q,P,F,RL1,OmegaF,parea)
PA = parea
end function PA

!integrand for phi integration, case1 is volume, case2 is area
real*8 function integrand(theta,phi,q,P,F,RL1,OmegaF,case)
implicit none
real*8, intent(in) :: theta,phi,q,P,F,RL1,OmegaF 
integer, intent(in) :: case
real*8 :: Rthephi
Rthephi=Rtp(theta,phi,q,P,F,RL1,OmegaF)
if (case .eq. 1) integrand = 2.d0*Rthephi*Rthephi*Rthephi/3.d0
if (case .eq. 2) integrand = 2.d0*Rthephi*Rthephi
end function integrand

!integrand for theta integration,  case1 is volume, case2 is area
!real*8 function integrand2(theta,q,P,F,RL1,OmegaF,case)
real*8 function integrand2(mu,q,P,F,RL1,OmegaF,case)
implicit none
!real*8, intent(in) :: theta,q,P,F,RL1,OmegaF 
real*8, intent(in) :: mu,q,P,F,RL1,OmegaF 
integer, intent(in) :: case
real*8 :: theta
theta = dacos(mu)
if (case .eq. 1) integrand2 = 2.d0*PV(theta,q,P,F,RL1,OmegaF)
if (case .eq. 2) integrand2 = 2.d0*PA(theta,q,P,F,RL1,OmegaF)
end function integrand2

! Eggleton formula
real*8 function REg(q)
implicit none
real*8, intent(in) :: q 
real*8 :: pi
pi = 3.14159265358979323846d0
REg = 0.49d0/(0.6d0+q**(2.d0/3.d0)*dlog(1+q**(-1.d0/3.d0)))
end function REg

! ********************************************************************
!
!  Romberg Integration method
!Romberg1 for integral over phi for any theta, Romberg2 for integral over theta
!
SUBROUTINE ROMBERG1(case,A,B,theta,q,P,F,RL1,OmegaF,integral) 
      implicit none
      REAL*8, intent(in) :: A,B,theta,q,P,F,RL1,OmegaF
      integer, intent(in) :: case
      REAL*8, intent(out) :: integral
      REAL*8 :: H, SUM
      INTEGER :: I, J, K, L, M, N
      REAL*8, allocatable ::  R(:,:)
      N=7
      allocate(R(N,N))  
      H = B - A   
      R(1,1)=0.5*H*(integrand(theta,A,q,P,F,RL1,OmegaF,case)+integrand(theta,B,q,P,F,RL1,OmegaF,case))   
      L = 1       
      DO 4 I = 2,N
        H = 0.5*H 
        L = L + L 
        SUM = 0.0 
        DO 2 K = 1,L-1,2    
          SUM = SUM + integrand(theta,A + H*REAL(K),q,P,F,RL1,OmegaF,case)
   2    CONTINUE  
        M = 1     
        R(I,1) = 0.5*R(I-1,1) + H*SUM 
        DO 3 J = 2,I
          M = 4*M 
          R(I,J) = R(I,J-1) + (R(I,J-1) - R(I-1,J-1))/REAL(M - 1)   
   3    CONTINUE      
   4  CONTINUE    
   5  FORMAT(5X,5E22.14)  
      integral = R(N,N) 
      deallocate(R)
      RETURN      
end subroutine romberg1

! **********************************************************************

SUBROUTINE ROMBERG2(case,A,B,q,P,F,RL1,OmegaF,integral) 
      implicit none
      REAL*8, intent(in) :: A,B,q,P,F,RL1,OmegaF
      integer, intent(in) :: case
      REAL*8, intent(out) :: integral
      REAL*8 :: H, SUM
      INTEGER :: I, J, K, L, M, N
      REAL*8, allocatable ::  R(:,:)
      N=7
      allocate(R(N,N))  
      H = B - A   
      R(1,1) = 0.5*H*(integrand2(A,q,P,F,RL1,OmegaF,case) + integrand2(B,q,P,F,RL1,OmegaF,case)) 
      L = 1       
      DO 4 I = 2,N
        H = 0.5*H 
        L = L + L 
        SUM = 0.0 
        DO 2 K = 1,L-1,2    
          SUM = SUM + integrand2(A + H*REAL(K),q,P,F,RL1,OmegaF,case)
   2    CONTINUE  
        M = 1     
        R(I,1) = 0.5*R(I-1,1) + H*SUM 
        DO 3 J = 2,I
          M = 4*M 
          R(I,J) = R(I,J-1) + (R(I,J-1) - R(I-1,J-1))/REAL(M - 1)   
   3    CONTINUE      
   4  CONTINUE    
   5  FORMAT(5X,5E22.14)  
      integral = R(N,N) 
      deallocate(R)
      RETURN      
end subroutine romberg2
! ********************************************************************
end program RocheLobe
