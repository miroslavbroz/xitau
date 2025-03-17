! root.f90

module root_module

contains

! Root of a function f(x).

double precision function root(func, a_, b_)

implicit none
double precision :: func
double precision, intent(in) :: a_, b_

double precision, parameter :: eps = 1.0d-9
double precision :: a, b
double precision :: c, s, fa, fb, fc, fs, d, dum
logical :: mflag, con1, con2, con3, con4, con5

a = a_
b = b_

fa = func(a)
fb = func(b)

! test that root is bracketed by a,b
if (fa*fb.ge.0.d0) then
  write(6,*) "root: Error bad initial guess!"
  write(6,*) "a = ", a
  write(6,*) "b = ", b
  write(6,*) "fa = ", fa
  write(6,*) "fb = ", fb
  stop 
endif

! swap a,b if needed
if (abs(fa).lt.abs(fb)) then
  dum = a; a = b; b = dum
  dum = fa; fa = fb; fb = dum
end if 

c = a
fc = func(c)
mflag = .true.

do while ((abs(b-a).gt.eps).and.(fb.ne.0.d0))

  if ((fa.ne.fc).and.(fb.ne.fc)) then
    s = a*fb*fc/((fa-fb)*(fa-fc)) + b*fa*fc/((fb-fa)*(fb-fc)) + c*fa*fb/((fc-fa)*(fc-fb))
  else
    s = b - fb*(b-a)/(fb-fa)
  endif
  con1 = (s.le.(3.d0*a+b)/4.d0).or.(s.ge.b)
  con2 = mflag.and.(abs(s-b).ge.abs(b-c)/2.d0)
  con3 = (.not.mflag).and.(abs(s-b).ge.abs(c-d)/2.d0)
  con4 = mflag.and.(abs(b-c).lt.abs(eps))
  con5 = (.not.mflag).and.(abs(c-d).lt.abs(eps))
  if (con1.or.con2.or.con3.or.con4.or.con5) then
    s = (a+b)/2.d0
    mflag = .true.
  else
    mflag = .false.
  endif

  fs = func(s)
  d = c
  c = b
  fc = func(c)

  if (fa*fs.lt.0.d0) then 
    b=s; fb=fs
  else 
    a=s; fa=fs
  endif 

  ! swap a,b if needed
  if (abs(fa).lt.abs(fb)) then
    dum = a; a = b; b = dum
    dum = fa; fa = fb; fb = dum
  endif

enddo

root = s
return

end function root

end module root_module


