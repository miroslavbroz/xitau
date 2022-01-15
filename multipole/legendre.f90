! legendre.f90
! Legendre polynomials.
! Miroslav Broz (miroslav.broz@email.cz), Nov 14th 2019

! Reference: Bursa, Karsky, Kostelecky: Dynamika umelych druzic
! v tihovem poli Zeme. Praha: Academia, 1993.

module legendre_module

contains

double precision function Pl(l, x)

integer :: l
double precision :: x

if (l.eq.0) then
  Pl = 1.d0
else if (l.eq.1) then
  Pl = x
else if (l.eq.2) then
  Pl = 1.d0/2.d0*(3.d0*x**2 - 1.d0)
else if (l.eq.3) then
  Pl = 1.d0/2.d0*(5.d0*x**3 - 3.d0*x)
else if (l.eq.4) then
  Pl = 1.d0/8.d0*(35.d0*x**4 - 30.d0*x**2 + 3.d0)
else if (l.eq.5) then
  Pl = 1.d0/8.d0*(63.d0*x**5 - 70.d0*x**3 + 15.d0*x)
else if (l.eq.6) then
  Pl = 1.d0/16.d0*(231.d0*x**6 - 315.d0*x**4 + 105.d0*x**2 - 5.d0)
else if (l.eq.7) then
  Pl = 1.d0/16.d0*(429.d0*x**7 - 693.d0*x**5 + 315.d0*x**3 - 35.d0*x)
else if (l.eq.8) then
  Pl = 1.d0/128.d0*(6435.d0*x**8 - 12012.d0*x**6 + 6930.d0*x**4 - 1260.d0*x**2 + 35.d0)
else if (l.eq.9) then
  Pl = 1.d0/128.d0*(12155.d0*x**9 - 25740.d0*x**7 + 18018.d0*x**5 - 4620.d0*x**3 + 315.d0*x)
else if (l.eq.10) then
  Pl = 1.d0/256.d0*(46189.d0*x**10 - 109395.d0*x**8 + 90090.d0*x**6 - 30030.d0*x**4 + 3465.d0*x**2 - 63.d0)
else
  write(*,*) '# Error in legendre.f: l = ', l
  stop
endif

return

end function Pl

double precision function Plm(l, m, x)

implicit none

integer :: l, m
double precision x

if (m.eq.0) then
  Plm = Pl(l, x)
  return
endif

! see recursion in dPlm_dx()
if (m.gt.l) then
  Plm = 0.d0
  return
endif

if ((l.lt.1).or.(l.gt.10).or.(m.lt.1).or.(m.gt.l)) then
  write(*,*) 'Error in legendre.f: l = ', l, ', m = ', m
  stop
endif

Plm = 0.d0

if (l.eq.1) then
  if (m.eq.1) then
    Plm = (1.d0-x**2)**(1.d0/2.d0)
  endif

else if (l.eq.2) then
  if (m.eq.1) then
    Plm = 3.d0*x*(1.d0-x**2)**(1.d0/2.d0)
  else if (m.eq.2) then
    Plm = 3.d0*(1.d0-x**2)
  endif

else if (l.eq.3) then
  if (m.eq.1) then
    Plm = 1.d0/2.d0*(1.d0-x**2)**(1.d0/2.d0) * (15.d0*x**2 - 3.d0)
  else if (m.eq.2) then
    Plm = 15.d0*x*(1.d0-x**2)
  else if (m.eq.3) then
    Plm = 15.d0*(1.d0-x**2)**(3.d0/2.d0)
  endif

else if (l.eq.4) then
  if (m.eq.1) then
    Plm = 1.d0/2.d0*(1.d0-x**2)**(1.d0/2.d0) * (35.d0*x**3 - 15.d0*x)
  else if (m.eq.2) then
    Plm = 1.d0/2.d0*(1.d0-x**2) * (105.d0*x**2 - 15.d0)
  else if (m.eq.3) then
    Plm = 105.d0*x*(1.d0-x**2)**(3.d0/2.d0)
  else if (m.eq.4) then
    Plm = 105.d0*(1.d0-x**2)**2
  endif

else if (l.eq.5) then
  if (m.eq.1) then
    Plm = 15.d0/8.d0*(1.d0-x**2)**(1.d0/2.d0) * (21.d0*x**4 - 14.d0*x**2 + 1.d0)
  else if (m.eq.2) then
    Plm = 15.d0/2.d0*(1.d0-x**2) * (21.d0*x**3 - 7.d0*x)
  else if (m.eq.3) then
    Plm = 105.d0/2.d0*(1.d0-x**2)**(3.d0/2.d0) * (9.d0*x**2 - 1.d0)
  else if (m.eq.4) then
    Plm = 945.d0*x*(1.d0-x**2)**2
  else if (m.eq.5) then
    Plm = 945.d0*(1.d0-x**2)**(5.d0/2.d0)
  endif

else if (l.eq.6) then
  if (m.eq.1) then
    Plm = 21.d0/8.d0*(1.d0-x**2)**(1.d0/2.d0) * (33.d0*x**5 - 30.d0*x**3 + 5.d0*x)
  else if (m.eq.2) then
    Plm = 15.d0/8.d0*(1.d0-x**2) * (231.d0*x**4 - 126.d0*x**2 + 7.d0)
  else if (m.eq.3) then
    Plm = 315.d0/2.d0*(1.d0-x**2)**(3.d0/2.d0) * (11.d0*x**3 - 3.d0*x)
  else if (m.eq.4) then
    Plm = 945.d0/2.d0*(1.d0-x**2)**2 * (11.d0*x**2 - 1.d0)
  else if (m.eq.5) then
    Plm = 10395.d0*x*(1.d0-x**2)**(5.d0/2.d0)
  else if (m.eq.6) then
    Plm = 10395.d0*(1.d0-x**2)**3
  endif

else if (l.eq.7) then
  if (m.eq.1) then
    Plm = 7.d0/16.d0*(1.d0-x**2)**(1.d0/2.d0) * (429.d0*x**6 - 495.d0*x**4 + 135.d0*x**3 - 5.d0)
  else if (m.eq.2) then
    Plm = 63.d0/8.d0*(1.d0-x**2) * (143.d0*x**5 - 110.d0*x**3 + 15.d0*x)
  else if (m.eq.3) then
    Plm = 315.d0/8.d0*(1.d0-x**2)**(3.d0/2.d0) * (143.d0*x**4 - 66.d0*x**2 + 3.d0)
  else if (m.eq.4) then
    Plm = 3465.d0/2.d0*(1.d0-x**2)**2 * (13.d0*x**3 - 3.d0*x)
  else if (m.eq.5) then
    Plm = 10395.d0/2.d0*(1.d0-x**2)**(5.d0/2.d0) * (13.d0*x**2 - 1.d0)
  else if (m.eq.6) then
    Plm = 135135.d0*x*(1.d0-x**2)**3
  else if (m.eq.7) then
    Plm = 135135.d0*(1.d0-x**2)**(7.d0/2.d0)
  endif

else if (l.eq.8) then
  if (m.eq.1) then
    Plm = 9.d0/16.d0*(1.d0-x**2)**(1.d0/2.d0) * (715.d0*x**7 - 1001.d0*x**5 + 385.d0*x**3 - 35.d0*x)
  else if (m.eq.2) then
    Plm = 315.d0/16.d0*(1.d0-x**2) * (143.d0*x**6 - 143.d0*x**4 + 33.d0*x**2 - 1.d0)
  else if (m.eq.3) then
    Plm = 3465.d0/8.d0*(1.d0-x**2)**(3.d0/2.d0) * (39.d0*x**5 - 26.d0*x**3 + 3.d0*x)
  else if (m.eq.4) then
    Plm = 10395.d0/8.d0*(1.d0-x**2)**2 * (65.d0*x**4 - 26.d0*x**2 + 1.d0)
  else if (m.eq.5) then
    Plm = 135135.d0/2.d0*(1.d0-x**2)**(5.d0/2.d0) * (5.d0*x**3 - x)
  else if (m.eq.6) then
    Plm = 135135.d0/2.d0*(1.d0-x**2)**3 * (15.d0*x**2 - 1.d0)
  else if (m.eq.7) then
    Plm = 2027025.d0*x*(1.d0-x**2)**(7.d0/2.d0)
  else if (m.eq.8) then
    Plm = 2027025.d0*(1.d0-x**2)**4
  endif

else if (l.eq.9) then
  if (m.eq.1) then
    Plm = 45.d0/128.d0*(1.d0-x**2)**(1.d0/2.d0) * (2431.d0*x**8 - 4004.d0*x**6 + 2002.d0*x**4 - 308.d0*x**2 + 7.d0)
  else if (m.eq.2) then
    Plm = 495.d0/16.d0*(1.d0-x**2) * (221.d0*x**7 - 237.d0*x**5 + 91.d0*x**3 - 7.d0*x)
  else if (m.eq.3) then
    Plm = 3465.d0/16.d0*(1.d0-x**2)**(3.d0/2.d0) * (221.d0*x**6 - 195.d0*x**4 - 39.d0*x**2 - 1.d0)
  else if (m.eq.4) then
    Plm = 135135.d0/8.d0*(1.d0-x**2)**2 * (17.d0*x**5 - 10.d0*x**3 + x)
  else if (m.eq.5) then
    Plm = 135135.d0/8.d0*(1.d0-x**2)**(5.d0/2.d0) * (85.d0*x**4 - 30.d0*x**2 + 1.d0)
  else if (m.eq.6) then
    Plm = 675675.d0/2.d0*(1.d0-x**2)**3 * (17.d0*x**3 - 3.d0*x)
  else if (m.eq.7) then
    Plm = 2027025.d0/2.d0*(1.d0-x**2)**(7.d0/2.d0) * (17.d0*x**2 - 1.d0)
  else if (m.eq.8) then
    Plm = 34459425.d0*x*(1.d0-x**2)**4
  else if (m.eq.9) then
    Plm = 34459425.d0*(1.d0-x**2)**(9.d0/2.d0)
  endif

else if (l.eq.10) then
  if (m.eq.1) then
    Plm = 5.d0/128.d0*(1.d0-x**2)**(1.d0/2.d0) * (46189.d0*x**9 - 87516.d0*x**7 + 54054.d0*x**5 - 12012.d0*x**3 + 639.d0*x)
  else if (m.eq.2) then
    Plm = 495.d0/128.d0*(1.d0-x**2) * (4199.d0*x**8 - 6188.d0*x**6 + 2730.d0*x**4 - 364.d0*x**2 + 7.d0)
  else if (m.eq.3) then
    Plm = 6435.d0/16.d0*(1.d0-x**2)**(3.d0/2.d0) * (323.d0*x**7 - 357.d0*x**5 + 105.d0*x**3 - 7.d0*x)
  else if (m.eq.4) then
    Plm = 45045.d0/16.d0*(1.d0-x**2)**2 * (323.d0*x**6 - 255.d0*x**4 + 45.d0*x**2 - 1.d0)
  else if (m.eq.5) then
    Plm = 135135.d0/8.d0*(1.d0-x**2)**(5.d0/2.d0) * (323.d0*x**5 - 170.d0*x**3 + 15.d0*x)
  else if (m.eq.6) then
    Plm = 675675.d0/8.d0*(1.d0-x**2)**3 * (323.d0*x**4 - 102.d0*x**2 + 3.d0)
  else if (m.eq.7) then
    Plm = 11486475.d0/2.d0*(1.d0-x**2)**(7.d0/2.d0) * (19.d0*x**3 - 3.d0*x)
  else if (m.eq.8) then
    Plm = 34459425.d0/2.d0*(1.d0-x**2)**4 * (19.d0*x**2 - 1.d0)
  else if (m.eq.9) then
    Plm = 654729075.d0*x*(1.d0-x**2)**(9.d0/2.d0)
  else if (m.eq.10) then
    Plm = 654729075.d0*(1.d0-x**2)**5
  endif

endif

return

end function Plm


double precision function dPlm_dx(l, m, x, Plm_)

implicit none

integer :: l, m
double precision :: x
double precision, optional, intent(in) :: Plm_  ! precomputed P(l,m)
double precision :: tmp

if (l.eq.0) then
  dPlm_dx = 0.d0
  return
endif

if (x.eq.1.d0) then
  if (m.eq.0) then
    dPlm_dx = l*(l+1)/2.d0
  else if (m.eq.1) then
    dPlm_dx = 1.d38
  else if (m.eq.2) then
    dPlm_dx = -(l-1)*l*(l+1)*(l+2)/4.d0
  else
    dPlm_dx = 0.d0
  endif
  return
endif

if (x.eq.-1.d0) then
  if (m.eq.0) then
    dPlm_dx = (-1)**(l+1) * l*(l+1)/2.d0
  else if (m.eq.1) then
    dPlm_dx = (-1)**l * 1.d38
  else if (m.eq.2) then
    dPlm_dx = (-1)**l * (l-1)*l*(l+1)*(l+2)/4.d0
  else
    dPlm_dx = 0.d0
  endif
  return
endif

if (present(Plm_)) then
  tmp = Plm_
else
  tmp = Plm(l,m,x)
endif

dPlm_dx = ((l+m)*Plm(l-1,m,x) - l*x*tmp) / (1.d0-x**2)

end function dPlm_dx


end module legendre_module


