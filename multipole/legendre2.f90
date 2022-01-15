! legendre2.f90
! Legendre polynomials (optimized version).
! Miroslav Broz (miroslav.broz@email.cz), Aug 21st 2020

! Reference: Bursa, Karsky, Kostelecky: Dynamika umelych druzic
! v tihovem poli Zeme. Praha: Academia, 1993.

module legendre2_module

double precision, save :: x2, x3, x4, x5, x6, x7, x8, x9, x10
double precision, save :: y, y2, y3, y4, y5, y1_2, y3_2, y5_2, y7_2, y9_2

contains

subroutine Plm_init(x)

implicit none
double precision :: x

x2 = x*x
x3 = x2*x
x4 = x3*x
x5 = x4*x
x6 = x5*x
x7 = x6*x
x8 = x7*x
x9 = x8*x
x10 = x9*x

y = (1.d0-x2)
y2 = y*y
y3 = y2*y
y4 = y3*y
y5 = y4*y
y1_2 = sqrt(y)
y3_2 = y1_2*y
y5_2 = y3_2*y
y7_2 = y5_2*y
y9_2 = y7_2*y

end subroutine Plm_init

double precision function Pl(l, x)

integer :: l
double precision :: x

if (l.eq.0) then
  Pl = 1.d0
else if (l.eq.1) then
  Pl = x
else if (l.eq.2) then
  Pl = 1.d0/2.d0*(3.d0*x2 - 1.d0)
else if (l.eq.3) then
  Pl = 1.d0/2.d0*(5.d0*x3 - 3.d0*x)
else if (l.eq.4) then
  Pl = 1.d0/8.d0*(35.d0*x4 - 30.d0*x2 + 3.d0)
else if (l.eq.5) then
  Pl = 1.d0/8.d0*(63.d0*x5 - 70.d0*x3 + 15.d0*x)
else if (l.eq.6) then
  Pl = 1.d0/16.d0*(231.d0*x6 - 315.d0*x4 + 105.d0*x2 - 5.d0)
else if (l.eq.7) then
  Pl = 1.d0/16.d0*(429.d0*x7 - 693.d0*x5 + 315.d0*x3 - 35.d0*x)
else if (l.eq.8) then
  Pl = 1.d0/128.d0*(6435.d0*x8 - 12012.d0*x6 + 6930.d0*x4 - 1260.d0*x2 + 35.d0)
else if (l.eq.9) then
  Pl = 1.d0/128.d0*(12155.d0*x9 - 25740.d0*x7 + 18018.d0*x5 - 4620.d0*x3 + 315.d0*x)
else if (l.eq.10) then
  Pl = 1.d0/256.d0*(46189.d0*x10 - 109395.d0*x8 + 90090.d0*x6 - 30030.d0*x4 + 3465.d0*x2 - 63.d0)
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
    Plm = y1_2
  endif

else if (l.eq.2) then
  if (m.eq.1) then
    Plm = 3.d0*x*y1_2
  else if (m.eq.2) then
    Plm = 3.d0*y
  endif

else if (l.eq.3) then
  if (m.eq.1) then
    Plm = 1.d0/2.d0*y1_2 * (15.d0*x2 - 3.d0)
  else if (m.eq.2) then
    Plm = 15.d0*x*y
  else if (m.eq.3) then
    Plm = 15.d0*y3_2
  endif

else if (l.eq.4) then
  if (m.eq.1) then
    Plm = 1.d0/2.d0*y1_2 * (35.d0*x3 - 15.d0*x)
  else if (m.eq.2) then
    Plm = 1.d0/2.d0*y * (105.d0*x2 - 15.d0)
  else if (m.eq.3) then
    Plm = 105.d0*x*y3_2
  else if (m.eq.4) then
    Plm = 105.d0*y2
  endif

else if (l.eq.5) then
  if (m.eq.1) then
    Plm = 15.d0/8.d0*y1_2 * (21.d0*x4 - 14.d0*x2 + 1.d0)
  else if (m.eq.2) then
    Plm = 15.d0/2.d0*y * (21.d0*x3 - 7.d0*x)
  else if (m.eq.3) then
    Plm = 105.d0/2.d0*y3_2 * (9.d0*x2 - 1.d0)
  else if (m.eq.4) then
    Plm = 945.d0*x*y2
  else if (m.eq.5) then
    Plm = 945.d0*y5_2
  endif

else if (l.eq.6) then
  if (m.eq.1) then
    Plm = 21.d0/8.d0*y1_2 * (33.d0*x5 - 30.d0*x3 + 5.d0*x)
  else if (m.eq.2) then
    Plm = 15.d0/8.d0*y * (231.d0*x4 - 126.d0*x2 + 7.d0)
  else if (m.eq.3) then
    Plm = 315.d0/2.d0*y3_2 * (11.d0*x3 - 3.d0*x)
  else if (m.eq.4) then
    Plm = 945.d0/2.d0*y2 * (11.d0*x2 - 1.d0)
  else if (m.eq.5) then
    Plm = 10395.d0*x*y5_2
  else if (m.eq.6) then
    Plm = 10395.d0*y3
  endif

else if (l.eq.7) then
  if (m.eq.1) then
    Plm = 7.d0/16.d0*y1_2 * (429.d0*x6 - 495.d0*x4 + 135.d0*x3 - 5.d0)
  else if (m.eq.2) then
    Plm = 63.d0/8.d0*y * (143.d0*x5 - 110.d0*x3 + 15.d0*x)
  else if (m.eq.3) then
    Plm = 315.d0/8.d0*y3_2 * (143.d0*x4 - 66.d0*x2 + 3.d0)
  else if (m.eq.4) then
    Plm = 3465.d0/2.d0*y2 * (13.d0*x3 - 3.d0*x)
  else if (m.eq.5) then
    Plm = 10395.d0/2.d0*y5_2 * (13.d0*x2 - 1.d0)
  else if (m.eq.6) then
    Plm = 135135.d0*x*y3
  else if (m.eq.7) then
    Plm = 135135.d0*y7_2
  endif

else if (l.eq.8) then
  if (m.eq.1) then
    Plm = 9.d0/16.d0*y1_2 * (715.d0*x7 - 1001.d0*x5 + 385.d0*x3 - 35.d0*x)
  else if (m.eq.2) then
    Plm = 315.d0/16.d0*y * (143.d0*x6 - 143.d0*x4 + 33.d0*x2 - 1.d0)
  else if (m.eq.3) then
    Plm = 3465.d0/8.d0*y3_2 * (39.d0*x5 - 26.d0*x3 + 3.d0*x)
  else if (m.eq.4) then
    Plm = 10395.d0/8.d0*y2 * (65.d0*x4 - 26.d0*x2 + 1.d0)
  else if (m.eq.5) then
    Plm = 135135.d0/2.d0*y5_2 * (5.d0*x3 - x)
  else if (m.eq.6) then
    Plm = 135135.d0/2.d0*y3 * (15.d0*x2 - 1.d0)
  else if (m.eq.7) then
    Plm = 2027025.d0*x*y7_2
  else if (m.eq.8) then
    Plm = 2027025.d0*y4
  endif

else if (l.eq.9) then
  if (m.eq.1) then
    Plm = 45.d0/128.d0*y1_2 * (2431.d0*x8 - 4004.d0*x6 + 2002.d0*x4 - 308.d0*x2 + 7.d0)
  else if (m.eq.2) then
    Plm = 495.d0/16.d0*y * (221.d0*x7 - 237.d0*x5 + 91.d0*x3 - 7.d0*x)
  else if (m.eq.3) then
    Plm = 3465.d0/16.d0*y3_2 * (221.d0*x6 - 195.d0*x4 - 39.d0*x2 - 1.d0)
  else if (m.eq.4) then
    Plm = 135135.d0/8.d0*y2 * (17.d0*x5 - 10.d0*x3 + x)
  else if (m.eq.5) then
    Plm = 135135.d0/8.d0*y5_2 * (85.d0*x4 - 30.d0*x2 + 1.d0)
  else if (m.eq.6) then
    Plm = 675675.d0/2.d0*y3 * (17.d0*x3 - 3.d0*x)
  else if (m.eq.7) then
    Plm = 2027025.d0/2.d0*y7_2 * (17.d0*x2 - 1.d0)
  else if (m.eq.8) then
    Plm = 34459425.d0*x*y4
  else if (m.eq.9) then
    Plm = 34459425.d0*y9_2
  endif

else if (l.eq.10) then
  if (m.eq.1) then
    Plm = 5.d0/128.d0*y1_2 * (46189.d0*x9 - 87516.d0*x7 + 54054.d0*x5 - 12012.d0*x3 + 639.d0*x)
  else if (m.eq.2) then
    Plm = 495.d0/128.d0*y * (4199.d0*x8 - 6188.d0*x6 + 2730.d0*x4 - 364.d0*x2 + 7.d0)
  else if (m.eq.3) then
    Plm = 6435.d0/16.d0*y3_2 * (323.d0*x7 - 357.d0*x5 + 105.d0*x3 - 7.d0*x)
  else if (m.eq.4) then
    Plm = 45045.d0/16.d0*y2 * (323.d0*x6 - 255.d0*x4 + 45.d0*x2 - 1.d0)
  else if (m.eq.5) then
    Plm = 135135.d0/8.d0*y5_2 * (323.d0*x5 - 170.d0*x3 + 15.d0*x)
  else if (m.eq.6) then
    Plm = 675675.d0/8.d0*y3 * (323.d0*x4 - 102.d0*x2 + 3.d0)
  else if (m.eq.7) then
    Plm = 11486475.d0/2.d0*y7_2 * (19.d0*x3 - 3.d0*x)
  else if (m.eq.8) then
    Plm = 34459425.d0/2.d0*y4 * (19.d0*x2 - 1.d0)
  else if (m.eq.9) then
    Plm = 654729075.d0*x*y9_2
  else if (m.eq.10) then
    Plm = 654729075.d0*y5
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

dPlm_dx = ((l+m)*Plm(l-1,m,x) - l*x*tmp) / y

return
end function dPlm_dx


end module legendre2_module


