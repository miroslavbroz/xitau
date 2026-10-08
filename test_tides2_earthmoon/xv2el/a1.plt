#!/usr/bin/env gnuplot

cm = 1.e-2  # m
km = 1.e3  # m
au = 1.49597870700e11  # m, from IAU 2012
day = 86400.  # s
yr = 365.25*day
G = 6.67430e-11  # kg^-1 m^3 s^-2, CODATA 2018; https://physics.nist.gov/cgi-bin/cuu/Value?bg
GM_S = 0.2959122082855911e-03  # AU^3/day^2, from JPLEPH DE405
M_S = GM_S*au**3/day**2/G

msum = 3.e-6*M_S
P1 = 27.33*day

# a^3 P^-2 = G(m1+m2)/(4 pi^2)
# 3a^2 da P^-2 - 2a^3 P^-3 dP = 0
# dP/P = 3/2 da/a

a(P) = (G*msum/(4.*pi**2)*P**2)**(1./3.)

dadt = 3.84*cm/yr

a1 = 2.5609507198313310e-003*au

print "a1 = ", a1/km, " km"
print "a1 = ", a(P1)/km, " km"
print "dadt = ", dadt, " m s^-1 = ", dadt / (cm/yr), " cm y^-1"

set xl "t [d]"
set yl "a - a0 [cm]"

set zeroaxis
set key left

p "xv2el.out" u 1:((a($3*day)-a1)/cm) w lp lc 'blue',\
  dadt*x*day/cm w l lw 2 lc 'orange'

pa -1

set term png small
set out "a1.png"
rep

q
 

