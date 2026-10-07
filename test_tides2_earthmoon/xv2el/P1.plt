#!/usr/bin/env gnuplot

cm = 1.e-2  # m
km = 1.e3  # m
day = 86400.  # s
yr = 365.25*day
a1 = 384000.*km
P1 = 27.33*day

# a^3 P^-2 = G(m1+m2)/(4 pi^2)
# 3a^2 da P^-2 - 2a^3 P^-3 dP = 0
# dP/P = 3/2 da/a

dadt = 3.84*cm/yr
dpdt = 3./2.*dadt/a1*P1
#dpdt = 1.5e-5*yr**-1  # chatgpt; what is the period rate of the Moon?

print dadt/a1
print dpdt/P1
print "dadt = ", dadt, " m s^-1 = ", dadt / (cm/yr), " cm y^-1"
print "dpdt = ", dpdt, " s s^-1 = ", dpdt / (1./yr), " s y^-1"

set xl "t [d]"
set yl "P1 - 27.33 [d]"

set zeroaxis
set key left

p "xv2el.out" u 1:($3-27.33) w lp lc 'blue',\
  dpdt*x*day/day w l lc 'orange'

pa -1

set term png small
set out "P1.png"
rep

q
 

