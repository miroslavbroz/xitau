#!/usr/bin/gnuplot

# omega1_PPN.plt
# Test pericentre precession due to general relativity (Fabrycky 2010).
# Miroslav Broz (miroslav.broz@email.cz), Jun 7th 2016

set term x11

AU = 1.49597870691e11  # m
M_S = 2.959139768995959e-04  # AU^3/day^2
M_J = 1.e-3*M_S
R_S = 6.995e8/AU
R_J = 7.1492e7/AU
G = 1.
day = 1.
yr = 365.25*day
Myr = 1.e6*yr
c = 299792458.  # m/s
c_AU_per_day = c/AU*84600.

rad = 180./pi

k_L = 0.34
m_star = 1.27*M_S
m_p = 0.672*M_J
e = 0.
R_p = 1.3*R_J
P = 4.617136*day

# a^3/P^2 = G(m1+m2)/(4 pi^2)

a = (G*(m_star+m_p)/(4.*pi**2) * P**2)**(1./3.)
n = 2.*pi/P

domega_dt_GR = 3.*(m_star+m_p)**(3./2.)/(a**(5./2.)*c_AU_per_day**2*(1.-e**2))
P_GR = 2.*pi/(domega_dt_GR)

print "m_star = ", m_star, " AU^3/day^2 = ", m_star/M_S, " M_S"
print "m_p = ", m_p, " AU^3/day^2 = ", m_p/M_S, " M_S"
print "a = ", a, " AU"
print "n = ", n, " rad/day"
print "domega/dt_GR = ", domega_dt_GR, " rad/day"
print "P_GR = ", P_GR/day, " day = ", P_GR/yr, " yr = ", P_GR/Myr, " Myr"

########################################################################

f(x) = x > 180. ? x - 360. : x

set xl "time [yr]"
set yl "omega_1 [deg]"

set ytics format "%.1e"
set key left
set zeroaxis

p \
  "xvpl2el.out" u ($1/yr):(f($7)) t "PPN" w lp lt 3,\
  "../../test_tides/xvpl2el/xvpl2el.out" u ($1/yr):(f($7)) t "tides"      w l lt 1,\
  "../../test_oblat/xvpl2el/xvpl2el.out" u ($1/yr):(f($7)) t "oblateness" w l lt 2,\
  0.0 + domega_dt_GR*(x*yr)*rad w l t "domega/dt_GR" lt 7 lw 3
pa -1

set term png small
set out "omega1_PPN.png"
rep

q


